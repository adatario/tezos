(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Prevalidator_internal_common
open Prevalidator_worker_state
open Shell_operation
module Events = Prevalidator_events
module Classification = Prevalidator_classification

(** This module encapsulates pending operations to maintain them in two
    different data structure and avoid coslty repetitive convertions when
    handling batches in [classify_pending_operations]. *)
module Pending_ops = Prevalidator_pending_operations

(** Module encapsulating some types that are used both in production
    and in tests. Having them in a module makes it possible to
    [include] this module in {!Internal_for_tests} below and avoid
    code duplication.

    The raison d'etre of these records of functions is to be able to use
    alternative implementations of all functions in tests.

    The purpose of the {!Tools.tools} record is to abstract away from {!Store.chain_store}.
    Under the hood [Store.chain_store] requires an Irmin store on disk,
    which makes it impractical for fast testing: every test would need
    to create a temporary folder on disk which doesn't scale well.

    The purpose of the {!Tools.worker_tools} record is to abstract away
    from the {!Worker} implementation. This implementation is overkill
    for testing: we don't need asynchronicity and concurrency in our
    pretty basic existing tests. Having this abstraction allows to get
    away with a much simpler state machine model of execution and
    to have simpler test setup. *)
module Tools = struct
  (** Functions provided by {!Distributed_db} and {!Store.chain_store}
      that are used in various places of the mempool. Gathered here so that we can test
      the mempool without requiring a full-fledged [Distributed_db]/[Store.Chain_store]. *)
  type 'prevalidation_t tools = {
    advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
        (** [advertise_current_head mempool head] sends a
            [Current_head (chain_id, head_header, mempool)] message to all known
            active peers for the chain being considered. *)
    chain_tools : Store.Block.t Classification.chain_tools;
        (** Lower-level tools provided by {!Prevalidator_classification} *)
    flush :
      head:Store.Block.t ->
      timestamp:Time.Protocol.t ->
      'prevalidation_t ->
      'prevalidation_t tzresult Lwt.t;
        (** Create a new empty prevalidation state, recycling some elements
            of the provided previous prevalidation state. *)
    fetch :
      ?peer:P2p_peer.Id.t ->
      ?timeout:Time.System.Span.t ->
      Operation_hash.t ->
      Operation.t tzresult Lwt.t;
        (** [fetch ?peer ?timeout oph] returns the value when it is known.
            It can fail with [Requester.Timeout] if [timeout] is provided and the value
            isn't known before the timeout expires. It can fail with [Requester.Cancel] if
            the request is canceled. *)
    read_block : Block_hash.t -> Store.Block.t tzresult Lwt.t;
        (** [read_block bh] tries to read the block [bh] from the chain store. *)
    send_get_current_head : ?peer:P2p_peer_id.t -> unit -> unit;
        (** [send_get_current_head ?peer ()] sends a [Get_Current_head]
            to a given peer, or to all known active peers for the chain considered.
            Expected answer is a [Get_current_head] message *)
    set_mempool : head:Block_hash.t -> Mempool.t -> unit tzresult Lwt.t;
        (** [set_mempool ~head mempool] sets the [mempool] of
            the [chain_store] of the chain considered. Does nothing if [head] differs
            from current_head which might happen when a new head concurrently arrives just
            before this operation is being called. *)
  }

  (** Abstraction over services implemented in production by {!Worker}
      but implemented differently in tests.

      Also see the enclosing module documentation as to why we have this record. *)
  type worker_tools = {
    push_request :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> bool Lwt.t;
        (** Adds a message to the queue. *)
    push_request_now :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> unit;
        (** Adds a message to the queue immediately. *)
  }
end

type 'a parameters = {
  limits : Shell_limits.prevalidator_limits;
  tools : 'a Tools.tools;
}

(** The type needed for the implementation of [Make] below, but
 *  which is independent from the protocol. *)
type ('protocol_data, 'a) types_state_shell = {
  classification : 'protocol_data Classification.t;
  parameters : 'a parameters;
  mutable predecessor : Store.Block.t;
  mutable timestamp : Time.System.t;
  mutable live_blocks : Block_hash.Set.t;
  mutable live_operations : Operation_hash.Set.t;
  mutable fetching : Operation_hash.Set.t;
  mutable pending : 'protocol_data Pending_ops.t;
  mutable mempool : Mempool.t;
  mutable advertisement : [`Pending of Mempool.t | `None];
  mutable banned_operations : Operation_hash.Set.t;
  worker : Tools.worker_tools;
}

let metrics = Shell_metrics.Mempool.init ["mempool"]

(** The concrete production instance of {!block_tools} *)
let block_tools : Store.Block.t Classification.block_tools =
  {
    bhash = Store.Block.hash;
    operations = Store.Block.operations;
    all_operation_hashes = Store.Block.all_operation_hashes;
  }

(** How to create an instance of {!chain_tools} from a {!Distributed_db.chain_db}. *)
let mk_chain_tools (chain_db : Distributed_db.chain_db) :
    Store.Block.t Classification.chain_tools =
  let open Lwt_syntax in
  let new_blocks ~from_block ~to_block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Chain_traversal.new_blocks chain_store ~from_block ~to_block
  in
  let read_predecessor_opt block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Block.read_predecessor_opt chain_store block
  in
  let inject_operation oph op =
    let* _ = Distributed_db.inject_operation chain_db oph op in
    Lwt.return_unit
  in
  {
    clear_or_cancel = Distributed_db.Operation.clear_or_cancel chain_db;
    inject_operation;
    new_blocks;
    read_predecessor_opt;
  }

(** Module type used both in production and in tests. *)
module type S = sig
  (** Type instantiated by {!Filter.Mempool.state}. *)
  type filter_state

  (** Type instantiated by {!Filter.Mempool.config}. *)
  type filter_config

  (** Similar to the type [operation] from the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type protocol_operation

  (** Type instantiated by {!Prevalidation.t} *)
  type prevalidation_t

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable validation_state : prevalidation_t;
        (** Internal prevalidation state. Among others, this contains the
            internal states of the protocol mempool and of the plugin
            filter. *)
    mutable operation_stream :
      (Classification.classification * protocol_operation operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state Tezos_rpc.Directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

  (** This function fetches an operation if it is not already handled
      as defined by [already_handled] below. The implementation makes
      sure to fetch an operation at most once, modulo operations
      lost because of bounded buffers becoming full.

      This function is an intruder to this module type. It just happens
      that it is needed both by internals of the implementation of {!S}
      and by the internals of the implementation of {!T}; so it needs
      to be exposed here. *)
  val may_fetch_operation :
    (protocol_operation, prevalidation_t) types_state_shell ->
    P2p_peer_id.t option ->
    Operation_hash.t ->
    unit Lwt.t

  (** The function called after every call to a function of {!API}. *)
  val handle_unprocessed : types_state -> unit Lwt.t

  (** The inner API of the mempool i.e. functions called by the worker
      when an individual request arrives. These functions are the
      most high-level ones that we test. All these [on_*] functions
      correspond to a single event. Possible
      sequences of calls to this API are always of the form:

      on_*; handle_unprocessed; on_*; handle_unprocessed; ... *)
  module Requests : sig
    val on_advertise : _ types_state_shell -> unit

    val on_arrived :
      types_state ->
      Operation_hash.t ->
      Operation.t ->
      (unit, Empty.t) result Lwt.t

    val on_ban : types_state -> Operation_hash.t -> unit tzresult Lwt.t

    val on_flush :
      handle_branch_refused:bool ->
      types_state ->
      Store.Block.t ->
      Block_hash.Set.t ->
      Operation_hash.Set.t ->
      unit tzresult Lwt.t

    val on_inject :
      types_state -> force:bool -> Operation.t -> unit tzresult Lwt.t

    val on_notify :
      _ types_state_shell -> P2p_peer_id.t -> Mempool.t -> unit Lwt.t
  end
end

(** A functor for obtaining the testable part of this file (see
    the instantiation of this functor in {!Internal_for_tests} at the
    end of this file). Contrary to the production-only functor {!Make} below,
    this functor doesn't assume a specific chain store implementation,
    which is the crux for having it easily unit-testable. *)
module Make_s
    (Filter : Shell_plugin.FILTER)
    (Prevalidation_t : Prevalidation.T
                         with type validation_state =
                           Filter.Proto.validation_state
                          and type filter_state = Filter.Mempool.state
                          and type filter_config = Filter.Mempool.config
                          and type protocol_operation = Filter.Proto.operation) :
  S
    with type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type protocol_operation = Filter.Proto.operation
     and type prevalidation_t = Prevalidation_t.t = struct
  type filter_state = Filter.Mempool.state

  type filter_config = Filter.Mempool.config

  type protocol_operation = Filter.Proto.operation

  type prevalidation_t = Prevalidation_t.t

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable validation_state : prevalidation_t;
    mutable operation_stream :
      (Classification.classification * protocol_operation operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state Tezos_rpc.Directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

  (* This function is in [Lwt] only for logging. *)
  let already_handled ~origin shell oph =
    let open Lwt_syntax in
    if Operation_hash.Set.mem oph shell.banned_operations then
      let+ () = Events.(emit ban_operation_encountered) (origin, oph) in
      true
    else
      Lwt.return
        (Pending_ops.mem oph shell.pending
        || Operation_hash.Set.mem oph shell.fetching
        || Operation_hash.Set.mem oph shell.live_operations
        || Classification.is_in_mempool oph shell.classification <> None
        || Classification.is_known_unparsable oph shell.classification)

  let advertise (shell : ('operation_data, _) types_state_shell) mempool =
    let open Lwt_syntax in
    match shell.advertisement with
    | `Pending {Mempool.known_valid; pending} ->
        shell.advertisement <-
          `Pending
            {
              known_valid = known_valid @ mempool.Mempool.known_valid;
              pending = Operation_hash.Set.union pending mempool.pending;
            }
    | `None ->
        shell.advertisement <- `Pending mempool ;
        Lwt.dont_wait
          (fun () ->
            let* () = Lwt_unix.sleep advertisement_delay in
            shell.worker.push_request_now Advertise ;
            Lwt.return_unit)
          (fun exc ->
            Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))

  (* Each classified operation should be notified exactly ONCE for a
     given stream. Operations which cannot be parsed are not notified. *)
  let handle_classification
      ~(notifier :
         Classification.classification -> protocol_operation operation -> unit)
      shell (op, kind) =
    Classification.add kind op shell.classification ;
    notifier kind op

  let mk_notifier operation_stream classification op =
    (* This callback is safe encapsulation-wise, because it depends
       on an "harmless" field of [types_state_shell]: [operation_stream] *)
    Lwt_watcher.notify operation_stream (classification, op)

  let pre_filter pv ~notifier parsed_op : [Pending_ops.priority | `Drop] Lwt.t =
    let open Lwt_syntax in
    let+ v =
      Prevalidation_t.pre_filter pv.validation_state pv.filter_config parsed_op
    in
    match v with
    | (`Branch_delayed _ | `Branch_refused _ | `Refused _ | `Outdated _) as errs
      ->
        handle_classification ~notifier pv.shell (parsed_op, errs) ;
        `Drop
    | `Passed_prefilter priority -> (priority :> [Pending_ops.priority | `Drop])

  let set_mempool shell mempool =
    shell.mempool <- mempool ;
    shell.parameters.tools.set_mempool
      ~head:(Store.Block.hash shell.predecessor)
      shell.mempool

  let remove_from_advertisement oph = function
    | `Pending mempool -> `Pending (Mempool.remove oph mempool)
    | `None -> `None

  (* This function retrieves an old/replaced operation and reclassifies it as
     [replacement_classification]. Note that we don't need to re-flush the
     mempool, as this function is only called in precheck mode.

     The operation is expected to be (a) parsable and (b) in the "prechecked"
     class. So, we softly handle the situations where the operation is
     unparsable or not found in any class in case this invariant is broken
     for some reason.
  *)
  let reclassify_replaced_manager_op old_hash shell
      (replacement_classification : [< Classification.error_classification]) =
    shell.advertisement <-
      remove_from_advertisement old_hash shell.advertisement ;
    match Classification.remove old_hash shell.classification with
    | Some (op, _class) ->
        Some (op, (replacement_classification :> Classification.classification))
    | None ->
        (* This case should not happen. *)
        shell.parameters.tools.chain_tools.clear_or_cancel old_hash ;
        None

  (* Determine the classification of a given operation in the current
     validation state, i.e. whether it could be included in a
     block on top of the current head, and if not, why. If yes, the
     operation is accumulated in the given [mempool].

     The function returns a tuple
     [(validation_state, mempool, to_handle)], where:
     - [validation_state] is the (possibly) updated validation_state,
     - [mempool] is the (possibly) updated mempool,
     - [to_handle] contains the given operation and its classification, and all
       operations whose classes are changed/impacted by this classification
       (eg. in case of operation replacement).
  *)
  let classify_operation shell ~filter_config ~validation_state mempool op :
      (prevalidation_t
      * Mempool.t
      * (protocol_operation operation * Classification.classification) trace)
      Lwt.t =
    let open Lwt_syntax in
    let* v_state, op, classification, replacements =
      Prevalidation_t.add_operation validation_state filter_config op
    in
    let to_replace =
      List.filter_map
        (fun (replaced_oph, new_classification) ->
          reclassify_replaced_manager_op replaced_oph shell new_classification)
        replacements
    in
    let to_handle = (op, classification) :: to_replace in
    let mempool =
      match classification with
      | `Prechecked | `Applied -> Mempool.cons_valid op.hash mempool
      | `Branch_refused _ | `Branch_delayed _ | `Refused _ | `Outdated _ ->
          mempool
    in
    return (v_state, mempool, to_handle)

  (* Classify pending operations into either: [Refused |
     Branch_delayed | Branch_refused | Applied | Outdated].
     To ensure fairness with other worker requests, classification of
     operations is done by batch of [operation_batch_size] operations.

     This function ensures the following invariants:

     - If an operation is classified, it is not part of the [pending]
     map

     - A classified operation is part of the [in_mempool] set

     - A classified operation is part only of one of the following
     classes: [Branch_refused, Branch_delayed, Refused, Applied]

     Moreover, this function ensures that only each newly classified
     operations are advertised to the remote peers. However, if a peer
     requests our mempool, we advertise all our classified operations and
     all our pending operations. *)
  let classify_pending_operations ~notifier shell filter_config state =
    let open Lwt_syntax in
    let* r =
      Pending_ops.fold_es
        (fun _prio oph op (acc_validation_state, acc_mempool, limit) ->
          if limit <= 0 then
            (* Using Error as an early-return mechanism *)
            Lwt.return_error (acc_validation_state, acc_mempool)
          else (
            shell.pending <- Pending_ops.remove oph shell.pending ;
            let* new_validation_state, new_mempool, to_handle =
              classify_operation
                shell
                ~filter_config
                ~validation_state:acc_validation_state
                acc_mempool
                op
            in
            let+ () = Events.(emit operation_reclassified) oph in
            List.iter (handle_classification ~notifier shell) to_handle ;
            Ok (new_validation_state, new_mempool, limit - 1)))
        shell.pending
        (state, Mempool.empty, shell.parameters.limits.operations_batch_size)
    in
    match r with
    | Error (state, advertised_mempool) ->
        (* Early return after iteration limit was reached *)
        let* (_was_pushed : bool) =
          shell.worker.push_request Request.Leftover
        in
        Lwt.return (state, advertised_mempool)
    | Ok (state, advertised_mempool, _) -> Lwt.return (state, advertised_mempool)

  let update_advertised_mempool_fields pv_shell delta_mempool =
    let open Lwt_syntax in
    if Mempool.is_empty delta_mempool then Lwt.return_unit
    else
      (* We only advertise newly classified operations. *)
      let mempool_to_advertise =
        Mempool.
          {delta_mempool with known_valid = List.rev delta_mempool.known_valid}
      in
      advertise pv_shell mempool_to_advertise ;
      let our_mempool =
        let prechecked_hashes =
          (* Outputs hashes in "decreasing" order which should not matter *)
          Classification.Sized_map.fold
            (fun x _ acc -> x :: acc)
            pv_shell.classification.prechecked
            []
        in
        {
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2065
             This field does not only contain valid operation *)
          Mempool.known_valid =
            List.fold_left
              (fun acc op -> op.hash :: acc)
              prechecked_hashes
              pv_shell.classification.applied_rev;
          pending = Pending_ops.hashes pv_shell.pending;
        }
      in
      let* _res = set_mempool pv_shell our_mempool in
      Lwt.pause ()

  let handle_unprocessed pv =
    let open Lwt_syntax in
    let notifier = mk_notifier pv.operation_stream in
    if Pending_ops.is_empty pv.shell.pending then Lwt.return_unit
    else
      let* () = Events.(emit processing_operations) () in
      let* validation_state, delta_mempool =
        classify_pending_operations
          ~notifier
          pv.shell
          pv.filter_config
          pv.validation_state
      in
      pv.validation_state <- validation_state ;
      update_advertised_mempool_fields pv.shell delta_mempool

  (* This function fetches one operation through the
     [distributed_db]. On errors, we emit an event and proceed as
     usual. *)
  let fetch_operation (shell : ('operation_data, _) types_state_shell) ?peer oph
      =
    let open Lwt_syntax in
    let+ () = Events.(emit fetching_operation) oph in
    let* r =
      shell.parameters.tools.fetch
        ~timeout:shell.parameters.limits.operation_timeout
        ?peer
        oph
    in
    match r with
    | Ok op ->
        shell.worker.push_request_now (Arrived (oph, op)) ;
        Lwt.return_unit
    | Error (Distributed_db.Operation.Canceled _ :: _) ->
        Events.(emit operation_included) oph
    | Error _ ->
        (* This may happen if the peer timed out for example. *)
        Events.(emit operation_not_fetched) oph

  (* This function fetches an operation if it is not already handled
     by the mempool. To ensure we fetch at most a given operation,
     we record it in the [pv.fetching] field.

     Invariant: This function should be the only one to modify this
     field.

     Invariant: To ensure, there is no leak, we ensure that when the
     promise [p] is terminated, we remove the operation from the
     fetching operations. This is to ensure that if an error
     happened, we can still fetch this operation in the future. *)
  let may_fetch_operation (shell : ('operation_data, _) types_state_shell) peer
      oph =
    let open Lwt_syntax in
    let origin =
      match peer with Some peer -> Events.Peer peer | None -> Leftover
    in
    let* already_handled = already_handled ~origin shell oph in
    if not already_handled then
      ignore
        (Lwt.finalize
           (fun () ->
             shell.fetching <- Operation_hash.Set.add oph shell.fetching ;
             fetch_operation shell ?peer oph)
           (fun () ->
             shell.fetching <- Operation_hash.Set.remove oph shell.fetching ;
             Lwt.return_unit)) ;
    Lwt.return_unit

  (** Module containing functions that are the internal transitions
      of the mempool. These functions are called by the {!Worker} when
      an event arrives. *)
  module Requests = struct
    module Parser = MakeParser (Filter.Proto)

    let on_arrived (pv : types_state) oph op : (unit, Empty.t) result Lwt.t =
      let open Lwt_syntax in
      let* already_handled =
        already_handled ~origin:Events.Arrived pv.shell oph
      in
      if already_handled then return_ok_unit
      else
        match Parser.parse oph op with
        | Error _ ->
            let* () = Events.(emit unparsable_operation) oph in
            Prevalidator_classification.add_unparsable
              oph
              pv.shell.classification ;
            return_ok_unit
        | Ok parsed_op -> (
            let* v =
              pre_filter
                pv
                ~notifier:(mk_notifier pv.operation_stream)
                parsed_op
            in
            match v with
            | `Drop -> return_ok_unit
            | (`High | `Medium | `Low _) as prio ->
                if
                  not
                    (Block_hash.Set.mem
                       op.Operation.shell.branch
                       pv.shell.live_blocks)
                then (
                  pv.shell.parameters.tools.chain_tools.clear_or_cancel oph ;
                  return_ok_unit)
                else (
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/1723
                     Should this have an influence on the peer's score ? *)
                  pv.shell.pending <-
                    Pending_ops.add parsed_op prio pv.shell.pending ;
                  return_ok_unit))

    let on_inject (pv : types_state) ~force op =
      let open Lwt_result_syntax in
      let oph = Operation.hash op in
      (* Currently, an injection is always done with the highest priority, because:
         - We want to process and propagate the injected operations fast,
         - We don't want to call prefilter to get the priority.
         But, this may change in the future
      *)
      let prio = `High in
      let*! already_handled =
        already_handled ~origin:Events.Injected pv.shell oph
      in
      if already_handled then
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1722
           Is this an error? *)
        return_unit
      else
        match Parser.parse oph op with
        | Error err ->
            failwith
              "Invalid operation %a: %a."
              Operation_hash.pp
              oph
              Error_monad.pp_print_trace
              err
        | Ok parsed_op -> (
            if force then (
              let*! () =
                pv.shell.parameters.tools.chain_tools.inject_operation oph op
              in
              pv.shell.pending <-
                Pending_ops.add parsed_op prio pv.shell.pending ;
              return_unit)
            else if
              not
                (Block_hash.Set.mem
                   op.Operation.shell.branch
                   pv.shell.live_blocks)
            then
              failwith
                "Operation %a is branched on a block %a which is too old"
                Operation_hash.pp
                oph
                Block_hash.pp
                op.Operation.shell.branch
            else
              let notifier = mk_notifier pv.operation_stream in
              let*! validation_state, delta_mempool, to_handle =
                classify_operation
                  pv.shell
                  ~filter_config:pv.filter_config
                  ~validation_state:pv.validation_state
                  Mempool.empty
                  parsed_op
              in
              let op_status =
                (* to_handle contains the given operation and its classification, and
                   all operations whose classes are changed/impacted by this
                   classification (eg. in case of operation replacement). Here, we
                   retrieve the classification of our operation. *)
                List.find_opt
                  (function
                    | ({hash; _} : protocol_operation operation), _ ->
                        Operation_hash.equal hash oph)
                  to_handle
              in
              match op_status with
              | Some (_h, (`Applied | `Prechecked)) ->
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2294
                     In case of `Passed_precheck_with_replace, we may want to only do
                     the injection/replacement if a flag `replace` is set to true
                     in the injection query. *)
                  let*! () =
                    pv.shell.parameters.tools.chain_tools.inject_operation
                      oph
                      op
                  in
                  (* Call handle & update_advertised_mempool only if op is accepted *)
                  List.iter (handle_classification ~notifier pv.shell) to_handle ;
                  pv.validation_state <- validation_state ;
                  (* Note that in this case, we may advertise an operation and bypass
                     the prioritirization strategy. *)
                  let*! v =
                    update_advertised_mempool_fields pv.shell delta_mempool
                  in
                  return v
              | Some
                  ( _h,
                    ( `Branch_delayed e
                    | `Branch_refused e
                    | `Refused e
                    | `Outdated e ) ) ->
                  Lwt.return
                  @@ error_with
                       "Error while applying operation %a:@ %a"
                       Operation_hash.pp
                       oph
                       pp_print_trace
                       e
              | None ->
                  (* This case should not happen *)
                  failwith
                    "Unexpected error while injecting operation %a. Operation \
                     not found after classifying it."
                    Operation_hash.pp
                    oph)

    let on_notify (shell : ('operation_data, _) types_state_shell) peer mempool
        =
      let open Lwt_syntax in
      let may_fetch_operation = may_fetch_operation shell (Some peer) in
      let* () = List.iter_s may_fetch_operation mempool.Mempool.known_valid in
      Seq.iter_s
        may_fetch_operation
        (Operation_hash.Set.to_seq mempool.Mempool.pending)

    let on_flush ~handle_branch_refused pv new_predecessor new_live_blocks
        new_live_operations =
      let open Lwt_result_syntax in
      let old_predecessor = pv.shell.predecessor in
      pv.shell.predecessor <- new_predecessor ;
      pv.shell.live_blocks <- new_live_blocks ;
      pv.shell.live_operations <- new_live_operations ;
      Lwt_watcher.shutdown_input pv.operation_stream ;
      pv.operation_stream <- Lwt_watcher.create_input () ;
      let timestamp_system = Tezos_base.Time.System.now () in
      pv.shell.timestamp <- timestamp_system ;
      let timestamp = Time.System.to_protocol timestamp_system in
      let* validation_state =
        pv.shell.parameters.tools.flush
          ~head:new_predecessor
          ~timestamp
          pv.validation_state
      in
      pv.validation_state <- validation_state ;
      let*! new_pending_operations =
        Classification.recycle_operations
          ~from_branch:old_predecessor
          ~to_branch:new_predecessor
          ~live_blocks:new_live_blocks
          ~parse:(fun oph op -> Result.to_option (Parser.parse oph op))
          ~classes:pv.shell.classification
          ~pending:(Pending_ops.operations pv.shell.pending)
          ~block_store:block_tools
          ~chain:pv.shell.parameters.tools.chain_tools
          ~handle_branch_refused
      in
      (* Could be implemented as Operation_hash.Map.filter_s which
         does not exist for the moment. *)
      let*! new_pending_operations, nb_pending =
        Operation_hash.Map.fold_s
          (fun _oph op (pending, nb_pending) ->
            let*! v =
              pre_filter pv ~notifier:(mk_notifier pv.operation_stream) op
            in
            match v with
            | `Drop -> Lwt.return (pending, nb_pending)
            | (`High | `Medium | `Low _) as prio ->
                (* Here, an operation injected in this node with `High priority will
                   now get its approriate priority. *)
                Lwt.return (Pending_ops.add op prio pending, nb_pending + 1))
          new_pending_operations
          (Pending_ops.empty, 0)
      in
      let*! () = Events.(emit operations_to_reclassify) nb_pending in
      pv.shell.pending <- new_pending_operations ;
      set_mempool pv.shell Mempool.empty

    let on_advertise (shell : ('protocol_data, _) types_state_shell) =
      match shell.advertisement with
      | `None ->
          () (* May happen if nothing to advertise since last advertisement. *)
      | `Pending mempool ->
          shell.advertisement <- `None ;
          (* In this case, mempool is not empty, but let's avoid advertising
             empty mempools in case this invariant is broken. *)
          if not (Mempool.is_empty mempool) then
            shell.parameters.tools.advertise_current_head
              ~mempool
              shell.predecessor

    (* If [flush_if_prechecked] is [true], removing a prechecked
       operation triggers a flush of the mempool. Because flushing may
       be costly this should be done only when the action is triggered
       locally by the user. This allows a better UX if the user bans a
       prechecked operation so that a branch delayed operation becomes
       [applied] again. *)
    let remove ~flush_if_prechecked pv oph =
      let open Lwt_result_syntax in
      pv.shell.parameters.tools.chain_tools.clear_or_cancel oph ;
      pv.shell.advertisement <-
        remove_from_advertisement oph pv.shell.advertisement ;
      pv.shell.banned_operations <-
        Operation_hash.Set.add oph pv.shell.banned_operations ;
      match Classification.remove oph pv.shell.classification with
      | None ->
          pv.shell.pending <- Pending_ops.remove oph pv.shell.pending ;
          pv.shell.fetching <- Operation_hash.Set.remove oph pv.shell.fetching ;
          return_unit
      | Some (_op, classification) -> (
          match (classification, flush_if_prechecked) with
          | `Prechecked, true | `Applied, _ ->
              (* Modifying the list of operations classified as [Applied]
                 might change the classification of all the operations in
                 the mempool. Hence if the removed operation has been
                 applied we flush the mempool to force the
                 reclassification of all the operations except the one
                 removed. *)
              let+ () =
                on_flush
                  ~handle_branch_refused:false
                  pv
                  pv.shell.predecessor
                  pv.shell.live_blocks
                  pv.shell.live_operations
              in
              pv.shell.pending <- Pending_ops.remove oph pv.shell.pending
          | `Branch_delayed _, _
          | `Branch_refused _, _
          | `Refused _, _
          | `Outdated _, _
          | `Prechecked, false ->
              pv.validation_state <-
                Prevalidation_t.remove_operation pv.validation_state oph ;
              return_unit)

    let on_ban pv oph_to_ban =
      pv.shell.banned_operations <-
        Operation_hash.Set.add oph_to_ban pv.shell.banned_operations ;
      remove ~flush_if_prechecked:true pv oph_to_ban
  end
end

module type ARG = sig
  val limits : Shell_limits.prevalidator_limits

  val chain_db : Distributed_db.chain_db

  val chain_id : Chain_id.t
end

module WorkerGroup = Worker.MakeGroup (Name) (Prevalidator_worker_state.Request)

(** The functor that is not tested, in other words used only in production.
    This functor's code is not tested (contrary to functor {!Make_s} above),
    because it hardcodes a dependency to [Store.chain_store] in its instantiation
    of type [chain_store]. This is what makes the code of this functor
    not testable for the moment, because [Store.chain_store] has poor
    testing capabilities.

    Note that, because this functor [include]s {!Make_s}, it is a
    strict extension of [Make_s]. *)
module Make
    (Filter : Shell_plugin.FILTER)
    (Arg : ARG)
    (Prevalidation_t : Prevalidation.T
                         with type validation_state =
                           Filter.Proto.validation_state
                          and type filter_state = Filter.Mempool.state
                          and type filter_config = Filter.Mempool.config
                          and type protocol_operation = Filter.Proto.operation
                          and type chain_store = Store.chain_store) : T = struct
  module S = Make_s (Filter) (Prevalidation_t)
  open S

  type types_state = S.types_state

  let get_rpc_directory pv = pv.rpc_directory

  let name = (Arg.chain_id, Filter.Proto.hash)

  module Types = struct
    type state = types_state

    type parameters = Shell_limits.prevalidator_limits * Distributed_db.chain_db
  end

  module Worker :
    Worker.T
      with type Name.t = Name.t
       and type ('a, 'b) Request.t = ('a, 'b) Request.t
       and type Request.view = Request.view
       and type Types.state = Types.state
       and type Types.parameters = Types.parameters =
    WorkerGroup.MakeWorker (Types)

  open Types

  type worker = Worker.infinite Worker.queue Worker.t

  (** Returns a json describing the prevalidator's [filter_config].
      The boolean [include_default] ([true] by default) indicates
      whether the json should include the fields which have a value
      equal to their default value. *)
  let get_filter_config_json ?(include_default = true) pv =
    let include_default_fields = if include_default then `Always else `Never in
    Data_encoding.Json.construct
      ~include_default_fields
      Filter.Mempool.config_encoding
      pv.filter_config

  let filter_validation_passes allowed_validation_passes
      (op : protocol_operation) =
    match allowed_validation_passes with
    | [] -> true
    | validation_passes -> (
        match Filter.Proto.acceptable_pass op with
        | None -> false
        | Some validation_pass ->
            List.mem ~equal:Compare.Int.equal validation_pass validation_passes)

  let build_rpc_directory w =
    lazy
      (let open Lwt_result_syntax in
      let dir : state Tezos_rpc.Directory.t ref =
        ref Tezos_rpc.Directory.empty
      in
      let module Proto_services =
        Block_services.Make (Filter.Proto) (Filter.Proto)
      in
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.get_filter Tezos_rpc.Path.open_root)
          (fun pv params () ->
            return
              (get_filter_config_json
                 ~include_default:params#include_default
                 pv)) ;
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.set_filter Tezos_rpc.Path.open_root)
          (fun pv () obj ->
            let open Lwt_syntax in
            let* () =
              try
                let config =
                  Data_encoding.Json.destruct Filter.Mempool.config_encoding obj
                in
                pv.filter_config <- config ;
                Lwt.return_unit
              with _ -> Events.(emit invalid_mempool_filter_configuration) ()
            in
            return_ok (get_filter_config_json pv)) ;
      (* Ban an operation (from its given hash): remove it from the
         mempool if present. Add it to the set pv.banned_operations
         to prevent it from being fetched/processed/injected in the
         future.
         Note: If the baker has already received the operation, then
         it's necessary to restart it manually to flush the operation
         from it. *)
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.ban_operation Tezos_rpc.Path.open_root)
          (fun _pv () oph ->
            let open Lwt_result_syntax in
            let*! r = Worker.Queue.push_request_and_wait w (Request.Ban oph) in
            match r with
            | Error (Closed None) -> fail [Worker_types.Terminated]
            | Error (Closed (Some errs)) -> fail errs
            | Error (Request_error err) -> fail err
            | Error (Any exn) -> fail [Exn exn]
            | Ok () -> return_unit) ;
      (* Unban an operation (from its given hash): remove it from the
         set pv.banned_operations (nothing happens if it was not banned). *)
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.unban_operation Tezos_rpc.Path.open_root)
          (fun pv () oph ->
            pv.shell.banned_operations <-
              Operation_hash.Set.remove oph pv.shell.banned_operations ;
            return_unit) ;
      (* Unban all operations: clear the set pv.banned_operations. *)
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.unban_all_operations
             Tezos_rpc.Path.open_root)
          (fun pv () () ->
            pv.shell.banned_operations <- Operation_hash.Set.empty ;
            return_unit) ;
      dir :=
        Tezos_rpc.Directory.gen_register
          !dir
          (Proto_services.S.Mempool.pending_operations Tezos_rpc.Path.open_root)
          (fun pv params () ->
            (* FIXME https://gitlab.com/tezos/tezos/-/issues/2250

               We merge prechecked operation with applied operation
               so that the encoding of the RPC does not need to be
               changed. Once prechecking will be done by the protocol
               and not the plugin, we will change the encoding to
               reflect that. *)
            let prechecked_with_applied =
              if params#applied then
                let applied_seq =
                  pv.shell.classification.applied_rev |> List.to_seq
                  |> Seq.map (fun ({hash; _} as op) -> (hash, op))
                in
                Classification.Sized_map.to_map
                  pv.shell.classification.prechecked
                |> Operation_hash.Map.to_seq |> Seq.append applied_seq
                |> Seq.filter_map (fun (oph, op) ->
                       if
                         filter_validation_passes
                           params#validation_passes
                           op.protocol
                       then Some (oph, op.protocol)
                       else None)
                |> List.of_seq
              else []
            in
            let process_map map =
              let open Operation_hash in
              Map.filter_map
                (fun _oph (op, error) ->
                  if
                    filter_validation_passes
                      params#validation_passes
                      op.protocol
                  then Some (op.protocol, error)
                  else None)
                map
            in
            let refused =
              if params#refused then
                process_map (Classification.map pv.shell.classification.refused)
              else Operation_hash.Map.empty
            in
            let outdated =
              if params#outdated then
                process_map
                  (Classification.map pv.shell.classification.outdated)
              else Operation_hash.Map.empty
            in
            let branch_refused =
              if params#branch_refused then
                process_map
                  (Classification.map pv.shell.classification.branch_refused)
              else Operation_hash.Map.empty
            in
            let branch_delayed =
              if params#branch_delayed then
                process_map
                  (Classification.map pv.shell.classification.branch_delayed)
              else Operation_hash.Map.empty
            in
            let unprocessed =
              Operation_hash.Map.filter_map
                (fun _ {protocol; _} ->
                  if filter_validation_passes params#validation_passes protocol
                  then Some protocol
                  else None)
                (Pending_ops.operations pv.shell.pending)
            in
            let pending_operations =
              {
                Proto_services.Mempool.applied = prechecked_with_applied;
                refused;
                outdated;
                branch_refused;
                branch_delayed;
                unprocessed;
              }
            in
            Proto_services.Mempool.pending_operations_version_dispatcher
              ~version:params#version
              pending_operations) ;
      dir :=
        Tezos_rpc.Directory.register
          !dir
          (Proto_services.S.Mempool.request_operations Tezos_rpc.Path.open_root)
          (fun pv t () ->
            pv.shell.parameters.tools.send_get_current_head ?peer:t#peer_id () ;
            return_unit) ;
      dir :=
        Tezos_rpc.Directory.gen_register
          !dir
          (Proto_services.S.Mempool.monitor_operations Tezos_rpc.Path.open_root)
          (fun pv params () ->
            Lwt_mutex.with_lock pv.lock @@ fun () ->
            let op_stream, stopper =
              Lwt_watcher.create_stream pv.operation_stream
            in
            (* First call : retrieve the current set of op from the mempool *)
            let applied_seq =
              if params#applied then
                pv.shell.classification.applied_rev |> List.to_seq
                |> Seq.map (fun {hash; protocol; _} -> ((hash, protocol), None))
              else Seq.empty
            in
            (* FIXME https://gitlab.com/tezos/tezos/-/issues/2250

               For the moment, applied and prechecked operations are
               handled the same way for the user point of view. *)
            let prechecked_seq =
              if params#applied then
                Classification.Sized_map.to_map
                  pv.shell.classification.prechecked
                |> Operation_hash.Map.to_seq
                |> Seq.map (fun (hash, {protocol; _}) ->
                       ((hash, protocol), None))
              else Seq.empty
            in
            let process_error_map map =
              let open Operation_hash in
              map |> Map.to_seq
              |> Seq.map (fun (hash, (op, error)) ->
                     ((hash, op.protocol), Some error))
            in
            let refused_seq =
              if params#refused then
                process_error_map
                  (Classification.map pv.shell.classification.refused)
              else Seq.empty
            in
            let branch_refused_seq =
              if params#branch_refused then
                process_error_map
                  (Classification.map pv.shell.classification.branch_refused)
              else Seq.empty
            in
            let branch_delayed_seq =
              if params#branch_delayed then
                process_error_map
                  (Classification.map pv.shell.classification.branch_delayed)
              else Seq.empty
            in
            let outdated_seq =
              if params#outdated then
                process_error_map
                  (Classification.map pv.shell.classification.outdated)
              else Seq.empty
            in
            let filter ((_, op), _) =
              filter_validation_passes params#validation_passes op
            in
            let current_mempool =
              Seq.append outdated_seq branch_delayed_seq
              |> Seq.append branch_refused_seq
              |> Seq.append refused_seq |> Seq.append prechecked_seq
              |> Seq.append applied_seq |> Seq.filter filter |> List.of_seq
            in
            let current_mempool = ref (Some current_mempool) in
            let filter_result = function
              | `Prechecked | `Applied -> params#applied
              | `Refused _ -> params#refused
              | `Outdated _ -> params#outdated
              | `Branch_refused _ -> params#branch_refused
              | `Branch_delayed _ -> params#branch_delayed
            in
            let rec next () =
              let open Lwt_syntax in
              match !current_mempool with
              | Some mempool ->
                  current_mempool := None ;
                  Lwt.return_some mempool
              | None -> (
                  let* o = Lwt_stream.get op_stream in
                  match o with
                  | Some (kind, op) when filter_result kind ->
                      let errors =
                        match kind with
                        | `Prechecked | `Applied -> None
                        | `Branch_delayed errors
                        | `Branch_refused errors
                        | `Refused errors
                        | `Outdated errors ->
                            Some errors
                      in
                      Lwt.return_some [((op.hash, op.protocol), errors)]
                  | Some _ -> next ()
                  | None -> Lwt.return_none)
            in
            let shutdown () = Lwt_watcher.shutdown stopper in
            Tezos_rpc.Answer.return_stream {next; shutdown}) ;
      !dir)

  (** Module implementing the events at the {!Worker} level. Contrary
      to {!Requests}, these functions depend on [Worker]. *)
  module Handlers = struct
    type self = worker

    let on_request :
        type r request_error.
        worker ->
        (r, request_error) Request.t ->
        (r, request_error) result Lwt.t =
     fun w request ->
      let open Lwt_result_syntax in
      Prometheus.Counter.inc_one metrics.worker_counters.worker_request_count ;
      let pv = Worker.state w in
      let post_processing :
          (r, request_error) result Lwt.t -> (r, request_error) result Lwt.t =
       fun r ->
        let open Lwt_syntax in
        let* () = handle_unprocessed pv in
        r
      in
      post_processing
      @@
      match request with
      | Request.Flush (hash, event, live_blocks, live_operations) ->
          Requests.on_advertise pv.shell ;
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/1727
             Rebase the advertisement instead. *)
          let* block = pv.shell.parameters.tools.read_block hash in
          let handle_branch_refused =
            Chain_validator_worker_state.(
              match event with
              | Head_increment | Ignored_head -> false
              | Branch_switch -> true)
          in
          Lwt_mutex.with_lock pv.lock
          @@ fun () : (r, error trace) result Lwt.t ->
          Requests.on_flush
            ~handle_branch_refused
            pv
            block
            live_blocks
            live_operations
      | Request.Notify (peer, mempool) ->
          let*! () = Requests.on_notify pv.shell peer mempool in
          return_unit
      | Request.Leftover ->
          (* unprocessed ops are handled just below *)
          return_unit
      | Request.Inject {op; force} -> Requests.on_inject pv ~force op
      | Request.Arrived (oph, op) -> Requests.on_arrived pv oph op
      | Request.Advertise ->
          Requests.on_advertise pv.shell ;
          return_unit
      | Request.Ban oph -> Requests.on_ban pv oph

    let on_close w =
      let pv = Worker.state w in
      Lwt_watcher.shutdown_input pv.operation_stream ;
      Operation_hash.Set.iter
        pv.shell.parameters.tools.chain_tools.clear_or_cancel
        pv.shell.fetching ;
      Lwt.return_unit

    let mk_tools (chain_db : Distributed_db.chain_db) : _ Tools.tools =
      let advertise_current_head ~mempool bh =
        Distributed_db.Advertise.current_head chain_db ~mempool bh
      in
      let chain_tools = mk_chain_tools chain_db in
      let flush = Prevalidation_t.flush (Distributed_db.chain_store chain_db) in
      let fetch ?peer ?timeout oph =
        Distributed_db.Operation.fetch chain_db ?timeout ?peer oph ()
      in
      let read_block bh =
        let chain_store = Distributed_db.chain_store chain_db in
        Store.Block.read_block chain_store bh
      in
      let send_get_current_head ?peer () =
        match peer with
        | None -> Distributed_db.Request.current_head_from_all chain_db
        | Some peer ->
            Distributed_db.Request.current_head_from_peer chain_db peer
      in
      let set_mempool ~head mempool =
        let chain_store = Distributed_db.chain_store chain_db in
        Store.Chain.set_mempool chain_store ~head mempool
      in
      {
        advertise_current_head;
        chain_tools;
        flush;
        fetch;
        read_block;
        send_get_current_head;
        set_mempool;
      }

    let mk_worker_tools w : Tools.worker_tools =
      let push_request r = Worker.Queue.push_request w r in
      let push_request_now r = Worker.Queue.push_request_now w r in
      {push_request; push_request_now}

    type launch_error = error trace

    let on_launch w _ (limits, chain_db) : (state, launch_error) result Lwt.t =
      let open Lwt_result_syntax in
      let chain_store = Distributed_db.chain_store chain_db in
      let*! head = Store.Chain.current_head chain_store in
      let*! mempool = Store.Chain.mempool chain_store in
      let*! live_blocks, live_operations =
        Store.Chain.live_blocks chain_store
      in
      let timestamp_system = Tezos_base.Time.System.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      let* validation_state =
        Prevalidation_t.create chain_store ~head ~timestamp
      in
      let fetching =
        List.fold_left
          (fun s h -> Operation_hash.Set.add h s)
          Operation_hash.Set.empty
          mempool.known_valid
      in
      let classification_parameters =
        Classification.
          {
            map_size_limit = limits.Shell_limits.max_refused_operations;
            on_discarded_operation =
              Distributed_db.Operation.clear_or_cancel chain_db;
          }
      in
      let classification = Classification.create classification_parameters in
      let parameters = {limits; tools = mk_tools chain_db} in
      let shell =
        {
          classification;
          parameters;
          predecessor = head;
          timestamp = timestamp_system;
          live_blocks;
          live_operations;
          mempool = Mempool.empty;
          fetching;
          pending = Pending_ops.empty;
          advertisement = `None;
          banned_operations = Operation_hash.Set.empty;
          worker = mk_worker_tools w;
        }
      in
      Shell_metrics.Mempool.set_applied_collector (fun () ->
          List.length shell.classification.applied_rev |> float_of_int) ;
      Shell_metrics.Mempool.set_prechecked_collector (fun () ->
          Prevalidator_classification.Sized_map.cardinal
            shell.classification.prechecked
          |> float_of_int) ;
      Shell_metrics.Mempool.set_refused_collector (fun () ->
          Prevalidator_classification.cardinal shell.classification.refused
          |> float_of_int) ;
      Shell_metrics.Mempool.set_branch_refused_collector (fun () ->
          Prevalidator_classification.cardinal
            shell.classification.branch_refused
          |> float_of_int) ;
      Shell_metrics.Mempool.set_branch_delayed_collector (fun () ->
          Prevalidator_classification.cardinal
            shell.classification.branch_delayed
          |> float_of_int) ;
      Shell_metrics.Mempool.set_outdated_collector (fun () ->
          Prevalidator_classification.cardinal shell.classification.outdated
          |> float_of_int) ;
      Shell_metrics.Mempool.set_unprocessed_collector (fun () ->
          Prevalidator_pending_operations.cardinal shell.pending |> float_of_int) ;

      let pv =
        {
          shell;
          validation_state;
          operation_stream = Lwt_watcher.create_input ();
          rpc_directory = build_rpc_directory w;
          filter_config =
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/1725
               initialize from config file *)
            Filter.Mempool.default_config;
          lock = Lwt_mutex.create ();
        }
      in
      let*! () =
        Seq.iter_s
          (may_fetch_operation pv.shell None)
          (Operation_hash.Set.to_seq fetching)
      in
      return pv

    let on_error (type a b) _w st (request : (a, b) Request.t) (errs : b) :
        unit tzresult Lwt.t =
      Prometheus.Counter.inc_one metrics.worker_counters.worker_error_count ;
      let open Lwt_syntax in
      match request with
      | Request.(Inject _) as r ->
          let* () = Events.(emit request_failed) (Request.view r, st, errs) in
          return_ok_unit
      | Request.Notify _ -> ( match errs with _ -> .)
      | Request.Leftover -> ( match errs with _ -> .)
      | Request.Arrived _ -> ( match errs with _ -> .)
      | Request.Advertise -> ( match errs with _ -> .)
      | Request.Flush _ ->
          let request_view = Request.view request in
          let* () = Events.(emit request_failed) (request_view, st, errs) in
          Lwt.return_error errs
      | Request.Ban _ ->
          let request_view = Request.view request in
          let* () = Events.(emit request_failed) (request_view, st, errs) in
          Lwt.return_error errs

    let on_completion _w r _ st =
      Prometheus.Counter.inc_one metrics.worker_counters.worker_completion_count ;
      match Request.view r with
      | View (Inject _) | View (Ban _) ->
          Events.(emit request_completed_notice) (Request.view r, st)
      | Request.View (Flush _) ->
          Events.(emit request_completed_info) (Request.view r, st)
      | View (Notify _) | View Leftover | View (Arrived _) | View Advertise ->
          Events.(emit request_completed_debug) (Request.view r, st)

    let on_no_request _ = Lwt.return_unit
  end

  let table = Worker.create_table Queue

  (* NOTE: we register a single worker for each instantiation of this Make
   * functor (and thus a single worker for the single instantiation of Worker).
   * Whilst this is somewhat abusing the intended purpose of worker, it is part
   * of a transition plan to a one-worker-per-peer architecture. *)
  let worker_promise =
    Worker.launch table name (Arg.limits, Arg.chain_db) (module Handlers)

  let worker =
    lazy
      (match Lwt.state worker_promise with
      | Lwt.Return (Ok worker) -> worker
      | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep -> assert false)
end

let make limits chain_db chain_id (module Filter : Shell_plugin.FILTER) : t =
  let module Prevalidation_t = Prevalidation.Make (Filter) in
  let module Prevalidator =
    Make
      (Filter)
      (struct
        let limits = limits

        let chain_db = chain_db

        let chain_id = chain_id
      end)
      (Prevalidation_t)
  in
  (module Prevalidator : T)
