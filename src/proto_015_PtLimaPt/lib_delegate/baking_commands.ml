(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Client_proto_args

let pidfile_arg =
  Tezos_clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun _ s -> return s))

let may_lock_pidfile pidfile_opt f =
  match pidfile_opt with
  | None -> f ()
  | Some pidfile ->
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () ->
          failwith "Failed to create the pidfile: %s" pidfile)
        ~filename:pidfile
        f

let http_headers_env_variable =
  "TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS"

let http_headers =
  match Sys.getenv_opt http_headers_env_variable with
  | None -> None
  | Some contents ->
      let lines = String.split_on_char '\n' contents in
      Some
        (List.fold_left
           (fun acc line ->
             match String.index_opt line ':' with
             | None ->
                 invalid_arg
                   (Printf.sprintf
                      "Http headers: invalid %s environment variable, missing \
                       colon"
                      http_headers_env_variable)
             | Some pos ->
                 let header = String.trim (String.sub line 0 pos) in
                 let header = String.lowercase_ascii header in
                 if header <> "host" then
                   invalid_arg
                     (Printf.sprintf
                        "Http headers: invalid %s environment variable, only \
                         'host' headers are supported"
                        http_headers_env_variable) ;
                 let value =
                   String.trim
                     (String.sub line (pos + 1) (String.length line - pos - 1))
                 in
                 (header, value) :: acc)
           []
           lines)

let operations_arg =
  Tezos_clic.arg
    ~long:"operations-pool"
    ~placeholder:"file|uri"
    ~doc:
      (Printf.sprintf
         "When specified, the baker will try to fetch operations from this \
          file (or uri) and to include retrieved operations in the block. The \
          expected format of the contents is a list of operations [ \
          alpha.operation ].  Environment variable '%s' may also be specified \
          to add headers to the requests (only 'host' headers are supported). \
          If the resource cannot be retrieved, e.g., if the file is absent, \
          unreadable, or the web service returns a 404 error, the resource is \
          simply ignored."
         http_headers_env_variable)
    (Tezos_clic.map_parameter
       ~f:(fun uri ->
         let open Baking_configuration in
         match Uri.scheme uri with
         | Some "http" | Some "https" ->
             Operations_source.(Remote {uri; http_headers})
         | None | Some _ ->
             (* acts as if it were file even though it might no be *)
             Operations_source.(Local {filename = Uri.to_string uri}))
       uri_parameter)

let context_path_arg =
  Tezos_clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:
      "When specified, the client will read in the local context at the \
       provided path in order to build the block, instead of relying on the \
       'preapply' RPC."
    string_parameter

let endorsement_force_switch_arg =
  Tezos_clic.switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "Disable consistency, injection and double signature checks for \
       (pre)endorsements."
    ()

let do_not_monitor_node_mempool_arg =
  Tezos_clic.switch
    ~long:"ignore-node-mempool"
    ~doc:
      "Ignore mempool operations from the node and do not subsequently monitor \
       them. Use in conjunction with --operations option to restrict the \
       observed operations to those of the mempool file."
    ()

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let liquidity_baking_toggle_vote_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun _ctxt -> return ["on"; "off"; "pass"])
    (let open Protocol.Alpha_context.Liquidity_baking in
    fun _ctxt -> function
      | "on" -> return LB_on
      | "off" -> return LB_off
      | "pass" -> return LB_pass
      | s ->
          failwith
            "unexpected vote: %s, expected either \"on\", \"off\", or \"pass\"."
            s)

let liquidity_baking_toggle_vote_arg =
  Tezos_clic.arg
    ~doc:
      "Vote to continue or end the liquidity baking subsidy. The possible \
       values for this option are: \"off\" to request ending the subsidy, \
       \"on\" to request continuing or restarting the subsidy, and \"pass\" to \
       abstain. Note that this \"option\" is mandatory!"
    ~long:"liquidity-baking-toggle-vote"
    ~placeholder:"vote"
    liquidity_baking_toggle_vote_parameter

let get_delegates (cctxt : Protocol_client_context.full)
    (pkhs : Tezos_crypto.Signature.V0.public_key_hash list) =
  let proj_delegate (alias, public_key_hash, public_key, secret_key_uri) =
    {
      Baking_state.alias = Some alias;
      public_key_hash;
      public_key;
      secret_key_uri;
    }
  in
  (if pkhs = [] then
   Client_keys_v0.get_keys cctxt >>=? fun keys ->
   List.map proj_delegate keys |> return
  else
    List.map_es
      (fun pkh ->
        Client_keys_v0.get_key cctxt pkh >>=? function
        | alias, pk, sk_uri -> return (proj_delegate (alias, pkh, pk, sk_uri)))
      pkhs)
  >>=? fun delegates ->
  Tezos_signer_backends.Encrypted.decrypt_list
    cctxt
    (List.filter_map
       (function
         | {Baking_state.alias = Some alias; _} -> Some alias | _ -> None)
       delegates)
  >>=? fun () ->
  let delegates_no_duplicates = List.sort_uniq compare delegates in
  (if List.compare_lengths delegates delegates_no_duplicates <> 0 then
   cctxt#warning
     "Warning: the list of public key hash aliases contains duplicate hashes, \
      which are ignored"
  else Lwt.return ())
  >>= fun () -> return delegates_no_duplicates

let sources_param =
  Tezos_clic.seq_of_param
    (Client_keys_v0.Public_key_hash.source_param
       ~name:"baker"
       ~desc:
         "name of the delegate owning the endorsement/baking right or name of \
          the consensus key signing on the delegate's behalf")

let delegate_commands () : Protocol_client_context.full Tezos_clic.command list
    =
  let open Tezos_clic in
  let group =
    {name = "delegate.client"; title = "Tenderbake client commands"}
  in
  [
    command
      ~group
      ~desc:"Forge and inject block using the delegates' rights."
      (args8
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_switch
         operations_arg
         context_path_arg
         do_not_monitor_node_mempool_arg)
      (prefixes ["bake"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force,
             extra_operations,
             context_path,
             do_not_monitor_node_mempool )
           pkhs
           cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.bake
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ~force
          ~monitor_node_mempool:(not do_not_monitor_node_mempool)
          ?extra_operations
          ?context_path
          delegates);
    command
      ~group
      ~desc:"Forge and inject an endorsement operation."
      (args1 endorsement_force_switch_arg)
      (prefixes ["endorse"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.endorse ~force cctxt delegates);
    command
      ~group
      ~desc:"Forge and inject a preendorsement operation."
      (args1 endorsement_force_switch_arg)
      (prefixes ["preendorse"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.preendorse ~force cctxt delegates);
    command
      ~group
      ~desc:"Send a Tenderbake proposal"
      (args7
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_switch
         operations_arg
         context_path_arg)
      (prefixes ["propose"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force,
             extra_operations,
             context_path )
           sources
           cctxt ->
        get_delegates cctxt sources >>=? fun delegates ->
        Baking_lib.propose
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ~force
          ?extra_operations
          ?context_path
          delegates);
  ]

let directory_parameter =
  Tezos_clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let per_block_vote_file_arg =
  Tezos_clic.arg
    ~doc:"read per block votes as json file"
    ~short:'V'
    ~long:"votefile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun _ s -> return s))

let baker_commands () : Protocol_client_context.full Tezos_clic.command list =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      (args8
         pidfile_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         keep_alive_arg
         liquidity_baking_toggle_vote_arg
         per_block_vote_file_arg
         operations_arg)
      (prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"node_data_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ sources_param)
      (fun ( pidfile,
             minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             keep_alive,
             liquidity_baking_toggle_vote,
             per_block_vote_file,
             extra_operations )
           node_data_path
           sources
           cctxt ->
        let per_block_vote_file_or_default =
          match per_block_vote_file with
          | None -> Baking_configuration.default_per_block_vote_file
          | Some arg -> arg
        in
        (* We don't let the user run the baker without providing some option (CLI, file path, or file in default location) for the toggle vote. *)
        Liquidity_baking_vote_file.read_liquidity_baking_toggle_vote_on_startup
          ~default:liquidity_baking_toggle_vote
          ~per_block_vote_file:per_block_vote_file_or_default
        >>=? fun (liquidity_baking_toggle_vote, vote_file_present) ->
        let per_block_vote_file =
          match (per_block_vote_file, vote_file_present) with
          | Some _, _ | None, true -> Some per_block_vote_file_or_default
          | None, false -> None
        in
        may_lock_pidfile pidfile @@ fun () ->
        get_delegates cctxt sources >>=? fun delegates ->
        let context_path = Filename.Infix.(node_data_path // "context") in
        Client_daemon.Baker.run
          cctxt
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ~liquidity_baking_toggle_vote
          ?per_block_vote_file
          ?extra_operations
          ~chain:cctxt#chain
          ~context_path
          ~keep_alive
          delegates);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (* no_options *)
      (args1 keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun keep_alive cctxt ->
        Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive);
  ]

let accuser_commands () =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.accuser";
      title = "Commands related to the accuser daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg Client_proto_args.preserved_levels_arg keep_alive_arg)
      (prefixes ["run"] @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.Accuser.run
          cctxt
          ~chain:cctxt#chain
          ~preserved_levels
          ~keep_alive);
  ]
