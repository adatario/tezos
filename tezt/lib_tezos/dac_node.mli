(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** Spawn Data-availability-committee (DAC) nodes and control them. *)

(** DAC Node state *)
type t

(** Creates a DAC node *)

val create :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  node:Node.t ->
  client:Client.t ->
  unit ->
  t

(** Get the name of an dac node. *)
val name : t -> string

(** Get the RPC host given as [--rpc-addr] to an dac node. *)
val rpc_host : t -> string

(** Get the RPC port given as [--rpc-addr] to an dac node. *)
val rpc_port : t -> int

(** Return the endpoint of the dac node, i.e., http://rpc_host:rpc_port. *)
val endpoint : t -> string

(** Get the data-dir of an dac node. *)
val data_dir : t -> string

(** [run ?wait_ready ?env node] launches the given dac
    node where env is a map of environment variable.

    If [wait_ready] is [true], the promise waits for the dac node to be ready.
    [true] by default.
*)
val run : ?wait_ready:bool -> ?env:string String_map.t -> t -> unit Lwt.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Shows in stdout every events sent by the node *)
val log_events : t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** [is_running_not_ready dac_node] returns true if the given node is
    running but its status is not ready *)
val is_running_not_ready : t -> bool

(** Wait until a node terminates and return its status. If the node is not
    running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

(** Run [octez-dac-node init-config]. Returns the name of the resulting
    configuration file.
*)
val init_config : t -> string Lwt.t

(** DAC related functions. *)
module Dac : sig
  (** [set_parameters ?threshold dac_node] Runs
    [octez-dac-node set dac parameters --data-dir data_dir], where
    [data_dir = dac_node.persistent_state.data_dir]. If the optional integer
    parameter [~threshold] is passed, then the dac node configuration file is
    updated with the dac threshold indicated. If the [~reveal_data_dir]
    optional argument is passed, then the dac node configuration file is
    updated with the corresponding reveal_data_dir. If no optional arguments are
    passed, the configuration file of the dac node is left unchanged.
*)
  val set_parameters :
    ?threshold:int -> ?reveal_data_dir:string -> t -> unit Lwt.t

  (** [add_committee_member dac_node] runs
    [octez-dac-node add data availability committee member alias --data-dir data-dir],
    where [data-dir = dac_node.persistent_state.data_dir]. *)
  val add_committee_member : address:string -> t -> unit Lwt.t
end

module Config_file : sig
  (** C node configuration files. *)

  (** Read the configuration file ([config.json]) of a DAC node. *)
  val read : t -> JSON.t

  (** Write the configuration file of a DAC node, replacing the existing one. *)
  val write : t -> JSON.t -> unit

  (** Update the configuration file of a DAC node. If the DAC node is already
      running, it needs to be restarted manually. *)
  val update : t -> (JSON.t -> JSON.t) -> unit
end
