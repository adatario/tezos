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

(** [Protocol.Sc_protocol_reveal_hash.t] is unknown to  modules outside the
    protocol and only known at runtime. To avoid the proliferation of functors
    in the dac node, [hash] hides the dynamic [Protocol.Sc_protocol_reveal_hash.t]
    behind an abstract static type. An instance of [Dac_plugin.T] behaviour
    of operations on [hash].
  *)

type hash

val hash_to_bytes : hash -> bytes

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/4856
    Fix static supported_hashes type *)
type supported_hashes = Blake2B

module type T = sig
  (** The encoding of reveal hashes. *)
  val encoding : hash Data_encoding.t

  (** [hash_string ~scheme ?key strings] hashes [strings] using the
    supported hashing [scheme] given in input. *)
  val hash_string :
    scheme:supported_hashes -> ?key:string -> string list -> hash

  (** [hash_bytes ~scheme ?key strings] hashes [bytes] using the
      supported hashing [scheme] given in input. *)
  val hash_bytes : scheme:supported_hashes -> ?key:bytes -> bytes list -> hash

  (** [scheme_of_hash] hash returns the supported hashing scheme
      that was used to obtain [hash]. *)
  val scheme_of_hash : hash -> supported_hashes

  (** [of_hex hex] decodes a hex into hash. *)
  val of_hex : string -> hash option

  (** [to_hex hash] encodes hash into hex. *)
  val to_hex : hash -> string

  (** [size ~scheme] returns the size of reveal hashes using the [scheme]
      specified in input. *)
  val size : scheme:supported_hashes -> int

  (** Hash argument definition for RPC *)
  val hash_rpc_arg : hash Tezos_rpc.Arg.arg

  module Proto : Registered_protocol.T
end

(** Dac plugin module type *)
type t = (module T)

(** [register make_plugin] derives and registers a new [Dac_plugin.T] given an
    [of_bytes]. Implementers of plugin are responsible for providing the
    definition of this derivation. Functions that expose
    [Protocol.Sc_protocol_reveal_hash.t] can be wrapped into [hash] via
    [Dac_hash.to_bytes] and [of_bytes].
*)
val register : ((bytes -> hash) -> (module T)) -> unit

val get : Protocol_hash.Table.key -> (module T) option
