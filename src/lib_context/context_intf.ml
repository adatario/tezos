(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

open Tezos_context_encoding.Context

module type STORE = sig
  include
    Irmin.S
      with type key = Path.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type Private.Sync.endpoint = unit

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error],
      [> `Cannot_fix of string | `Corrupted of int] )
    result

  val sync : repo -> unit

  (* val clear : repo -> unit Lwt.t *)
  (* val reconstruct_index : ?output:string -> Irmin.config -> unit *)
  (* val migrate : Irmin.config -> unit *)
  (* val flush : repo -> unit *)
  val create_repo : readonly:bool -> string -> repo Lwt.t
end

module type Context = sig
  type error +=
    | Cannot_create_file of string
    | Cannot_open_file of string
    | Cannot_find_protocol
    | Suspicious_file of int

  module Make (Store : STORE) : Tezos_context_sigs.Context.S
  (* with type Private.store_tree = Store.tree *)

  include Tezos_context_sigs.Context.S
end
