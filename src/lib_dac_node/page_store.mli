(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4651
   Integrate the pages backend with preimage data manager. *)

type error +=
  | Cannot_write_page_to_page_storage of {hash : string; content : bytes}
  | Cannot_read_page_from_page_storage of string

(** [S] is the module type defining the backend required for
    persisting DAC pages data onto the page storage. *)
module type S = sig
  (** [t] represents an instance of a storage backened. *)
  type t

  (** [configuration] is the type of the configuration which can
      be used to initialize the store.*)
  type configuration

  (** [init configuration] initializes a page store using the information
      provided in [configuration]. *)
  val init : configuration -> t

  (** [save dac_plugin t ~hash ~content] writes [content] of page onto storage
      backend [t] under given [key].

      When writing fails it returns a [Cannot_write_page_to_page_storage]
      error.

      We require [save] to be atomic. *)
  val save :
    Dac_plugin.t ->
    t ->
    hash:Dac_plugin.hash ->
    content:bytes ->
    unit tzresult Lwt.t

  (** [load dac_plugin t ~hash] returns [content] of the storage backend [t]
      represented by a key. When reading fails it returns a
      [Cannot_read_page_from_page_storage] error. *)
  val load : Dac_plugin.t -> t -> hash:Dac_plugin.hash -> bytes tzresult Lwt.t
end

(** [Filesystem] is an implementation of the page store backed up by
    the local filesystem. The configuration is a string denoting the
    path where files will be saved. A page is saved as a file whose
    name is the hex encoded hash of its content. *)
module Filesystem : S with type configuration = string
