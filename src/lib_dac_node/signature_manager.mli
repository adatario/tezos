(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
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

(* Module for managing the verification of aggregate signatures *)
type error +=
  | Cannot_convert_root_page_hash_to_bytes of string
  | Cannot_compute_aggregate_signature of string
  | Public_key_for_witness_not_available of int * string

(* [sign_root_hash dac_pliugin cctx dac_sk_uris_opt root_hash] is legacy function that
   returns an aggregate signature over [root_hash] and a bitmap of witnesses where
   empty elements of [dac_sk_uris_opt] are 0 and non-empty elements are 1. *)
val sign_root_hash :
  Dac_plugin.t ->
  #Client_context.wallet ->
  Client_keys.aggregate_sk_uri option trace ->
  Dac_plugin.hash ->
  (Tezos_crypto.Aggregate_signature.signature * Z.t, tztrace) result Lwt.t

(** [verify dac_plugin public_keys_opt root_hash aggregate_signature witnesses] verifies
    the [aggergate_signature] signed by the witnessed dac members. The witnessed 
    dac members is given by applying the [witnesses] bitmap against [public_keys_opt]
 *)
val verify :
  Dac_plugin.t ->
  public_keys_opt:Tezos_crypto.Aggregate_signature.public_key option trace ->
  Dac_plugin.hash ->
  Tezos_crypto.Aggregate_signature.signature ->
  Z.t ->
  (bool, tztrace) result Lwt.t
