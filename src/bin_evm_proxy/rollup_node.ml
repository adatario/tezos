(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module RPC = struct
  open Tezos_rpc
  open Path

  let smart_rollup_address =
    Service.get_service
      ~description:"Smart rollup address"
      ~query:Query.empty
      ~output:Data_encoding.string
      (open_root / "global" / "smart_rollup_address")

  let call_service ~base =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_service
      Media_type.all_media_types
      ~base

  (** We use [rpc get /global/smart_rollup_address] to check if the rollup-node
      answers. *)
  let is_connected base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base smart_rollup_address () () () in
    match answer with
    | Ok _ -> return ()
    | Error tztrace ->
        failwith
          "Failed to communicate with %a, because %a"
          Uri.pp
          base
          pp_print_trace
          tztrace
end

module type S = sig
  val assert_connected : unit tzresult Lwt.t
end

module Make (Base : sig
  val base : Uri.t
end) =
struct
  let assert_connected = RPC.is_connected Base.base
end
