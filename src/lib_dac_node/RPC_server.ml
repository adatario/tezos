(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold  <contact@tmarigold.dev>                      *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server

type error +=
  | Cannot_construct_external_message
  | Cannot_deserialize_external_message

let () =
  register_error_kind
    `Permanent
    ~id:"dac_cannot_construct_external_message"
    ~title:"External rollup message could not be constructed"
    ~description:"External rollup message could not be constructed"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "External rollup message could not be constructed")
    Data_encoding.unit
    (function Cannot_construct_external_message -> Some () | _ -> None)
    (fun () -> Cannot_construct_external_message) ;
  register_error_kind
    `Permanent
    ~id:"dac_cannot_deserialize_rollup_external_message"
    ~title:"External rollup message could not be deserialized"
    ~description:"External rollup message could not be deserialized"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "External rollup message could not be deserialized")
    Data_encoding.unit
    (function Cannot_deserialize_external_message -> Some () | _ -> None)
    (fun () -> Cannot_deserialize_external_message)

let add_service registerer service handler directory =
  registerer directory service handler

let handle_serialize_dac_store_preimage dac_plugin cctxt dac_sk_uris page_store
    (data, pagination_scheme) =
  let open Lwt_result_syntax in
  let open Pages_encoding in
  let* root_hash =
    match pagination_scheme with
    | Merkle_tree_V0 ->
        Merkle_tree.V0.serialize_payload dac_plugin ~page_store data
    | Hash_chain_V0 ->
        Hash_chain.V0.serialize_payload
          dac_plugin
          ~for_each_page:(fun (hash, content) ->
            Page_store.Filesystem.save dac_plugin page_store ~hash ~content)
          data
  in
  let* signature, witnesses =
    Signature_manager.sign_root_hash dac_plugin cctxt dac_sk_uris root_hash
  in
  let*! external_message =
    External_message.Default.make dac_plugin root_hash signature witnesses
  in
  match external_message with
  | Ok external_message -> return @@ (root_hash, external_message)
  | Error _ -> tzfail @@ Cannot_construct_external_message

let handle_verify_external_message_signature dac_plugin public_keys_opt
    encoded_l1_message =
  let open Lwt_result_syntax in
  let external_message =
    let open Option_syntax in
    let* encoded_l1_message in
    let* as_bytes = Hex.to_bytes @@ `Hex encoded_l1_message in
    let ((module P) : Dac_plugin.t) = dac_plugin in
    External_message.Default.of_bytes P.encoding as_bytes
  in
  match external_message with
  | None -> tzfail @@ Cannot_deserialize_external_message
  | Some {root_hash; signature; witnesses} ->
      Signature_manager.verify
        dac_plugin
        ~public_keys_opt
        root_hash
        signature
        witnesses

let handle_retrieve_preimage dac_plugin page_store hash =
  Page_store.Filesystem.load dac_plugin page_store ~hash

let register_serialize_dac_store_preimage ctx cctxt dac_sk_uris page_store
    directory =
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       (RPC_services.dac_store_preimage ctx)
       (fun () input ->
         handle_serialize_dac_store_preimage
           ctx
           cctxt
           dac_sk_uris
           page_store
           input)

let register_verify_external_message_signature ctx public_keys_opt directory =
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       RPC_services.verify_external_message_signature
       (fun external_message () ->
         handle_verify_external_message_signature
           ctx
           public_keys_opt
           external_message)

let register_retrieve_preimage dac_plugin page_store =
  add_service
    Tezos_rpc.Directory.register1
    (RPC_services.retrieve_preimage dac_plugin)
    (fun hash () () -> handle_retrieve_preimage dac_plugin page_store hash)

let register dac_plugin reveal_data_dir cctxt dac_public_keys_opt dac_sk_uris =
  let page_store = Page_store.Filesystem.init reveal_data_dir in
  Tezos_rpc.Directory.empty
  |> register_serialize_dac_store_preimage
       dac_plugin
       cctxt
       dac_sk_uris
       page_store
  |> register_verify_external_message_signature dac_plugin dac_public_keys_opt
  |> register_retrieve_preimage dac_plugin page_store

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4750
   Move this to RPC_server.Legacy once all operating modes are supported. *)
let start_legacy ~rpc_address ~rpc_port ~reveal_data_dir ~threshold cctxt ctxt
    dac_pks_opt dac_sk_uris =
  let open Lwt_syntax in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory
      Tezos_rpc.Directory.empty
      Tezos_rpc.Path.open_root
      (fun () ->
        match Node_context.get_status ctxt with
        | Ready {dac_plugin = (module Dac_plugin); _} ->
            let _threshold = threshold in
            Lwt.return
              (register
                 (module Dac_plugin)
                 reveal_data_dir
                 cctxt
                 dac_pks_opt
                 dac_sk_uris)
        | Starting -> Lwt.return Tezos_rpc.Directory.empty)
  in
  let rpc_address = P2p_addr.of_string_exn rpc_address in
  let host = Ipaddr.V6.to_string rpc_address in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default rpc_address in
  let server =
    RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
  in
  Lwt.catch
    (fun () ->
      let* () =
        RPC_server.launch
          ~host
          server
          ~callback:(RPC_server.resto_callback server)
          node
      in
      return_ok server)
    fail_with_exn

let shutdown = RPC_server.shutdown

let install_finalizer rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = shutdown rpc_server in
  let* () = Event.(emit shutdown_node exit_status) in
  Tezos_base_unix.Internal_event_unix.close ()
