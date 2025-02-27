(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

module Make (Mapper : sig
  val of_bytes : bytes -> Dac_plugin.hash
end) : Dac_plugin.T = struct
  let to_bytes = Dac_plugin.hash_to_bytes

  let to_reveal_hash dac_hash =
    dac_hash |> to_bytes
    |> Data_encoding.Binary.of_bytes_exn Protocol.Sc_rollup_reveal_hash.encoding

  let of_reveal_hash reveal_hash =
    reveal_hash
    |> Data_encoding.Binary.to_bytes_exn Protocol.Sc_rollup_reveal_hash.encoding
    |> Mapper.of_bytes

  let encoding =
    Data_encoding.conv
      to_reveal_hash
      of_reveal_hash
      Protocol.Sc_rollup_reveal_hash.encoding

  let dac_hash_to_proto_supported_hashes = function
    | Dac_plugin.Blake2B -> Protocol.Sc_rollup_reveal_hash.Blake2B

  let proto_to_dac_hash_supported_hashes = function
    | Protocol.Sc_rollup_reveal_hash.Blake2B -> Dac_plugin.Blake2B

  let hash_string ~(scheme : Dac_plugin.supported_hashes) ?key strings =
    Protocol.Sc_rollup_reveal_hash.hash_string
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)
      ?key
      strings
    |> of_reveal_hash

  let hash_bytes ~(scheme : Dac_plugin.supported_hashes) ?key bytes =
    Protocol.Sc_rollup_reveal_hash.hash_bytes
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)
      ?key
      bytes
    |> of_reveal_hash

  let scheme_of_hash hash =
    to_reveal_hash hash |> Protocol.Sc_rollup_reveal_hash.scheme_of_hash
    |> proto_to_dac_hash_supported_hashes

  let of_hex hex =
    Protocol.Sc_rollup_reveal_hash.of_hex hex |> Option.map of_reveal_hash

  let to_hex hash = to_reveal_hash hash |> Protocol.Sc_rollup_reveal_hash.to_hex

  let size ~scheme =
    Protocol.Sc_rollup_reveal_hash.size
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)

  let hash_rpc_arg =
    let construct = to_hex in
    let destruct hash =
      match of_hex hash with
      | None -> Error "Cannot parse reveal hash"
      | Some reveal_hash -> Ok reveal_hash
    in
    Tezos_rpc.Arg.make
      ~descr:"A reveal hash"
      ~name:"reveal_hash"
      ~destruct
      ~construct
      ()

  module Proto = Registerer.Registered
end

let make_plugin : (bytes -> Dac_plugin.hash) -> (module Dac_plugin.T) =
 fun of_bytes ->
  let module Plugin = Make (struct
    let of_bytes = of_bytes
  end) in
  (module Plugin)

let () = Dac_plugin.register make_plugin
