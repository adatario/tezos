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

type error +=
  | Cannot_write_page_to_page_storage of {hash : string; content : bytes}
  | Cannot_read_page_from_page_storage of string

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_write_page_to_page_storage"
    ~title:"Cannot write DAC page to page store"
    ~description:"Persisting DAC page with given key and content failed"
    ~pp:(fun ppf (key, content) ->
      Format.fprintf
        ppf
        "Unable to write the following DAC page to the page storage: {key=%s; \
         content=%s}"
        key
        (Bytes.to_string content))
    Data_encoding.(obj2 (req "key" Data_encoding.string) (req "content" bytes))
    (function
      | Cannot_write_page_to_page_storage {hash; content} -> Some (hash, content)
      | _ -> None)
    (fun (hash, content) -> Cannot_write_page_to_page_storage {hash; content}) ;
  register_error_kind
    `Permanent
    ~id:"cannot_read_dac_page_from_page_storage"
    ~title:"Cannot read DAC page from the page storage"
    ~description:"Reading DAC page with given key from the page storage failed"
    ~pp:(fun ppf key ->
      Format.fprintf
        ppf
        "Unable to read DAC page with {key=%s} from the page storage"
        key)
    Data_encoding.(obj1 (req "hash" Data_encoding.string))
    (function
      | Cannot_read_page_from_page_storage hash -> Some hash | _ -> None)
    (fun hash -> Cannot_read_page_from_page_storage hash)

module type S = sig
  type t

  type configuration

  val init : configuration -> t

  val save :
    Dac_plugin.t ->
    t ->
    hash:Dac_plugin.hash ->
    content:bytes ->
    unit tzresult Lwt.t

  val load : Dac_plugin.t -> t -> hash:Dac_plugin.hash -> bytes tzresult Lwt.t
end

(** Implementation of dac pages storage using filesystem. *)
module Filesystem : S with type configuration = string = struct
  (** [t] represents directory path *)
  type t = string

  type configuration = string

  let init data_dir = data_dir

  let path data_dir hash_string_key =
    Filename.(concat data_dir @@ hash_string_key)

  let save ((module P) : Dac_plugin.t) data_dir ~hash ~content =
    let open Lwt_result_syntax in
    let hash_string = P.to_hex hash in
    let path = path data_dir hash_string in
    let*! result =
      Lwt_utils_unix.with_atomic_open_out path @@ fun chan ->
      Lwt_utils_unix.write_bytes chan content
    in
    match result with
    | Ok () -> return ()
    | Error _ ->
        tzfail
        @@ Cannot_write_page_to_page_storage {hash = hash_string; content}

  let load ((module P) : Dac_plugin.t) data_dir ~hash =
    let open Lwt_result_syntax in
    let hash_string = P.to_hex hash in
    let path = path data_dir hash_string in
    Lwt.catch
      (fun () ->
        let*! result = Lwt_utils_unix.read_file path in
        return @@ String.to_bytes result)
      (fun _exn -> tzfail @@ Cannot_read_page_from_page_storage hash_string)
end
