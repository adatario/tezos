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

type t = {
  verbosity : [`Default | `Info | `Debug];
  index_log_size : int option;
  auto_flush : int;
}
(* This limit ensures that no trees with more than [auto_flush]
   mutations can exist in memory, bounding the memory usage of a
   single commit performed by a read-write process. As a trade-off,
   the intermediate flushed trees to the store might be unused and
   will have to be garbage collected later on to save space. *)

let default =
  {verbosity = `Default; index_log_size = None; auto_flush = 10000}

let max_verbosity a b =
  match (a, b) with
  | (`Debug, _) | (_, `Debug) -> `Debug
  | (`Info, _) | (_, `Info) -> `Info
  | _ -> `Default

let v =
  match Unix.getenv "TEZOS_CONTEXT" with
  | exception Not_found -> default
  | v ->
      List.fold_left
        (fun acc s ->
          match String.trim s with
          | "v" | "verbose" ->
              {acc with verbosity = max_verbosity acc.verbosity `Info}
          | "vv" -> {acc with verbosity = `Debug}
          | v -> (
              match String.split '=' v |> List.map String.trim with
              | ["index-log-size"; n] ->
                  {acc with index_log_size = int_of_string_opt n}
              | ["auto-flush"; n] ->
                  {acc with index_log_size = int_of_string_opt n}
              | _ -> acc))
        default
        (String.split ',' v)
