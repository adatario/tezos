(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Operations_source = struct
  type t =
    | Local of {filename : string}
    | Remote of {uri : Uri.t; http_headers : (string * string) list option}

  let pp ppf = function
    | Local {filename} -> Format.pp_print_string ppf filename
    | Remote {uri; _} -> Format.fprintf ppf "%a" Uri.pp uri

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 1)
          ~title:"Local"
          (obj2 (req "filename" string) (req "kind" (constant "Local")))
          (function Local {filename} -> Some (filename, ()) | _ -> None)
          (fun (filename, ()) -> Local {filename});
        case
          (Tag 2)
          ~title:"Remote"
          (obj3
             (req "uri" string)
             (opt "http_headers" (list (tup2 string string)))
             (req "kind" (constant "Remote")))
          (function
            | Remote {uri; http_headers} ->
                Some (Uri.to_string uri, http_headers, ())
            | _ -> None)
          (fun (uri_str, http_headers, ()) ->
            Remote {uri = Uri.of_string uri_str; http_headers});
      ]
end

open Protocol.Alpha_context

type fees_config = {
  minimal_fees : Tez.t;
  minimal_nanotez_per_gas_unit : Q.t;
  minimal_nanotez_per_byte : Q.t;
}

type validation_config =
  | Local of {context_path : string}
  | Node
  | ContextIndex of Abstract_context_index.t

type nonce_config = Deterministic | Random

type state_recorder_config = Filesystem | Disabled

type t = {
  fees : fees_config;
  nonce : nonce_config;
  validation : validation_config;
  retries_on_failure : int;
  user_activated_upgrades : (int32 * Protocol_hash.t) list;
  liquidity_baking_toggle_vote :
    Protocol.Alpha_context.Liquidity_baking.liquidity_baking_toggle_vote;
  per_block_vote_file : string option;
  force_apply : bool;
  force : bool;
  state_recorder : state_recorder_config;
  extra_operations : Operations_source.t option;
}

let default_fees_config =
  {
    minimal_fees =
      (match Tez.of_mutez 100L with None -> assert false | Some t -> t);
    minimal_nanotez_per_gas_unit = Q.of_int 100;
    minimal_nanotez_per_byte = Q.of_int 1000;
  }

let default_validation_config = Node

(* Unclear if determinist nonces, and more importantly, if
   [supports_deterministic_nonces] is supported. *)
let default_nonce_config = Random

let default_retries_on_failure_config = 5

let default_user_activated_upgrades = []

let default_liquidity_baking_toggle_vote =
  Protocol.Alpha_context.Liquidity_baking.LB_pass

let default_force = false

let default_force_apply = false

let default_state_recorder_config = Filesystem

let default_extra_operations = None

let default_per_block_vote_file = "per_block_votes.json"

let default_config =
  {
    fees = default_fees_config;
    nonce = default_nonce_config;
    validation = default_validation_config;
    retries_on_failure = default_retries_on_failure_config;
    user_activated_upgrades = default_user_activated_upgrades;
    liquidity_baking_toggle_vote = default_liquidity_baking_toggle_vote;
    force_apply = default_force_apply;
    force = default_force;
    state_recorder = default_state_recorder_config;
    extra_operations = default_extra_operations;
    per_block_vote_file = None;
  }

let make ?(minimal_fees = default_fees_config.minimal_fees)
    ?(minimal_nanotez_per_gas_unit =
      default_fees_config.minimal_nanotez_per_gas_unit)
    ?(minimal_nanotez_per_byte = default_fees_config.minimal_nanotez_per_byte)
    ?(nonce = default_nonce_config) ?context_path
    ?(retries_on_failure = default_retries_on_failure_config)
    ?(user_activated_upgrades = default_user_activated_upgrades)
    ?(liquidity_baking_toggle_vote = default_liquidity_baking_toggle_vote)
    ?per_block_vote_file ?(force_apply = default_force_apply)
    ?(force = default_force) ?(state_recorder = default_state_recorder_config)
    ?extra_operations () =
  let fees =
    {minimal_fees; minimal_nanotez_per_gas_unit; minimal_nanotez_per_byte}
  in
  let validation =
    match context_path with
    | None -> Node
    | Some context_path -> Local {context_path}
  in
  {
    fees;
    validation;
    nonce;
    retries_on_failure;
    user_activated_upgrades;
    liquidity_baking_toggle_vote;
    per_block_vote_file;
    force_apply;
    force;
    state_recorder;
    extra_operations;
  }

let fees_config_encoding : fees_config Data_encoding.t =
  let open Data_encoding in
  let q_encoding =
    conv (fun q -> Q.to_string q) (fun s -> Q.of_string s) string
  in
  conv
    (fun {minimal_fees; minimal_nanotez_per_gas_unit; minimal_nanotez_per_byte} ->
      (minimal_fees, minimal_nanotez_per_gas_unit, minimal_nanotez_per_byte))
    (fun (minimal_fees, minimal_nanotez_per_gas_unit, minimal_nanotez_per_byte) ->
      {minimal_fees; minimal_nanotez_per_gas_unit; minimal_nanotez_per_byte})
    (obj3
       (req "minimal_fees" Tez.encoding)
       (req "minimal_nanotez_per_gas_unit" q_encoding)
       (req "minimal_nanotez_per_byte" q_encoding))

let validation_config_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Local"
        (Tag 0)
        (obj1 (req "local" string))
        (function Local {context_path} -> Some context_path | _ -> None)
        (fun context_path -> Local {context_path});
      case
        ~title:"Node"
        (Tag 1)
        (constant "node")
        (function Node -> Some () | _ -> None)
        (fun () -> Node);
    ]

let nonce_config_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Deterministic"
        (Tag 0)
        (constant "deterministic")
        (function Deterministic -> Some () | _ -> None)
        (fun () -> Deterministic);
      case
        ~title:"Random"
        (Tag 1)
        (constant "Random")
        (function Random -> Some () | _ -> None)
        (fun () -> Random);
    ]

let retries_on_failure_config_encoding = Data_encoding.int31

let user_activate_upgrades_config_encoding =
  let open Data_encoding in
  list (tup2 int32 Protocol_hash.encoding)

let liquidity_baking_toggle_vote_config_encoding =
  Protocol.Alpha_context.Liquidity_baking.liquidity_baking_toggle_vote_encoding

let force_config_encoding = Data_encoding.bool

let force_apply_config_encoding = Data_encoding.bool

let state_recorder_config_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Filesystem"
        (Tag 0)
        (constant "filesystem")
        (function Filesystem -> Some () | _ -> None)
        (fun () -> Filesystem);
      case
        ~title:"Disabled"
        (Tag 1)
        (constant "disabled")
        (function Disabled -> Some () | _ -> None)
        (fun () -> Disabled);
    ]

let encoding : t Data_encoding.t =
  let open Data_encoding in
  def
    (String.concat "." [Protocol.name; "baking_configuration"])
    ~title:"Baking configuration"
    ~description:"Baking configuration"
  @@ conv
       (fun c ->
         ( ( c.fees,
             c.validation,
             c.nonce,
             c.retries_on_failure,
             c.user_activated_upgrades,
             c.liquidity_baking_toggle_vote,
             c.per_block_vote_file,
             c.force_apply,
             c.force,
             c.state_recorder ),
           c.extra_operations ))
       (fun ( ( fees,
                validation,
                nonce,
                retries_on_failure,
                user_activated_upgrades,
                liquidity_baking_toggle_vote,
                per_block_vote_file,
                force_apply,
                force,
                state_recorder ),
              extra_operations ) ->
         {
           fees;
           validation;
           nonce;
           retries_on_failure;
           user_activated_upgrades;
           liquidity_baking_toggle_vote;
           per_block_vote_file;
           force_apply;
           force;
           state_recorder;
           extra_operations;
         })
       (merge_objs
          (obj10
             (req "fees" fees_config_encoding)
             (req "validation" validation_config_encoding)
             (req "nonce" nonce_config_encoding)
             (req "retries_on_failure" retries_on_failure_config_encoding)
             (req
                "user_activated_upgrades"
                user_activate_upgrades_config_encoding)
             (req
                "liquidity_baking_toggle_vote"
                liquidity_baking_toggle_vote_config_encoding)
             (opt "per_block_vote_file" Data_encoding.string)
             (req "force_apply" force_apply_config_encoding)
             (req "force" force_config_encoding)
             (req "state_recorder" state_recorder_config_encoding))
          (obj1 (opt "extra_operations" Operations_source.encoding)))

let pp fmt t =
  let json = Data_encoding.Json.construct encoding t in
  Format.fprintf fmt "%a" Data_encoding.Json.pp json
