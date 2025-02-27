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

(** Testing
    -------
    Component:    Protocol
    Invocation:   dune runtest src/proto_alpha/lib_protocol/test/integration/operations
    Subject:      Entrypoint
*)

let () =
  Alcotest_lwt.run
    "protocol > integration > operations"
    [
      ("voting", Test_voting.tests);
      ("origination", Test_origination.tests);
      ("revelation", Test_reveal.tests);
      ("transfer", Test_transfer.tests);
      ("activation", Test_activation.tests);
      ("paid storage increase", Test_paid_storage_increase.tests);
      ("combined", Test_combined_operations.tests);
      ("failing_noop operation", Test_failing_noop.tests);
      ("sc rollup", Test_sc_rollup.tests);
      ("sc rollup transfer", Test_sc_rollup_transfer.tests);
      ("zk rollup", Test_zk_rollup.tests);
      ("transfer ticket", Test_transfer_ticket.tests);
    ]
  |> Lwt_main.run
