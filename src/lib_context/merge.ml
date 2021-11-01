
module Store = Context_impl.Store
module Index = Context_impl.Checks.Index.I

let main path =
  Printf.eprintf "Hello\n%!";
  Printf.eprintf "making index\n%!";
  let v = Index.v ~readonly:false ~fresh:false path ~log_size:2500000 in
  Printf.eprintf "filtering\n%!";
  let () =
    Index.filter v (fun _ -> true)
  in
  Printf.eprintf "Bye\n%!";
  exit 1

let () =
  match Unix.getenv "MERGE_IT" with
  | exception Not_found ->
     Printf.eprintf "not merging\n%!"
  | path -> main path
