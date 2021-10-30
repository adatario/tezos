let fold = ref 0

let fold_tree = ref 0

let fold_value = ref 0

let fold_rootval = ref 0

let report () =
  Printf.eprintf
    "> fold = %d\n\
    \  fold_tree = %#d\n\
    \  fold_value = %#d\n\
    \  fold_rootval = %#d\n\
     %!"
    !fold
    !fold_tree
    !fold_value
    !fold_rootval
