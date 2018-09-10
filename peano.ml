type peano =
  | Zero
  | Succ of peano;;

let incr = fun p -> Succ p;;

let incr_match = fun p ->
  match p with
  | Zero -> Succ Zero
  | Succ x -> Succ (Succ x);;

let foo () =
  print_endline "foo called!";
  1;;

let add_one x =
  x + 1;;

(foo);;
(foo) + 3;;
add_one ((foo) + 3);;
