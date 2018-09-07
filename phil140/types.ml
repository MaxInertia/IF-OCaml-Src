(*
Critical Thinking:
  The systematic evaluation and/or formulation of beliefs,
  statements or arguments by rational standards.

Evaluation:
  assess existing beliefs, statements or arguments

Formulation:
  creating our own beliefs, statements or arguments
*)

(*
type p140_type =
  | Statement of string
  | TruthValue of bool * bool;;
*)

(*
type truth_value =
  | Determined of bool
  | Indeterminate;;
*)

open Printf

type bool3 =
  | True
  | False
  | Indeterminate;;

let t = True;;
let f = False;;
let i = Indeterminate;;

let rec tv_to_string tv =
  match tv with
    | True -> "true"
    | False -> "false"
    | Indeterminate -> "indeterminate";;

printf "Type: bool3\nValues\n -%s\n -%s\n -%s\n"
  (tv_to_string t)
  (tv_to_string f)
  (tv_to_string i);;

(* bool3 operations *)



(* list printer *)
let rec print_list xs =
  if xs = []
  then ()
  else
    let head = List.hd xs
    and tail = List.tl xs in
    printf "   | %s\n" head;
    print_list tail;;

(* list printer with header *)
let print_list_header header xs =
  printf "%s\n" header;
  print_list xs;;

let lst = "True"::"False"::"Indeterminate"::[];;
print_endline "";;
print_list_header "type bool3 =" lst;;

(*
Cognitive Biases: caused by internal psychological forces
Social Biases: caused by external social forces
Worldview Biases: ...
Irrationality: ...
*)
