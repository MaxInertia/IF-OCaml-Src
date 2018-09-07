open Printf

let args = List.tl (Array.to_list Sys.argv);;

(*
    Helper function for parentheses_balanced.

    Note:
        Due to laziness return case 2, this is not strictly a parentheses counter.

    returns an int 'n':
        - n>0 if there are more opens '('.
        - n<0 if there are more closes ')', or one appears before it's open.
        - n==0 if all parentheses can be paired in correct order.
*)
let rec paren_checker xs i n =
    if (String.length xs) <= i || n < 0 then n
    else
        let c = xs.[i]
        and i = i+1 in
        (*printf "%i\n" n;*)
        if c == '(' then paren_checker xs i (n+1)
        else
            if c == ')' then paren_checker xs i (n-1)
            else paren_checker xs i n

(* Returns true if input has balanced parentheses, else false *)
let parentheses_balanced (* ... as all things should be*) str =
    if (paren_checker str 0 0) == 0 then true
    else false

(* Iterates over each argument supplied to the program *)
let rec loop xs =
    if xs == [] then () (* Return nothing *)
    else (* Display arg and recurse *)
        let str = sprintf "%s" (List.hd xs) in
        let balanced = parentheses_balanced str in
        if balanced then printf "%s is balanced!" str
        else printf "%s is not balanced" str;
        loop (List.tl xs);; (* Move on to the next argument provided *)

(* -- Testing -- *)

let rec tests cases expected =
    if List.length cases == 0 then ()
    else
        let result = parentheses_balanced (List.hd cases) in
        printf "%s : %b\n" (List.hd cases) result;
        assert (result == expected);
        tests (List.tl cases) expected;;

let cases_balanced =
    "()"::
    "(())"::
    "()()"::
    "((()))"::[];;

let cases_unbalanced =
    "("::
    ")"::
    "(()"::
    "())"::
    "(((("::
    ")))))"::
    "(())())"::
    ")("::
    ")"::[];;

let run_tests () =
    tests cases_balanced true;
    tests cases_unbalanced false;;

(* -- Main -- *)

let () =
    if (List.length args) == 1 && (List.hd args) == "-test" then
        run_tests ()
    else
        loop args;;
