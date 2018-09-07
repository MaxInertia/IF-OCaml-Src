open Printf

(* Store arguments in a list. Why? For fun *)
let args = List.tl (Array.to_list Sys.argv);;

(*
Terminology:

    order-pair (For lack of better word) is a pair
    of characters (fst, snd) whose appearance in
    a string adheres to the following conditions:

      1. On scanning from left to right, the first of
         the pair appears at least once for each of the second.

      2. The number of appearances of each is equal.
*)

(*
    Helper function for ordered_pair_balanced.

    Arguments:
        str: String being checked
        ind: index of current character being evaluated
        counter: order-pair counter
        fst: first char of order-pair
        snd: second char of order-pair

    returns an int 'n':
        - n>0 if there are more opens '('.
        - n<0 if there are more closes ')', or one appears before it's open.
        - n==0 if all parentheses can be paired in correct order.
*)
let rec ordered_pair_checker str i counter fst snd =
    if (String.length str) <= i || counter < 0 then counter
    else
        let c = str.[i]
        and i = i + 1 in
        (*printf "%i\n" counter;*)
        if c = fst then ordered_pair_checker str i (counter + 1) fst snd
        else
            if c = snd then ordered_pair_checker str i (counter - 1) fst snd
            else ordered_pair_checker str i counter fst snd;;

(* Returns true if input has balanced order-pairs, else false *)
let ordered_pair_balanced (* ... as all things should be*) str fst snd =
    if (ordered_pair_checker str 0 0 fst snd) == 0 then true
    else false;;

(* Iterates over each argument supplied to the program *)
let rec loop fst snd xs =
    if xs = [] then () (* Return nothing *)
    else (* Display arg and recurse *)
        let str = sprintf "%s" (List.hd xs) in
        let balanced = ordered_pair_balanced str fst snd in
        if balanced then printf "\"%s\" is balanced!\n" str
        else printf "\"%s\" is NOT balanced!\n" str;
        loop fst snd (List.tl xs);; (* Move on to the next argument provided *)

(* -- Testing -- *)

let rec tests cases expected =
    if List.length cases = 0 then true
    else
        let result = ordered_pair_balanced (List.hd cases) '(' ')' in
        (*printf "\"%s\" : %b\n" (List.hd cases) result;*)
        assert (result = expected);
        if (result != expected) then false
        else tests (List.tl cases) expected;;

let cases_balanced =
    "()"::
    "(())"::
    "()()"::
    "((()))"::
    "(xyz)"::
    "(        )"::
    "( c s   y   () ...)"::
    "(abcdefghijklmnopqrstuvwxyz!@#$%^&*()_+=-0987654321)"::
    "(\t\n\t)"::[];;

let cases_unbalanced =
    "("::
    ")"::
    "(()"::
    "())"::
    "(((("::
    ")))))"::
    "(())())"::
    ")("::
    ")"::
    "\t)\n"::
    "(\t\t("::[];;

let run_tests () =
    let tcs1 = tests cases_balanced true in
    let tcs1name = "Balanced Test Cases" in
    if tcs1 then printf "-- %s - PASSED\n" tcs1name
    else printf "-- %s - FAILED\n" tcs1name;

    let tcs2 = tests cases_unbalanced false in
    let tcs2name = "Unbalanced Test Cases" in
    if tcs2 then
        printf "-- %s - PASSED\n" tcs2name
    else
        printf "-- %s - FAILED\n" tcs2name;;

(* -- Main -- *)

let arg1 = List.hd args;;
let listargs = Array.of_list args;;
let arg_count = List.length args;;
let testrun = arg1 = "-test";;

let custom_pair_check () =
    let fst = listargs.(0).[0] in
    let snd = listargs.(1).[0] in
    printf ("Using fst: %c and snd: %c\n") fst snd;
    loop fst snd (List.tl (List.tl args));;

let rec main () =
    match arg_count with
        | 1 ->
            if testrun
            then run_tests ()
            else loop '(' ')' args
        | 3 -> custom_pair_check () (* Can `let .. in ...` go in a pattern match result?*)
        | _ -> ();;

main ()
