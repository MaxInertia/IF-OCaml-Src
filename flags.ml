open Printf

(* module Flag = struct
    type t = { id: string; value: string; }
    let compare = compare
end;; open Flag;; *)

(*type flag =
    | UniFlag of string
    | ArgFlag of string * string;;*)

module Flag = struct
    type t =
        | UniFlag of string
        | ArgFlag of string * string
    let compare x y =
        match (x, y) with
        | (UniFlag a, UniFlag b) ->
            if a = b
            then 0
            else 1
        | (ArgFlag (a,b), ArgFlag (c,d)) ->
            if a = c && b = d
            then 0
            else 1
        | _ -> 1;;
end;;
open Flag;;

module FlagSet =
    Set.Make(Flag);;

let rec parse_flags args =
    parse_loop 0 FlagSet.empty args;
and parse_loop i acc args =
    if Array.length args > i
    then parse_current_arg i acc args
    else acc; (* return accumulator *)
and parse_current_arg i acc args =
    if args.(i).[0] = '-'
    then parse_flag i acc args (* parse this flag *)
    else parse_loop (i + 1) acc args; (* move on to next argument *)
and parse_flag i acc args =
    let has_next = Array.length args > (i + 1) in
    if has_next && args.(i + 1).[0] != '-'
    then parse_loop (i + 1) (
            FlagSet.add (ArgFlag (args.(i), args.(i + 1))) acc
         ) args (* recurse with acc + new flag&value *)
    else parse_loop (i + 1) (
            FlagSet.add (UniFlag args.(i)) acc
         ) args;; (* recure with acc + new flag *)

let flag_to_string flag =
    match flag with
    | UniFlag f -> f
    | ArgFlag (f, a) -> f ^" "^ a;;

(* Testing *)

let xs = [| "b"; "-f"; "true"; "apples"; "--version"; "-c"; "camera!"; |];;

let rec print_flags flags =
    FlagSet.iter (
        fun e -> printf "%s\n" (flag_to_string e)
    ) flags;;

(*let flags = parse_flags xs;;*)

(* ------- *)

let flags = parse_flags Sys.argv;;

if FlagSet.is_empty flags
then
    print_endline "No flags!"
else
    print_endline "flag\tvalue";
    print_flags flags;;
