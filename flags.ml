open Printf

(* module Flag = struct
    type t = { id: string; value: string; }
    let compare = compare
end;; open Flag;; *)

(*type flag =
    | UniFlag of string
    | ArgFlag of string * string;;*)

module Flag : sig
    type t =
        | UniFlag of string
        | ArgFlag of string * string
    val compare : t -> t -> int
end = struct
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

module FlagMap =
    Map.Make(Flag);;

let empty_flag_map = FlagMap.empty;;

let uni_flag = UniFlag "-a";;
let uni_flag2 = UniFlag Sys.argv.(0);;
let arg_flag = ArgFlag ("-a", "true");;

let map_with_stuff = FlagMap.add uni_flag uni_flag empty_flag_map;;

let rec start_parsing args =
    parse_fs 0 FlagMap.empty args;
and parse_fs i acc args =
    let candidate = args.(i) in
    if Array.length args > (i + 1)
    then parse_current_arg i acc args
    else acc;
and parse_current_arg i acc args =
    if args.(i).[0] = '-'
    then parse_flag i acc args
    else parse_fs (i + 1) acc args;
and parse_flag i acc args =
    if args.(i + 1).[0] = '-'
    then parse_fs (i + 1) (
            FlagMap.add (*args.(i)*) uni_flag (ArgFlag (args.(i), args.(i + 1))) acc
         ) args
    else parse_fs (i + 1) (
            FlagMap.add (*args.(i)*) uni_flag (UniFlag args.(i)) acc
         ) args;;

let xs = [| "a"; "b"; "-f"; "true"; "apples"; "-c"; "camera!"; |];;

start_parsing xs;;

(*
let rec print_flags flags =
    FlagMap.iter (
        fun k v -> printf "(%s:\t%s)\n" k v
    ) flags;;

let fas = parse_flags Sys.argv;;
if FlagMap.is_empty fas = true
then
    print_endline "No flags!"
else
    print_endline "flag\tvalue";
    print_flags fas;;
*)
