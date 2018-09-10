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

(* FlagsMap.(empty |> add "A" "B");; *)

(* Try continuation passing here? *)
let rec parse_flags ?(index=0) ?(acc=FlagMap.empty) args =
    let candidate = args.(index) in
    if Array.length args > (index + 1) then
        if candidate.[0] = '-' then
            if args.(index + 1).[0] = '-' then
                let flag = UniFlag candidate in
                parse_flags
                    ~index:(index + 1)
                    ~acc:(FlagMap.add flag acc)
                    args
            else
                let flag = ArgFlag (candidate, args.(index+1)) in
                parse_flags
                    ~index:(index + 1)
                    ~acc:(FlagMap.add flag acc)
                    args
        else parse_flags
                ~index:(index + 1)
                ~acc:acc
                args
    else acc;;

let xs = [| "a"; "b"; "-f"; "true"; "apples"; "-c"; "camera!"; |];;

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
