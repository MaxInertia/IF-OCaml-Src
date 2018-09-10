open Printf

(*
module Flag = struct
    type t = { id: string; value: string; }
    let compare = compare
end;; open Flag;;

module FlagsMap =
    Map.Make(Flag);;
*)

module FlagMap =
    Map.Make(String);;

(*FlagsMap.(empty |> add "A" "B");;*)
(*let ref flagmap =
    FlagsMap.empty;;*)

let rec parse_flags ?(index=0) ?(acc=FlagMap.empty) args =
    let candidate = args.(index) in
    if Array.length args > (index + 1) then
        if candidate.[0] = '-' then
            parse_flags
                ~index:(index + 1)
                ~acc:(FlagMap.add candidate args.(index + 1) acc)
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
