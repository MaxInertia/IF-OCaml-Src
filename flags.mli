(*module type FlagMap : Map = sig
    val find: string -> FlagMap -> string
end;;
val parse_flags: int -> FlagMap -> string array -> FlagMap;;*)

 (*?(i:int) ?(acc: FlagMap) (args: string array) : FlagMap;;*)

module Flag : sig
    type t =
        | UniFlag of string
        | ArgFlag of string * string
    val compare : t -> t -> int
end;;

module type FlagSet =
sig
    type elt = Flag.t
    type t = Set.Make(Flag).t
    val is_empty : t -> bool
    val iter : (elt -> unit) -> t -> unit
end;;
include FlagSet;;

val parse_flags : string array -> FlagSet;;
