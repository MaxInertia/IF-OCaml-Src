open Printf
open Bashish

module Bsh = Bashish;;
(* -- A very simple build manager for OCaml -- *)

(*
 * Modules to be compiled and linked.
 * Add module names here!
 * TODO: Add flag to append/remove or override this?
 *)
let local_modules = [
    "bashish";
    "ocbuild";
];;

let build_dir = "build";;
let executable_name = "app";; (* TODO: Handle case where file already exists *)
let verbose = false;; (* Set this with a flag *)

type strings = string list;;

let append_to_each (xs: strings) (suffix: string) =
    List.map (fun x -> x ^ suffix) xs;;

(*
 * Module src filenames
 *)
let files_ml = append_to_each local_modules ".ml";;

(*
 * Compiled (but not linked) module filenames
 *)
let files_cmx = append_to_each local_modules ".cmx";;
let files_cmi = append_to_each local_modules ".cmi";;
let files_o = append_to_each local_modules ".o";;
let rec merge_all_lists xss =
    let ll_zero x y = 0 in
    match xss with
        | [] -> []
        | xs :: [] -> xs
        | xs :: ys :: rem -> merge_all_lists (
            (List.merge (ll_zero) xs ys) :: rem);;
let intermediate_build_files = merge_all_lists [files_o; files_cmi; files_cmx];;

(*
 * Single string containing the compiled
 * module filenames separated by spaces.
 *)
let cmx_string = String.concat " " files_cmx;;
let ml_string = String.concat " " files_ml;;

(* - Components of the compilation pipeline - *)

let compile src = Bsh.exec (sprintf "ocamlopt -c %s " src);;
let link binary = Bsh.exec (sprintf "ocamlopt -o %s %s" executable_name binary);;

(*
 * Delete intermediate build files (.cmx and .cmi)
 *)
let clean files = Bsh.rm build_dir;;
    (*List.iter (fun file ->
        match (verbose, Bsh.rm_safe (build_dir ^"/"^ file)) with
            | (false, _) -> ();
            | (true, 1) -> printf "File not found: %s\n" file;
            | (true, 0) -> printf "rm %s\n" file
            | _ -> print_endline "unknown issue in isle 12 (ocbuild.clean)\n"
    ) files;;*)

(* TODO: Parse flags from Sys.argv *)

let rec compile_and_link files =
    if List.length files = 0
    then print_endline "Error: The list of files to compile is empty."
    else compile_loop files
and compile_loop files =
    ignore (compile (List.hd files));
    if List.length files > 1
    then ignore (compile_loop (List.tl files))
    else ( link_all (); )
and link_all () =
    ignore (link cmx_string);
    Bsh.rm "build";
    organize_files intermediate_build_files
and organize_files files =
    if List.length files > 1
    then
        let file = List.hd files in
        Bsh.mv file (build_dir ^"/"^ file);
        organize_files (List.tl files);;


(* - Main - *)
let rec main () =
    let warning = printf "Warning: OCBuild has not been tested on %s" Sys.os_type in
    match Sys.os_type with
    | "Unix" | "Cygwin" | "MacOS" -> warning
    | "Win32" -> ()
    | _ -> print_endline "Unknown system type";
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "clean"
    then clean intermediate_build_files
    else compile_and_link files_ml;;

main ()
