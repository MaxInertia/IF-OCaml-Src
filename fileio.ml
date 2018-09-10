open Printf

module UnixIO = struct
  let write file content =
    let oc = open_out file in
    begin
      fprintf oc "%s\n" "beep";
      close_out oc
    end;;
  let read file = Sys.command (sprintf "cat %s" file)
  let create file = write file "";;
end;;

UnixIO.create "test.txt";;

print_endline "\nShowing File:\n";;

(* Read file and display the first line *)
let file = "fileio.ml";;
let ic = open_in file in
try
  let line = input_line ic in  (* read line from in_channel and discard \n *)
  print_endline line;          (* write the result to stdout *)
  flush stdout;                (* write on the underlying device now *)
  close_in ic                  (* close the input channel *)

with e ->                      (* some unexpected exception occurs *)
  close_in_noerr ic;           (* emergency closing *)
  raise e                      (* exit with error: files are closed but
                                  channels are not flushed *)

(* normal exit: all channels are flushed and closed *)

(*let file = "text.txt";;
let oc = open_out file in
fprintf oc "%s\n" "beep";
close_out oc;;*)

(*
module Win32IO = struct end;;
module Cygwin = struct end;;
*)

(*
let () =
  match os_type with
  | "Unix" ->
  | "Win32" ->
  | "Cygwin" ->
*)
