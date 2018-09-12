open Printf

module File = struct
  type permissions =
    | None
    | Read
    | Write
    | ReadWrite

  (* Add permission p to p0 *)
  let (++) p0 p =
    match (p0, p) with
    | (Write, Write) -> Write
    | (Read, Read) -> Read
    | (None, x) | (x, None) -> x
    | _ -> ReadWrite

  (* Remove permission p from p0 *)
  let (--) p0 p =
    match (p0, p) with
    | (Write, Write) | (Read, Read) | (_, ReadWrite) -> None
    | (x, _) -> x

  type file = {
      name : string;
      mutable perms : permissions;
      mutable write_channel : out_channel option; (* For writing to *)
      mutable read_channel : in_channel option; (* For reading from *)
  }

  exception ChannelClosed of string

  let get_writeable file =
    match file.write_channel with
    | Some oc -> oc
    | None -> raise (ChannelClosed "File has no output channel")

  let get_readable file =
    match file.read_channel with
    | Some ic -> ic
    | None -> raise (ChannelClosed "File has no input channel")

  let set_readable file value =
    match (file.perms, value) with
    | (None, true) | (Write, true) ->
        file.read_channel <- Some (open_in file.name);
        file.perms <- (file.perms ++ Read);
    | (Read, false) | (ReadWrite, false) ->
        close_in (get_readable file);
        file.read_channel <- None;
        file.perms <- (file.perms -- Read);
    | _ -> ()

    let set_writeable file value =
      match (file.perms, value) with
      | (None, true) | (Read, true) ->
          file.write_channel <- Some (open_out file.name);
          file.perms <- (file.perms ++ Write);
      | (Write, false) | (ReadWrite, false) ->
          close_out (get_writeable file);
          file.write_channel <- None;
          file.perms <- (file.perms -- Write);
      | _ -> ()

  let link file_name = {
      name = file_name;
      perms = None;
      write_channel = None;
      read_channel = None;
  }

  let write (file: file) content =
    match file.perms with
    | None | Read ->
      raise (ChannelClosed "Cannot write to current file")
    | Write | ReadWrite ->
      match file.write_channel with
      | Some oc ->
        fprintf oc "%s\n" "beep";
        close_out oc
      | None -> raise (ChannelClosed "ERROR: This file claimed Write permission but has no out_channel.")

  let create file =
    set_writeable file true;
    write file ""

  let rec close file =
    match file.perms with
    | None -> ()
    | Write -> close_write file
    | Read -> close_read file
    | ReadWrite -> (close_read file; close_write file);
  and close_read file = close_in (get_readable file)
  and close_write file = close_out (get_writeable file)

end;;


let my_file = File.link "test2.txt";;
File.create my_file;;

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
