(* TODO: Repurpose as test file for bashish.ml *)

print_endline "# show_file_exists";;

let show_file_exists file =
  if (Sys.file_exists file)
  then Printf.printf "File \"%s\" found.\n" file
  else Printf.printf "File \"%s\" not found.\n" file;;
show_file_exists "opchecker.ml";;
show_file_exists "phil140/types.ml";;
show_file_exists "i_dont_exist";;

let dir_exists dir = Sys.file_exists dir && Sys.is_directory dir;;

(* Random utility functions *)

(* list printer *)
let rec print_list xs =
  if xs = []
  then ()
  else
    let head = List.hd xs
    and tail = List.tl xs in
    Printf.printf "   | %s\n" head;
    print_list tail;;

(* list printer with header *)
let print_list_header header xs =
  Printf.printf "%s\n" header;
  print_list xs;;

(* Bashify! *)

print_endline "\n# mv";;

let mv oldpath newpath =
    let old_exists = Sys.file_exists oldpath
    and new_exists = Sys.file_exists newpath
    and err = Printf.printf "Error: File \"%s\" not found.\n"
    and warn = Printf.printf "Warning: File \"%s\" already exists.\n" in
    match (old_exists, new_exists) with
        | (true,  false) -> Sys.rename oldpath newpath
        | (false, false) -> err oldpath
        | (true,  true)  -> warn newpath
        | (false, true)  -> err newpath; warn oldpath;;

mv "test.txt" "phil140/test.txt";;

print_endline "\n# cd";;

let cd path =
    match (Sys.file_exists path) && (Sys.is_directory path) with
        | true -> Sys.chdir path; Printf.printf "%s: ...\n" (Sys.getcwd ())
        | _    -> Printf.printf "Invalid directory: %s" path;;

cd ".."; cd "..";;

print_endline "\n# ls";;

let ls () =
    if dir_exists "/"
    then print_list (Array.to_list (Sys.readdir "."))
    else print_endline "Error: Directory does not exist\n";;
