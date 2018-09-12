open Printf

let mv ?force:(f=false) oldpath newpath =
    let old_exists = Sys.file_exists oldpath
    and new_exists = Sys.file_exists newpath
    and err = Printf.printf "Error: File \"%s\" not found.\n"
    and warn = Printf.printf "Warning: File \"%s\" already exists.\n" in
    match (old_exists, new_exists) with
        | (true,  false) -> Sys.rename oldpath newpath
        | (false, false) -> err oldpath
        | (true,  true)  -> warn newpath
        | (false, true)  -> err newpath; warn oldpath;;

let is_file x = Sys.file_exists x && not (Sys.is_directory x);;
let is_dir x = Sys.file_exists x && Sys.is_directory x;;

let rm file = Sys.remove file;;

let rm_safe (x: string) : int =
    if is_file x
    then (rm x; 0)
    else 1;;

let cd dir =
    match is_dir dir with
        | true -> Sys.chdir dir
        | _    -> Printf.printf "Error: Directory \" %s\" not found" dir;;

let exec program = Sys.command program;;
