
Printf.printf "file_exists: %b\n" (Sys.file_exists "typesys.ml");;
Printf.printf "file_exists: %b\n" (Sys.file_exists "apple.ml");;

let show_file_exists file =
  if (Sys.file_exists file)
  then Printf.printf "The file \"%s\" exists."
  else Printf.printf "The file \"%s\" does not exists."
