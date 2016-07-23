open Printf
open Objects

(* Turn string into list of chars *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let interpret_char c =
  printf "%c" c

(* Iterate of a line and interpret each char *)
let interpret_line line =
  let char_array = explode line in
  List.iter interpret_char char_array;
  printf "\n"

(* Read all the lines in a file into a list of string *)
let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    let f x = read_recursive (x :: lines) in
    try Scanf.fscanf in_channel "%[^\r\n]\n" f
    with End_of_file -> lines in
  let lines = read_recursive [] in
  let _ = close_in_noerr in_channel in
  List.rev (lines)

let () =
  for i = 0 to Array.length Sys.argv - 2 do
    List.iter interpret_line (read_all_lines Sys.argv.(i + 1))
  done
