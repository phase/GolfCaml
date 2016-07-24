open Printf
open Objects

(* Token Regex from http://www.golfscript.com/golfscript/syntax.html *)
let token_regex = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\n\r]*|."

(* Iterate of a line and interpret each char *)
let interpret_line line =
  let tokens = Str.split token_regex line in
  List.iter (fun (x) -> printf "%s\n" x) tokens

(* Read all the lines in a file into a list of string *)
let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    let f x = read_recursive (x :: lines) in
    try Scanf.fscanf in_channel "%[^\r\n]\n" f
    with End_of_file -> lines in
  let lines = read_recursive [] in
  close_in_noerr in_channel;
  List.rev (lines)

let () =
  for i = 0 to Array.length Sys.argv - 2 do
    List.iter interpret_line (read_all_lines Sys.argv.(i + 1))
  done
