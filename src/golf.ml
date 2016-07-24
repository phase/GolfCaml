open Printf
open Lexer

(* Tokenize a line and interpret the tokens *)
let interpret_line line =
  let tokens = get_tokens line in
  let print_token t =
    match t with
    | INT s -> printf "INT: %s\n" s
    | IDENT s -> printf "IDENT: %s\n" s
    | STRING s -> printf "STRING: %s\n" s
    | COMMENT s -> printf "COMMENT: %s\n" s
    | CODEBLOCK s -> printf "CODEBLOCK: %s\n" s in
  Queue.iter print_token tokens

(* Read all the lines in a file into a string list *)
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
