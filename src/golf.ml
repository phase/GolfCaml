open Printf
open Lexer
open Ops

(* Tokenize a line and interpret the tokens *)
let interpret_string str =
  let tokens = get_tokens str in
  let print_token t =
    match t with
    | INT s -> Stack.push (Objects.Int (int_of_string s)) Objects.stack
    | IDENT s -> printf "IDENT: %s\n" s
    | STRING s ->
      let s = String.sub s 1 ((String.length s) - 2) in
      Stack.push (Objects.String s) Objects.stack
    | BLOCK s ->
      let s = String.sub s 1 ((String.length s) - 2) in
      Stack.push (Objects.Block s) Objects.stack
    | OPERATOR s -> Ops.handle s
    | COMMENT _ -> () in
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

let interpret_inputs () =
  for i = 0 to Array.length Sys.argv - 2 do
    List.iter interpret_string (read_all_lines Sys.argv.(i + 1))
  done

let () =
  interpret_inputs ();
  Objects.print_stack_reverse ()
