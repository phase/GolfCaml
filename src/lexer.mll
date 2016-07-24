{
  type token =
    | INT of string
    | STRING of string
    | IDENT of string
    | COMMENT of string
    | CODEBLOCK of string

  type state = CODE | LINE_COMMENT
  let state = ref CODE
}

let newline      = '\n'
let num          = [ '0'-'9' ]
let alphanum     = [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ]
let comment_line = "#"([^ '\n' ]+)
let space        = [ ' ' '\t' '\n' ]
let codeblock    = '{'([^ '}' ]+)'}'
let str          = '"'([^ '"' ]+)'"'
let unquoted     = ('/'?(alphanum+'/'?)+)

rule code = parse
| space+                      { code lexbuf }
| "#"                         { line_comment "" lexbuf }
| str                         { STRING (Lexing.lexeme lexbuf) }
| codeblock                   { CODEBLOCK (Lexing.lexeme lexbuf) }
| num+                        { INT (Lexing.lexeme lexbuf) }
| alphanum+                   { unquoted (Lexing.lexeme lexbuf) lexbuf }

and unquoted buff = parse
| "#"                         { state := LINE_COMMENT; if buff = "" then line_comment "" lexbuf else IDENT buff }
| alphanum+                   { unquoted (buff ^ Lexing.lexeme lexbuf) lexbuf }
| space+                      { IDENT buff }

and line_comment buff = parse
| newline                     { state := CODE; COMMENT buff }
| _                           { line_comment (buff ^ Lexing.lexeme lexbuf) lexbuf }

{

  let lexer lb =
    match !state with
    | CODE -> code lb
    | LINE_COMMENT -> line_comment "" lb

  let get_tokens line =
    let queue : token Queue.t = Queue.create () in
    let lexbuf = Lexing.from_string line in
    ( (* TODO: This looks hella ugly *)
      try
        while true do
          Queue.add (lexer lexbuf) queue
        done
      with Failure _ -> ()
    );
    queue
}
