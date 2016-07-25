open Printf
open Objects

(*
Each operator is structured like this:
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | CodeBlock aCB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
*)

(* `+` operator *)
let add a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> Int (aI + bI)
    | String bS -> String ((string_of_int aI) ^ bS)
    | CodeBlock bCB -> CodeBlock ((string_of_int aI) ^ " " ^ bCB))
  | String aS -> (match b with
    | Int bI -> String (aS ^ (string_of_int bI))
    | String bS -> String (aS ^ bS)
    | CodeBlock bCB -> CodeBlock (aS ^ " " ^ bCB))
  | CodeBlock aCB -> (match b with
    | Int bI -> CodeBlock (aCB ^ " " ^ (string_of_int bI))
    | String bS -> CodeBlock (aCB ^ " " ^ bS)
    | CodeBlock bCB -> CodeBlock (aCB ^ " " ^ bCB))

let sub a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | CodeBlock aCB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")

let mul a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | CodeBlock aCB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")

let div a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")
  | CodeBlock aCB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | CodeBlock bCB -> String "ERROR")

let handle s =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match s with
  | "+" -> add a b
  | "-" -> sub a b
  | "*" -> mul a b
  | "/" -> div a b
  | _ -> Int 0 in
  Stack.push c stack
