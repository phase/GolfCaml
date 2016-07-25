open Printf
open Objects

(*
Each operator is structured like this:
let XX () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match a with
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
  in
  Stack.push c stack
*)

(* `+` operator *)
let add () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match a with
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
  in
  Stack.push c stack

let sub () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match a with
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
  in
  Stack.push c stack

let mul () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match a with
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
  in
  Stack.push c stack

let div () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = match a with
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
  in
  Stack.push c stack

let handle s =
  match s with
  | "+" -> add ()
  | "-" -> sub ()
  | "*" -> mul ()
  | "/" -> div ()
  | _ -> printf "Operator not found: %s\n" s
