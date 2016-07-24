open Printf
open Objects

let add () =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  match a with
  | Int ai ->
    match b with
    | Int bi ->
      let ci = Int (ai + bi) in
      Stack.push ci stack
    | _ -> ()
  | _ -> ()

let sub () =
  printf "Sub not implemented yet\n"

let mul () =
  printf "Mul not implemented yet\n"

let div () =
  printf "Div not implemented yet\n"

let handle s =
  match s with
  | "+" -> add ()
  | "-" -> sub ()
  | "*" -> mul ()
  | "/" -> div ()
  | _ -> printf "Operator not found: %s\n" s
