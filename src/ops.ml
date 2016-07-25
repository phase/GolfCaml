open Printf
open Objects

(*
Each operator is structured like this:
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | Block aB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
*)

(* `+` operator *)
let add a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> Int (aI + bI)
    | String bS -> String ((string_of_int aI) ^ bS)
    | Block bB -> Block ((string_of_int aI) ^ " " ^ bB))
  | String aS -> (match b with
    | Int bI -> String (aS ^ (string_of_int bI))
    | String bS -> String (aS ^ bS)
    | Block bB -> Block (aS ^ " " ^ bB))
  | Block aB -> (match b with
    | Int bI -> Block (aB ^ " " ^ (string_of_int bI))
    | String bS -> Block (aB ^ " " ^ bS)
    | Block bB -> Block (aB ^ " " ^ bB))

let sub a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | Block aB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")

let mul a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | Block aB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")

let div a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | String aS -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")
  | Block aB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")

let binary_operator f =
  let b = Stack.pop stack in
  let a = Stack.pop stack in
  let c = f a b in
  Stack.push c stack

let handle s =
  match s with
  | "+" -> binary_operator add
  | "-" -> binary_operator sub
  | "*" -> binary_operator mul
  | "/" -> binary_operator div
  | _ -> ()
