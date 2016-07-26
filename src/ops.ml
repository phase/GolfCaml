open Printf
open Objects

(*
Binary operators are structured like this:
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

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let strip s c =
  let len = String.length s in
  let res = Bytes.create len in
  let rec check i j =
    if i >= len then String.sub res 0 j
    else if String.contains c s.[i] then
      check (succ i) j
    else begin
      Bytes.set res j s.[i];
      check (succ i) (succ j)
    end
  in check 0 0

(* `-` operator *)
let sub a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> Int (aI - bI)
    (* This one is hella weird. If the string contains the integer, then a blank
    string is pushed. Otherwise, a string containing *only that integer* is
    pushed. Blocks act the same way. I guess it's used for comparing. *)
    | String bS -> let aIS = string_of_int aI in
      if contains bS aIS
      then String ""
      else String aIS
    | Block bB -> let aIS = string_of_int aI in
      if contains bB aIS
      then Block ""
      else Block aIS)
  | String aS -> (match b with
    | Int bI -> String (strip aS (string_of_int bI))
    | String bS -> String (strip aS bS)
    | Block bB -> Block (strip aS bB))
  | Block aB -> (match b with
    | Int bI -> Block (strip aB (string_of_int bI))
    | String bS -> Block (strip aB bS)
    | Block bB -> Block (strip aB bB))

let repeat s n =
  let len = Bytes.length s in
  let res = Bytes.create (n * len) in
  for i = 0 to pred n do
    Bytes.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let repeat_between a b =
  let a_len = Bytes.length a in
  let b_len = Bytes.length b in
  let res_len = ((pred a_len) * b_len) + a_len in
  let res = Bytes.create res_len in
  for i = 0 to (a_len - 2) do
    let b_iters = i + (i * b_len) in
    Bytes.blit a i res b_iters 1;
    Bytes.blit b 0 res (succ b_iters) b_len
  done;
  Bytes.blit a (pred a_len) res (pred res_len) 1;
  Bytes.to_string res

(* `*` operator *)
let mul a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> Int (aI * bI)
    | String bS -> String (repeat bS aI)
    | Block bB -> Block (repeat bB aI))
  | String aS -> (match b with
    | Int bI -> String (repeat aS bI)
    | String bS -> String (repeat_between aS bS)
    | Block bB -> Block "") (* Not sure what this does. "1"{1}* == 49?!?!?! *)
  | Block aB -> (match b with
    | Int bI -> String "ERROR"
    | String bS -> String "ERROR"
    | Block bB -> String "ERROR")

(* `/` operator *)
let div a b =
  match a with
  | Int aI -> (match b with
    | Int bI -> Int (aI / bI)
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
