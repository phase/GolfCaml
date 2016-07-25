type stack_object =
  | Int of int
  | String of string
  | CodeBlock of string

let string_of_object o =
  match o with
  | Int i -> string_of_int i
  | String s -> s
  | CodeBlock s -> "{" ^ s ^ "}"

let stack : stack_object Stack.t = Stack.create ()

let reverse stack =
  let elements = Stack.create () in
  Stack.iter (fun x -> Stack.push x elements) stack;
  elements

let print_stack_reverse () =
  let print_object o =
    Printf.printf "%s" (string_of_object o) in
  Stack.iter print_object (reverse stack);
  Printf.printf "\n"
