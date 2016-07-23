type stack_object =
  | Int of int
  | Float of float
  | String of string
  | CodeBlock of string

let string_of_object o =
  match o with
    | Int (i) -> string_of_int i
    | Float (f) -> string_of_float f
    | String (s) -> s
    | CodeBlock (s) -> s

let stack : stack_object Stack.t = Stack.create ()
