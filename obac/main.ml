
open Expr

(* TODO : to change later *)

let something () =
  let example = "5*sqrt(36+x^2)+4*(20-x)" in
  let e = Parsexpr.expr_of_string "5*sqrt(36+x^2)+4*(20-x)" in
  let s = match e with
    | Op(o,_) -> "operator "^o
    | Var v -> "variable "^v
    | Num i -> "number "^(string_of_int i)
  in
  print_string ("We have parsed "^example^" and its tree starts with "^s^"\n")

let main =
  print_string "Welcome to Obac 0.01\n";
  if not !Sys.interactive then something ()
