
open Expr;;
open Mathexpr;;
open Simplify;;
open Derivative;;
open Plot;;


(* TODO  : to change later *)
let something () =
  let example = "y^x + (x+y)" in
  (*  let e = Parsexpr.expr_of_string "5*sqrt(36+x^2)+4*(20-x)" in*)
  let e = Parsexpr.expr_of_string example in  
  let s = match e with
    | Op(o,_) -> "operator "^o
    | Var v -> "variable "^v
    | Num i -> "number "^(string_of_int i) 
  and r = cons_math_expr e in
  print_string ("We have parsed "^example^" and its tree starts with "^s^"\n");
  print_string("\nmath_expr: "^(print_tree_of_math r)^"\n\n");
  let simpl_e = simpl r in 
  print_string("Simplified math_expr: "^(print_tree_of_math simpl_e)^"\n\n");
  
  let ex="5" in
  let ma = cons_math_expr (Parsexpr.expr_of_string ex) in
  let sub_e = Mathexpr.subst simpl_e "x" ma in
  print_string("SUB math_expr: "^(print_tree_of_math sub_e)^"\n\n");
  Plot.plot e 5;;

let main =
  print_string "Welcome to Obac 0.01\n";
  if not !Sys.interactive then something ()
