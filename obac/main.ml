
open Expr;;
open Mathexpr;;
open Simplify;;
open Derivative;;
open Plot;;

(* TODO  : to change later *)
let something () =
  let example = "sqrt(x*x)/sqrt(x+x)" in
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


	let m="7" in
	let m2 = Parsexpr.expr_of_string m in
	let m3 = cons_math_expr m2 in
  let sub_e = Mathexpr.subst r "x" m3 in
  print_string("SUB math_expr: "^(print_tree_of_math sub_e)^"\n\n");

Plot.plot sub_e "x";;

let main =

  print_string "Welcome to Obac 0.01\n";
  if not !Sys.interactive then something ()
