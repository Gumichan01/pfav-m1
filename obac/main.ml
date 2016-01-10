
open Expr;;
open Mathexpr;;
open Simplify;;
open Derivative;;
open Plot;;

(* TODO  : to change later *)
let something () =
  let example = "x +1" in
  (*  let e = Parsexpr.expr_of_string "5*sqrt(36+x^2)+4*(20-x)" in*)
  let e = Parsexpr.expr_of_string example in  
  let s = match e with
    | Op(o,_) -> "operator "^o
    | Var v -> "variable "^v
    | Num i -> "number "^(string_of_int i)
      
  and r = cons_math_expr e in
  print_string ("We have parsed "^example^" and its tree starts with "^s^"\n");

  print_string("\nmath_expr: "^(print_tree_of_math r));  
  print_string("\nFormula: "^(print_formula r)^"\n\n");
  
  let simpl_e = simpl r in
  let deriv_e = (derive r "x") in
  let solved_e = (solve r "x") in
(*  print_string("Simplifed math_expr: "^(print_tree_of_math simpl_e)^"\n");*)
  print_string("Simplified formula: "^(print_formula simpl_e)^"\n");
(*  print_string("Derived math_expr: "^(print_tree_of_math deriv_e)^"\n");*)
  print_string("Derived formula: "^(print_formula deriv_e)^"\n");
(*  print_string("Solved math_expr: \n");(print_tree_of_math solved_e);*)
(*  print_string("Solved formula: \n");(print_solve (solved_e));*)
(*  print_string("The result of the evaluation is: "^string_of_float(eval simpl_e)^
		  "\n");*)
  
  
  let m="7" in
  let m2 = Parsexpr.expr_of_string m in
  let m3 = cons_math_expr m2 in
  let sub_e = Mathexpr.subst r "x" m3 in
  print_string("SUB math_expr: "^(print_tree_of_math sub_e)^"\n\n");
  
  Plot.plot sub_e "x";;

let main =
  
  print_string "Welcome to Obac 0.01\n";
  if not !Sys.interactive then something ()
