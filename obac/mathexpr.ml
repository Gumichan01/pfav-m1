

#use "expr.mli";;

(* Generic mathematic expression *)
type ('n,'op) gen_math_expr =
  | Pi                                                      (* Pi : 3.14...   *)
  | Exp0                                                    (* e : exp(0)     *)
  | Val of 'n                                               (* Constant value *)
  | Var of string                                           (* Variable       *)
  | Unop of 'op * ('n,'op) gen_math_expr                    (* '+','-' unaire *)
  | Binop of 'op * 
      ('n,'op) gen_math_expr * 
      ('n,'op) gen_math_expr                                (* '+','-','*'    *)
  | Sqrt of ('n,'op) gen_math_expr                          (* Square root    *)
  | Expo of ('n,'op) gen_math_expr                          (* Exponential    *)
  | Log of ('n,'op) gen_math_expr                           (* Logarithm      *)
  | Cos of ('n,'op) gen_math_expr                           (* Cosine         *)
  | Sin of ('n,'op) gen_math_expr                           (* Sine           *)
  | Tan of ('n,'op) gen_math_expr                           (* Tangent        *)
  | Acos of ('n,'op) gen_math_expr                          (* Secant         *)
  | Asin of ('n,'op) gen_math_expr                          (* Cosecant       *)
  | Atan of ('n,'op) gen_math_expr                          (* Cotangent      *)
  | Frac of ('n,'op) gen_math_expr * ('n,'op) gen_math_expr (* Fraction       *)
;;

(* The Mathetical expression that will be used in the program *)
type math_expr = (Num.num,char) gen_math_expr;;

(* Build a mathematical expression from a basic expression *)
let rec consMathExpr (b : (*Expr.*)basic_expr) : math_expr = 
match b with
  | Num n -> Val (Num.Int n)
  | Var s -> Var s
  | Op(s,l) -> parse_basic_expr (s,l)
  | _ -> failwith "TODO create a mathematical expression from a basic_expr"

and parse_basic_expr = function
  | ("pi",[]) -> Pi
  | ("e",[]) -> Exp0
  | ("+",[t]) -> let m1 = consMathExpr t in
		  Unop ('+',m1)
  | ("-",[t]) -> let m1 = consMathExpr t in
		  Unop ('-',m1)
  | _ -> failwith "Unrecognized expression to parse"
;;

(* Test *)
consMathExpr (Num 5);;
consMathExpr (Var "x");;
consMathExpr (Op ("+",[Var "pi"]));;
consMathExpr (Op ("-",[Var "pi"]));;
consMathExpr (Op ("+",[]));;
