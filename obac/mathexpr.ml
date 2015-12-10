

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
  | Frac of ('n,'op) gen_math_expr * ('n,'op) gen_math_expr (* Fraction       *)
  | Sqrt of ('n,'op) gen_math_expr                          (* Square root    *)
  | Expo of ('n,'op) gen_math_expr                          (* Exponential    *)
  | Log of ('n,'op) gen_math_expr                           (* Logarithm      *)
  | Cos of ('n,'op) gen_math_expr                           (* Cosine         *)
  | Sin of ('n,'op) gen_math_expr                           (* Sine           *)
  | Tan of ('n,'op) gen_math_expr                           (* Tangent        *)
  | Acos of ('n,'op) gen_math_expr                          (* Secant         *)
  | Asin of ('n,'op) gen_math_expr                          (* Cosecant       *)
  | Atan of ('n,'op) gen_math_expr                          (* Cotangent      *)
;;

(* The Mathematical expression that will be used in the program *)
type math_expr = (Num.num,char) gen_math_expr;;



(* Build a recursive Binop expression with the same operator *)
(* It is used when the expression is one of the followings: 
   x1 + x2 + ... + xn
   x1 - x2 - ... - xn
   x1 * x2 * ... * xn
*)
let rec consBinop (op: char) (l : math_expr list) : math_expr = 
  match l with
    | [] -> failwith "Binop cannot be applied on an empty list"
    | [x] -> x
    | t::q -> Binop(op,t,(consBinop op q))
;;

(* Create the Fraction expression *)
let rec consFract:  math_expr list -> math_expr = 
  fun l -> 
    match l with
      | [] -> failwith "Binop cannot be applied on an empty list"
      | [x] -> x
      | t::q -> Frac(t,consFract(q))
;;



(* Build a mathematical expression from a basic expression *)
let rec consMathExpr (b : (*Expr.*)basic_expr) : math_expr = 
match b with
  | Num n -> Val (Num.Int n)
  | Var s -> Var s
  | Op(s,l) -> parse_op (s,l)

(* Parse any kind of operation *)
and parse_op = function
  | ("pi",[]) -> Pi
  | ("e",[]) -> Exp0
  (* Basic operations *)
  | ("+",_) as p -> parse_basic_op p
  | ("-",_) as m -> parse_basic_op m
  | ("*",_) as f -> parse_basic_op f
  | ("/",_) as d -> parse_basic_op d
  (* Mathematical functions *)
  | _ as o -> parse_math_function o

(* Mathematical functions to parse *)
and parse_math_function = function
  | ("sqrt",[x]) -> Sqrt(consMathExpr x)
  | ("exp",[x]) -> Expo(consMathExpr x)
  | ("log",[x]) -> Log(consMathExpr x)
  | ("cos",[x]) -> Cos(consMathExpr x)
  | ("sin",[x]) -> Sin(consMathExpr x)
  | ("tan",[x]) -> Tan(consMathExpr x)
  | ("acos",[x]) -> Acos(consMathExpr x)
  | ("asin",[x]) -> Asin(consMathExpr x)
  | ("atan",[x]) -> Atan(consMathExpr x)
  | _ -> failwith "Unrecognized operator to parse"

(* Parse any kind of basic operation: '+', '-', '*', '/' *)
and parse_basic_op = function
  | ("+",[t]) -> let m1 = consMathExpr t in
		 Unop ('+',m1)
  | ("-",[t]) -> let m1 = consMathExpr t in
		  Unop ('-',m1)
  | ("+",t::q) -> let l = List.map (consMathExpr) (t::q) in
				       consBinop '+' l
  | ("-",t::q) -> let l = List.map (consMathExpr) (t::q) in
				       consBinop '-' l
  | ("*",t::q) when (List.length (t::q) > 1) -> 
    let l = List.map (consMathExpr) (t::q) in consBinop '*' l
  | ("/",t::q) when (List.length (t::q) > 1) ->
    let l = List.map (consMathExpr) (t::q) in  consFract l
  | _ -> failwith "Unrecognized basic operator to parse"
;;


(* Integration of an expression *)
let rec integ : math_expr -> string -> math_expr -> math_expr -> math_expr = 
  failwith "TODO integ : math_expr -> string -> math_expr -> math_expr -> math_expr ";;


(* Derive an expression *)
let rec derive : math_expr -> string -> math_expr = 
  failwith "TODO derive : math_expr -> string -> math_expr ";;


(* Solve an equation finding a value that 
   puts the expression to zero *)
let rec solve : math_expr -> string -> math_expr = 
  failwith "TODO solve : math_expr -> string -> math_expr ";;


(* Simplify an expression *)
let rec simpl : math_expr -> math_expr = 
  fun x -> match x with
    | Binop(_,_,_) as b -> simpl_binop b
    | Frac(_,_) as f -> simpl_fract f
    | Sqrt(_) as s -> simpl_sqrt s
    | Expo(_) as e -> simpl_exp e
    | Log(_) as l -> simpl_log l
    (* In this case, the operation is a trigonometric function *)
    | _ as s -> simpl_trigo s

(* Simplify a binary operation *)
and simpl_binop = function
  | _ as o -> o 

(* Simplify a fraction *)
and simpl_fract = function
  | _ as o -> o 

(* Simplify a square root *)
and simpl_sqrt = function
  | _ as o -> o 

(* Simplify a exponential function *)
and simpl_exp = function
  | _ as o -> o 

(* Simplify the logarithm *)
and simpl_log = function
  | _ as o -> o 

(* Simplify a trigonometric *)
and simpl_trigo = function
  | _ as o -> o 
;;



(* Subtitution *)
let rec subst : math_expr -> string -> math_expr -> math_expr = 
  failwith "TODO subst : math_expr -> string -> math_expr -> math_expr ";;

(* Evaluate an expression to get a floating point value *)
let rec eval : math_expr -> float = 
  failwith "TODO eval : math_expr -> float ";;




(* Test *)
(* Ces tests doivent échouer *)
consMathExpr (Op ("",[]));;
consMathExpr (Op ("",[Var "pi"]));;
consMathExpr (Op ("+",[]));;
consMathExpr (Op ("-",[]));;
consMathExpr (Op ("*",[]));;
consMathExpr (Op ("*",[Var "pi"]));;
consMathExpr (Op ("/",[]));;
consMathExpr (Op ("/",[Var "pi"]));;          

(* Ces tests doivent réussir *)
consMathExpr (Num 5);;
consMathExpr (Var "x");;
consMathExpr (Op ("+",[Var "pi"]));;
consMathExpr (Op ("-",[Var "pi"]));;
consMathExpr (Op ("+",[(Num 1);(Num 2)]));;
consMathExpr (Op ("+",[(Op("+",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("-",[(Num 1);(Num 2)]));;
consMathExpr (Op ("-",[(Op("+",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("+",[(Op("-",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("*",[(Op("*",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("*",[(Op("+",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("-",[(Op("*",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("-",[(Op("/",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("/",[(Num 1);(Num 2)]));;
consMathExpr (Op ("/",[(Op("/",[Num 2;Num 3]));Num 5]));;
consMathExpr (Op ("sqrt",[(Num 1)]));;
consMathExpr (Op ("sqrt",[(Op("/",[Num 2;Num 3]))]));;
consMathExpr (Op ("sqrt",[Op ("-",[(Op("*",[Num 2;Num 3]));Num 5])]));;
consMathExpr (Op ("exp",[Op ("sqrt",[Op ("-",[(Op("*",[Num 2;Num 3]));Num 5])])]));;
consMathExpr (Op ("cos",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
consMathExpr (Op ("sin",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
consMathExpr (Op ("tan",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
consMathExpr (Op ("acos",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
consMathExpr (Op ("asin",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
consMathExpr (Op ("atan",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
