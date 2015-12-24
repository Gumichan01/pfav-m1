open Expr;;
(*#use "expr.mli";*)

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
  | Pow of ('n,'op) gen_math_expr * ('n,'op) gen_math_expr  (* Power          *)
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

(* Exceptions *)
exception Reserved_keyword of string;;
exception Parsing_error of string;;
exception Invalid_binop of string;;
exception Invalid_fraction of string;;
exception Invalid_power of string;;
exception Invalid_sqrt of string;;
exception Invalid_log of string;;
exception Invalid_math_expr of string;;
exception Invalid_derive_n_Argument of string;;



let pi_div_two = Frac(Pi,Val(Num.Int(2)))
let pi_div_three = Frac(Pi,Val(Num.Int(3)))
let pi_div_four = Frac(Pi,Val(Num.Int(4)))
let pi_div_six = Frac(Pi,Val(Num.Int(6)))

let two_pi_div_three = Frac(Binop('*',Val(Num.Int(2)),Pi),Val(Num.Int(3))) 
let three_pi_div_three = Frac(Binop('*',Val(Num.Int(3)),Pi),Val(Num.Int(4)))
let five_pi_div_six = Frac(Binop('*',Val(Num.Int(5)),Pi),Val(Num.Int(6)))

let one_half = Frac(Val(Num.Int(1)),Val(Num.Int(2)))
let sqrt_three_div_2 = Frac(Sqrt(Val(Num.Int(3))),Val(Num.Int(2)))
let sqrt_two_div_two = Frac(Sqrt(Val(Num.Int(2))),Val(Num.Int(2)))



(* A function that print the tree of the given expression *)
let rec print_tree_of_math : math_expr -> string = fun m ->
  match m with
    | Pi -> "Pi"
    | Exp0 -> "e"
    | Val(Num.Int(x)) -> "Val(Num.Int("^(string_of_int x)^"))"
    | Var s -> "Var("^s^")"
    | Unop(op,e) -> "Unop("^(Char.escaped op)^","^(print_tree_of_math e)^")"
    | Binop(op,e1,e2) -> "Binop("^(Char.escaped op)^","^(print_tree_of_math e1)^
      ","^(print_tree_of_math e2)^")"
    | Frac(e1,e2) -> "Frac("^(print_tree_of_math e1)^","^
      (print_tree_of_math e2)^")"
    | Pow(e1,e2) -> "Pow("^(print_tree_of_math e1)^","^
      (print_tree_of_math e2)^")"
    | Sqrt(n) -> "Sqrt("^(print_tree_of_math n)^")"
    | Expo(n) -> "Expo("^(print_tree_of_math n)^")"
    | Log(n) -> "Log("^(print_tree_of_math n)^")"
    | Cos(n) -> "Cos("^(print_tree_of_math n)^")"
    | Sin(n) -> "Sin("^(print_tree_of_math n)^")"
    | Tan(n) -> "Tan("^(print_tree_of_math n)^")"
    | Acos(n) -> "Acos("^(print_tree_of_math n)^")"
    | Asin(n) -> "Asin("^(print_tree_of_math n)^")"
    | Atan(n) -> "Atan("^(print_tree_of_math n)^")"
    | _ -> raise (Invalid_math_expr "Invalid mathematic expression to print")
;;


(* Check if the string is a reserved keyword *)
let is_not_reserved_keyword = function
  | "pow" | "sqrt" | "exp" | "log" -> false 
  | "cos" | "sin" | "tan" | "acos" | "asin" | "atan" -> false
  | _ -> true
;;


(* A shorcut to apply map *)
let map_list f l = List.map f l;;


let consVar: string -> math_expr = 
  fun s ->
    if is_not_reserved_keyword(s)
    then 
      (Var s)
    else 
      raise (Reserved_keyword (s^" is a keyword, not usable as a variable"))
;;

(* Build a recursive Binop expression with the same operator *)
let rec cons_binop (op: char) (l : math_expr list) : math_expr = 
  match l with
    | [] -> raise (Invalid_binop "Binop cannot be applied on an empty list")
    | [x] -> x
    | t::q -> Binop(op,t,(cons_binop op q))
;;


(* Create the Fraction expression *)
let cons_frac:  math_expr list -> math_expr = 
  fun l -> 
    match l with
      | [x;y] -> Frac(x,y)
      | _ -> raise (Invalid_fraction "Invalid fraction expression")
;;

(* Create the Power expression *)
let cons_pow: math_expr list -> math_expr = 
  fun l -> 
    match l with
      | [x;y] -> Pow(x,y)
      | _ -> raise (Invalid_power "Invalid power expression")
;;


(* Auxilliary function for binary operations *)
let parse_binop_aux op l f = 
      let ll = map_list f l in
      match op with
	| "+" -> cons_binop '+' ll
	| "-" -> cons_binop '-' ll
	| "*" -> cons_binop '*' ll
	| "/" -> cons_frac ll
	| _  as s -> raise (Invalid_math_expr (s^"is not a valid operator"))
;;



(* Build a mathematical expression from a basic expression *)
let rec cons_math_expr (b : basic_expr) : math_expr = 
match b with
  | Num n -> Val (Num.Int n)
  | Var s -> consVar s
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
(* TODO : interdire log(-1) et a(cos/sin/tan) de x pour x > 1 ou x < -1 *)
and parse_math_function = function
  | ("^",l) when (List.length l = 2) -> 
    let ll = map_list (cons_math_expr) l in cons_pow ll
  | ("sqrt",[x]) -> parse_sqrt (cons_math_expr x)
  | ("exp",[x]) -> Expo(cons_math_expr x)
  | ("log",[x]) -> parse_log (cons_math_expr x)
  | ("cos",[x]) -> Cos(cons_math_expr x)
  | ("sin",[x]) -> Sin(cons_math_expr x)
  | ("tan",[x]) -> Tan(cons_math_expr x)
  | ("acos",[x]) -> Acos(cons_math_expr x)
  | ("asin",[x]) -> Asin(cons_math_expr x)
  | ("atan",[x]) -> Atan(cons_math_expr x)
  | _ -> raise (Parsing_error "Unrecognized mathematic operator to parse")

(* Parse any kind of basic operation: '+', '-', '*', '/' *)
and parse_basic_op = function
  | ("+",[t]) -> let m1 = cons_math_expr t in
		 Unop ('+',m1)
  | ("-",[t]) -> let m1 = cons_math_expr t in
		  Unop ('-',m1)
  | ("+",l) -> parse_binop_aux "+" l (cons_math_expr)
  | ("-",l) when (List.length l = 2) -> parse_binop_aux "-" l (cons_math_expr)
  | ("*",l) when (List.length l > 1) -> parse_binop_aux "*" l (cons_math_expr)
  | ("/",l) when (List.length l = 2) -> parse_binop_aux "/" l (cons_math_expr)
  | _ -> raise (Parsing_error "Unrecognized basic operator to parse")


(* Check if the argument of sqrt is valid *)
and parse_sqrt = function
  (* If the argument is a positive or null value -> OK; otherwise -> KO *)
  | Val(Num.Int(x)) as v when x >= 0 -> Sqrt(v)
  | Val(Num.Int(x)) -> raise (Invalid_sqrt ("Invalid square root of "
					    ^(string_of_int x)^""))
  (* If the argument is -y -> KO*)
  | (Unop('-',Var(y))) -> raise (Invalid_sqrt ("Invalid square root of -"^y^""))

  (* Warning : some expressions can be invalid,
     so it will be necessary to check them during evaluation *)
  | _ as r -> Sqrt(r)

(* Check if the argument of log is valid *)
and parse_log = function
  (* If the argument is a non-zero but positive value -> OK; otherwise -> KO *)
  | Val(Num.Int(x)) as v when x > 0 -> Log(v)
  | Val(Num.Int(x)) -> raise (Invalid_log ("Invalid logarithm of "
					    ^(string_of_int x)^""))

  (* If the argument is -y -> KO*)
  | (Unop('-',Var(y))) -> raise (Invalid_sqrt ("Invalid logarithm of -"^y^""))

  | _ as r -> Log(r)
;;



(* Solve an equation finding a value that 
   puts the expression to zero *)
let rec solve : math_expr -> string -> math_expr = 
  fun x s -> match x with
    | _ -> failwith "TODO solve : math_expr -> string -> math_expr ";;


(* Subtitution *)
let rec subst : math_expr -> string -> math_expr -> math_expr = 
  fun x s m -> match x with
    | _ -> failwith "TODO subst : math_expr -> string -> math_expr -> math_expr ";;


(* Evaluate an expression to get a floating point value *)
let rec eval : math_expr -> float = 
  fun m -> match m with
    | _ -> failwith "TODO eval : math_expr -> float ";;



(* Test *)
(* Ces tests doivent echouer *)
(*cons_math_expr (Op ("",[]));;
cons_math_expr (Op ("",[Var "pi"]));;
cons_math_expr (Op ("+",[]));;
cons_math_expr (Op ("-",[]));;
cons_math_expr (Op ("*",[]));;
cons_math_expr (Op ("*",[Var "pi"]));;
cons_math_expr (Op ("/",[]));;
cons_math_expr (Op ("/",[Var "pi"]));;
cons_math_expr (Op ("-",[(Num 1);(Num 2);(Num 3)]));;
cons_math_expr (Op ("^",[(Num 2);(Num 8);(Num 4)]));;
cons_math_expr (Op ("^",[(Num 2)]));;*)

(* Ces tests doivent reussir *)
(*cons_math_expr (Num 5);;
cons_math_expr (Var "x");;
cons_math_expr (Op ("+",[Var "pi"]));;
cons_math_expr (Op ("-",[Var "pi"]));;
cons_math_expr (Op ("+",[(Num 1);(Num 2)]));;
cons_math_expr (Op ("+",[(Op("+",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("-",[(Num 1);(Num 2)]));;
cons_math_expr (Op ("-",[(Op("+",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("+",[(Op("-",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("*",[(Op("*",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("*",[(Op("+",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("-",[(Op("*",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("-",[(Op("/",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("/",[(Num 1);(Num 2)]));;
cons_math_expr (Op ("/",[(Op("/",[Num 2;Num 3]));Num 5]));;
cons_math_expr (Op ("^",[(Num 2);(Num 8)]));;
cons_math_expr (Op ("sqrt",[(Num 1)]));;
cons_math_expr (Op ("sqrt",[(Op("/",[Num 2;Num 3]))]));;
cons_math_expr (Op ("sqrt",[Op ("-",[(Op("*",[Num 2;Num 3]));Num 5])]));;
cons_math_expr (Op ("exp",[Op ("sqrt",[Op ("-",[(Op("*",[Num 2;Num 3]));Num 5])])]));;
cons_math_expr (Op ("cos",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
cons_math_expr (Op ("sin",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
cons_math_expr (Op ("tan",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
cons_math_expr (Op ("acos",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
cons_math_expr (Op ("asin",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;
cons_math_expr (Op ("atan",[(Op("/",[Op ("+",[Var "pi"]);Num 3]))]));;*)
