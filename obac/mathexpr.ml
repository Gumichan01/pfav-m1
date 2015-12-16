

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
exception Invalid_math_expr of string;;

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
and parse_math_function = function
  | ("^",l) when (List.length l = 2) -> 
    let ll = map_list (cons_math_expr) l in cons_pow ll
  | ("sqrt",[x]) -> Sqrt(cons_math_expr x)
  | ("exp",[x]) -> Expo(cons_math_expr x)
  | ("log",[x]) -> Log(cons_math_expr x)
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
;;



(* Integration of an expression *)
let rec integ : math_expr -> string -> math_expr -> math_expr -> math_expr = 
fun x s a b -> match x with
  | _ -> failwith "TODO integ : math_expr -> string -> math_expr -> math_expr -> math_expr ";;


(* Derive an expression *)
let rec derive : math_expr -> string -> math_expr = 
fun x s -> match x with
  | _ -> failwith "TODO derive : math_expr -> string -> math_expr ";;
  


(* Solve an equation finding a value that 
   puts the expression to zero *)
let rec solve : math_expr -> string -> math_expr = 
  fun x s -> match x with
    | _ -> failwith "TODO solve : math_expr -> string -> math_expr ";;


(* Simplify an expression *)
let rec simpl : math_expr -> math_expr = 
  fun x -> match x with
    | Unop(_,_) as u -> simpl_unop u
    | Binop(_,_,_) as b -> simpl_binop b
    | Frac(_,_) as f -> simpl_fract f
    | Pow(_,_) as p -> simpl_pow p
    | Sqrt(_) as s -> simpl_sqrt s
    | Expo(_) as e -> simpl_exp e
    | Log(_) as l -> simpl_log l
    (* In this case, the operation is a trigonometric function *)
    | _ as s -> simpl_trigo s

(* Simplify a unary operation *)
and simpl_unop = function
  | Unop('-',Unop('-',x)) -> simpl(x)
  | Unop(s,x) -> Unop(s,simpl(x))
  | _ as o -> o

(* Simplify a binary operation *)
and simpl_binop = function
  | Binop ('+',_,_) as bplus-> simpl_plus bplus
  | Binop ('-',_,_) as bminus-> simpl_minus bminus
(** TODO : '*' et '/' *)
  | Binop(op,x,y) -> Binop(op,(simpl x),(simpl y))
  | _ as bf -> bf

(* Simplify additions *)
and simpl_plus = function
  (* a² + 2ab +b² = (a + b)² *)
  | (Binop('+',Pow(a,p1),Binop('+',Binop('*',Val(Num.Int(2)),
					 Binop('*',aa,bb)),Pow(b,p2))) as id)
    when (p1 = p2) && (p1 = Val(Num.Int(2))) -> simpl_identity id a aa b bb p1
  (* x + x*y = x * (y + 1) *)
  | Binop('+',x,Binop('*',y,Val(Num.Int(z)))) 
      when x = y -> simpl_binop(Binop('*',simpl(x),Val(Num.Int(z+1))))
  (* x + y*x = x * (y + 1) *)
  | Binop('+',x,Binop('*',Val(Num.Int(z)),y)) 
      when x = y -> simpl_binop(Binop('*',simpl(x),Val(Num.Int(z+1))))
  (* x + x*y = x * (y+z), z is an expression *)
  | Binop('+',x,Binop('*',y,z)) 
      when x = y -> simpl_binop(Binop('*',x,Binop('+',simpl(z),Val(Num.Int 1))))
  (* x + z*x = x * (y+z), z is an expression *)
  | Binop('+',x,Binop('*',z,y)) 
      when x = y -> simpl_binop(Binop('*',x,Binop('+',simpl(z),Val(Num.Int 1))))
  (* x + x = x * 2 *)
  | Binop('+',x,y) when x = y -> simpl_binop(Binop('*',simpl(x),Val(Num.Int 2)))
    (* ax + ay = a * (x + y) *)
  | Binop('+',Binop('*',a,x),Binop('*',b,y)) when a = b -> 
    Binop('*',simpl(a),simpl(Binop('+',x,y)))
  (* Sum of x1 + x1 + ... + xn, x[1-n] are the same expression *)
  | Binop('+' as p,x,y) -> simpl_binop_aux p x y
  | _ as o -> o

(* Simplify substractions *)
and simpl_minus = function
  (* x - 0 = x *)
  | Binop('-',x,Val(Num.Int(0))) -> simpl(x)
  (* x - x = 0 *)
  | Binop('-',x,y) when x = y -> Val(Num.Int 0)
  (* x - (-y) = x + y *)
  | Binop('-',x,Unop('-',y)) -> simpl_binop(Binop('+',simpl(x),simpl(y)))
  (** TODO : x - x - ... - x *)
  (* x - z*x : z is a value *)
  | Binop('-',x,Binop('*',y,Val(Num.Int(z)))) 
      when x = y -> simpl_binop(Unop('-',Binop('*',Val(Num.Int(z-1)),simpl(x))))
  (* x - x*z : z is a value *)
  | Binop('-',x,Binop('*',Val(Num.Int(z)),y))
      when x = y -> simpl_binop(Unop('-',Binop('*',Val(Num.Int(z-1)),simpl(x))))
  (* x + x*y = x * (y+z), z is an expression *)
  | Binop('-',x,Binop('*',y,z))
      when x = y -> simpl_binop(Unop('-',
				     Binop('*',x,Binop('-',simpl(z),
						       Val(Num.Int 1)))))
  (* x + z*x = x * (y+z), z is an expression *)
  | Binop('-',x,Binop('*',z,y))
      when x = y -> simpl_binop(Unop('-',
				     Binop('*',x,Binop('-',simpl(z),
						       Val(Num.Int 1)))))
  | _ as o -> o

(* Simplify a² +2ab+ b² *)
and simpl_identity id a aa b bb p =
  let a' = (simpl a) and aa' = (simpl aa)
  and b' = (simpl b) and bb' = (simpl bb) in
  if((a' = aa') && (b' = bb')) then
    Pow(Binop('+',simpl(a),simpl(b)),p)
  else
    id
    
(* Auxiliary function of simpl_binop when operation : '+' *)
and simpl_binop_aux op x y = 
  let t = simpl x in let z = simpl y in 
		     let ex = (Binop(op,t,z)) in
		     if z <> y then simpl_binop (ex) else ex    

(* Simplify a fraction *)
and simpl_fract = function
  | _ as o -> o 

(* Simplify a power *)
and simpl_pow = function
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
  fun x s m -> match x with
    | _ -> failwith "TODO subst : math_expr -> string -> math_expr -> math_expr ";;


(* Evaluate an expression to get a floating point value *)
let rec eval : math_expr -> float = 
  fun m -> match m with
    | _ -> failwith "TODO eval : math_expr -> float ";;



(* Test *)
(* Ces tests doivent échouer *)
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

(* Ces tests doivent réussir *)
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
