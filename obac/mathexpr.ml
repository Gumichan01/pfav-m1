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
exception Invalid_derive_n_Argument of string;;

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


let rec pgcd x y = 
	if y==0 then x else pgcd  y (x mod y)

let rec extend_pgcd x y =
	if y= 0 then (1, 0, x)
	else 
	let q= x/y in let (u,v,g ) =extend_pgcd y ( x - q * y ) in
	(v , u-q * v , g)


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


(* Derive an expression
let rec derive : math_expr -> string -> math_expr = 
fun x s -> match x with
  | _ -> failwith "TODO derive : math_expr -> string -> math_expr ";;
 *)


(* TODO revoir derivation, signature non conforme vis-à-vis de l'énoncé*)
let rec derive : math_expr -> math_expr = 
  fun x -> match x with

    | Var(_) as u -> derive_var u
    | Unop(_,_) as u -> derive_unop u
    | Binop(_,_,_) as b -> derive_binop b
    | Frac(_,_) as f -> derive_fract f
    | Pow(_,_) as p -> derive_pow p
    | Sqrt(_) as s -> derive_sqrt s
    | Expo(_) as e -> derive_exp e
    | Log(_) as l -> derive_log l

    | Cos(_) as l -> derive_cos l
    | Sin(_) as l -> derive_sin l
    | Tan(_) as l -> derive_tan l
    | Acos(_) as l -> derive_acos l
    | Asin(_) as l -> derive_asin l
    | Atan(_) as l -> derive_atan l

    | Pi -> derive_val
    | Exp0 -> derive_val
    | Val(_) -> derive_val
   (* | _ -> derive_val *)
    

and derive_val =  Val(Num.Int(0))

and derive_var = function
   | Var(s) -> Val(Num.Int(1))
   | _ as o -> o

and derive_unop = function
   | Unop(op,x) -> Unop (op, (derive x))
   | _ as o -> o


and derive_binop = function
   |Binop ('+',a,b) -> Binop ('+', (derive a) , (derive b))
   |Binop ('-',a,b) -> Binop ('+', (derive a) , (derive (Unop('-',b) )) )
   |Binop ('*',a,b) -> Binop (
				'+',Binop('*', (derive a) ,b )  , Binop ('*',(derive b) , a)
				)
   | _ as o -> o


and derive_fract = function
	(*  u/v -> u'*v  - v'*u  / v^2  *)
   | Frac(a,b) -> Frac ( 
			Binop ('-' , Binop('*',(derive a),b) , Binop ('*',( derive b ) , a ) )
			, Pow(b,Val(Num.Int(2))) 
			)
   | _ as o -> o


and derive_pow = function 
	(* u^a ->  au' * u^(a-1)  *)
    |Pow(u,a) -> Binop ('*' , 
			Binop ('*' , a , (derive u) )
			,Pow ( u , Binop ('-' , a , Val(Num.Int(1))) )
		)
(*   | Pow (x,n) -> Pow ( Binop('*',n,x) , Binop('-',n,1) )*)
   | _ as o -> o


and derive_sqrt = function
	(* Sqrt(a) -> u' / 2*Sqrt(u)  *)
   | Sqrt(a) -> Frac ( 
			(derive a ), 
			Binop ( '*' , Val(Num.Int(2)) , Sqrt(a) )
			)
   | _ as o -> o

and derive_exp = function
   | _ as o -> o

and derive_log = function
	(* log(val) -> 1/a  *)
   | Log(Val(_)) | Log(Var(_)) as x -> Frac (  Val(Num.Int(1)) , x )

	(* log(x) -> x'/x *)
   | Log(x) -> Frac (derive x, x )
   | _ as o -> o

and derive_cos =function

    | Cos(x) -> Unop('-' , Sin(x) )
    | _ as o -> o

and derive_sin =function
    | Sin(x) -> Unop('+' , Cos(x) )
    | _ as o -> o

and derive_tan =function
    | Tan(x) -> Binop('+' , 
			Val(Num.Int(1)) 
			, Pow ( Tan(x) , Val(Num.Int(2)) )
			)
    | _ as o -> o

and derive_acos =function
    | Acos(x) -> Unop('-' , 
			Frac (
				Val(Num.Int(1))
				, Sqrt ( 
					Binop ('-' , 
						Val(Num.Int(1)) 
						,Pow(x,Val(Num.Int(2)) ) )
					)
				)
			)
    | _ as o -> o

and derive_asin =function
    | Acos(x) -> Unop('+' , 
			Frac (
				Val(Num.Int(1))
				, Sqrt ( 
					Binop ('-' , 
						Val(Num.Int(1)) 
						,Pow(x,Val(Num.Int(2)) ) )
					)
				)
			)
    | _ as o -> o

and derive_atan =function
    | Atan(x) -> Frac (
			Val(Num.Int(1)) 
			,Pow(
				Binop ('+' , Val(Num.Int(1)) , x )
				,Val(Num.Int(2))
			)
		)
    | _ as o -> o

;;

(* give the n derivation of an math_expr *)
let  rec derive_n : math_expr -> int -> math_expr = 
  fun x y -> match y with
    | 0 -> x
(*    | y when y<0 -> raise Invalid_derive_n_Argument ("argument de derivation_n inferieur a 0 ") *)
    | y -> derive_n  (derive x) (y-1)

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
  | Unop('-',Val(Num.Int 0)) -> Val(Num.Int 0)
  | Unop('-',Unop('-',x)) -> simpl(x)
  | Unop('-', x) -> 
    (
      let xx = simpl x in 
      match xx with
	| Unop('-',y) -> y
	| _ -> Unop('-',xx)
    )
  | Unop(s,x) -> Unop(s,simpl(x))
  | _ as o -> o

(* Simplify a binary operation *)
and simpl_binop = function
  | Binop ('+',_,_) as bplus -> simpl_plus bplus
  | Binop ('-',_,_) as bminus -> simpl_minus bminus
  | Binop ('*',_,_) as bmult -> simpl_mult bmult
  | Binop(op,x,y) -> Binop(op,(simpl x),(simpl y))
  | _ as bf -> bf

(* Simplify additions *)
and simpl_plus = function
  (* x + 0 = x *)
  | Binop('+',x,Val(Num.Int(0))) | Binop('+',Val(Num.Int(0)),x) -> simpl(x)

  (* x + (-y) = x - y : y is a value *)
  | Binop('+',x,Val(Num.Int(y))) when y < 0 -> Binop('-',simpl(x),
						     Val(Num.Int(-y)))

  (* x + (-y) = 0 if x = y *)
  | Binop('+',x,Unop('-',y))
  | Binop('+',Unop('-',y),x) when x = y -> Val(Num.Int(0))

  (* x + (-y) = x - y *)
  | Binop('+',x,Unop('-',y)) | Binop('+',Unop('-',y),x) -> Binop('-',simpl(x),simpl(y))

  (* ln(a) + ln(b) *)
  | Binop('+',Log(a),Log(b)) -> simpl_log (Log(simpl_mult(Binop('*',a,b))))
 
  (* a² + 2ab + b² = (a + b)² *)
  | (Binop('+',Pow(a,p1),Binop('+',Binop('*',Val(Num.Int(2)),
					 Binop('*',aa,bb)),Pow(b,p2))) as i)
    when (p1 = p2) && (p1 = Val(Num.Int(2))) -> simpl_identity '+' i a aa b bb p1

  (* a² - 2ab + b² = (a + b)² *)
  | (Binop('+',Binop('-',Pow(a,p1),(Binop('*',Val(Num.Int(2)),
					  Binop('*',aa,bb)))),Pow(b,p2)) as i)
      when (p1 = p2) && (p1 = Val(Num.Int(2))) -> simpl_identity '-' i a aa b bb p1

  (* x + x*y = x * (y + 1) *)
  | Binop('+',x,Binop('*',y,Val(Num.Int(z))))
      when x = y -> simpl_mult(Binop('*',Val(Num.Int(z+1)),simpl(x)))

  (* x + y*x = x * (y + 1) *)
  | Binop('+',x,Binop('*',Val(Num.Int(z)),y))

  (* x*y + x = x * (y + 1) *)
  | Binop('+',Binop('*',y,Val(Num.Int(z))),x) 
      when x = y -> simpl_mult(Binop('*',Val(Num.Int(z+1)),simpl(x)))

  (* y*x + x = x * (y + 1) *)
  | Binop('+',Binop('*',Val(Num.Int(z)),y),x) 
      when x = y -> simpl_mult(Binop('*',Val(Num.Int(z+1)),simpl(x)))

  (* x + x*y = x * (y+z), z is an expression *)
  | Binop('+',x,Binop('*',y,z))
      when x = y -> simpl_mult(Binop('*',x,Binop('+',simpl(z),Val(Num.Int 1))))

  (* x + z*x = x * (y+z), z is an expression *)
  | Binop('+',x,Binop('*',z,y)) 
      when x = y -> simpl_mult(Binop('*',x,Binop('+',simpl(z),Val(Num.Int 1))))

  (* x + x = 2x *)
  | Binop('+',x,y) when x = y -> simpl_mult(Binop('*',Val(Num.Int 2),simpl(x)))

  (* ax + ay = a * (x + y) *)
  | Binop('+',Binop('*',a,x),Binop('*',b,y)) when a = b -> 
    Binop('*',simpl(a),simpl(Binop('+',x,y)))

  (* x + y : x and y are constant values *)
(*  | Binop('+',Val(Num.Int(_)),Val(Num.Int(_))) as b -> b*)

  (* Sum of x1 + x2 + ... + xn, x[1-n] are the same expression *)
  | Binop('+' as p,x,y) -> simpl_binop_aux p x y

  | _ as o -> o

(* Simplify substractions *)
and simpl_minus = function
  (* x - 0 = x *)
  | Binop('-',x,Val(Num.Int(0))) -> simpl(x)

  (* x - x = 0 *)
  | Binop('-',x,y) when x = y -> Val(Num.Int 0)
  
  (* 0 - x = -x *)
  | Binop('-',Val(Num.Int(0)),x) -> simpl_unop(Unop('-',simpl(x)))

  (* ln(a) - ln(b) *)
  | Binop('-',Log(a),Log(b)) -> simpl_log (Log(simpl_fract(Frac(a,b))))

  (* a² - b² *)
  | Binop('-',Pow(x,p1),Pow(y,p2))
      when p1 = p2 && p1 = Val(Num.Int(2)) -> 
    let xx = simpl(x) in let yy = simpl(y) in 
			 if xx <> yy 
			 then
			   Binop('*',Binop('+',xx,yy),Binop('-',xx,yy))
			 else
			   Binop('*',Val(Num.Int(2)),xx)

  (* x - (-y) = x + y *)
  | Binop('-',x,Unop('-',y)) -> simpl_binop(Binop('+',simpl(x),simpl(y)))

  (* x - z*x : z is a value *)
  | Binop('-',x,Binop('*',y,Val(Num.Int(z)))) 
      when x = y -> simpl_binop(Unop('-',Binop('*',Val(Num.Int(z-1)),simpl(x))))

  (* x - x*z : z is a value *)
  | Binop('-',x,Binop('*',Val(Num.Int(z)),y))
      when x = y -> simpl_binop(Unop('-',Binop('*',Val(Num.Int(z-1)),simpl(x))))

  (* x - x*y = (y+1)*y, y is an expression *)
  | Binop('-',x,Binop('*',y,z))
      when x = y -> simpl_binop(Unop('-',
				     Binop('*',x,Binop('-',simpl(z),
						       Val(Num.Int 1)))))
  (* x - y*x = (y+1)*y, y is an expression *)
  | Binop('-',x,Binop('*',z,y))
      when x = y -> simpl_binop(Unop('-',
				     Binop('*',x,Binop('-',simpl(z),
						       Val(Num.Int 1)))))
  (* -x - x = -2x *)
  | Binop('-',Unop('-',x),y) 
      when (x = y) -> Binop('*',Val(Num.Int (-2)),simpl(x))

  (* yx - x = -(y+1)x : y is a value *)
  | Binop('-',Binop('*',Val(Num.Int(z)),x),y) when x = y -> 
    simpl_binop(Binop('*',Val(Num.Int(z-1)),x))

  (* xy - x = -(y+1)x : y is a value *)
  | Binop('-',Binop('*',x,Val(Num.Int(z))),y) when x = y -> 
    simpl_binop(Binop('*',Val(Num.Int(z-1)),x))

  (* yx - x = -(y+1)x : y is an expression *)
  | Binop('-',Binop('*',z,x),y) when x = y -> 
    Binop('*',Binop('+',simpl(z),Val(Num.Int(1))),x)

  (* xy - x = -(y+1)x : y is an expression *)
  | Binop('-',Binop('*',x,z),y) when x = y -> 
    Binop('*',Binop('+',simpl(z),Val(Num.Int(1))),x)

  (* x - y : x and y are constant values *)
  | Binop('-',Val(Num.Int(_)),Val(Num.Int(_))) as b -> b

  (* x - y | -x -y : x and y are variables *)
  | Binop('-',Var(_),Var(_)) | Binop('+',Unop(_,_),Var(_)) 
  | Binop('-',Var(_),Unop(_,_))  as b -> b

  (* Sub: -x1 - x2 - ... - x[n-1] = n*x with x[0...n-1] as same expression *)
  | Binop('-' as m,(Binop('-',_,_) as x),y) -> simpl_binop_aux m x y
  | _ as o -> o


(* Simplify a² (+/-) 2ab + b² *)
and simpl_identity op id a aa b bb p =
  let a' = (simpl a) and aa' = (simpl aa)
  and b' = (simpl b) and bb' = (simpl bb) in
  if((a' = aa') && (b' = bb')) then
    Pow(Binop(op,simpl(a),simpl(b)),p)
  else
    id

(* Simplify multiply *)
and simpl_mult = function
(** TODO simplify the multiplication *)
  (* x * 1 = x *)
  | Binop('*',x,Val(Num.Int(1))) | Binop('*',Val(Num.Int(1)),x) -> simpl(x)

  (* x * 0 = x *)
  | Binop('*',x,Val(Num.Int(0))) | Binop('*',Val(Num.Int(0)),x) -> Val(Num.Int(0))

  (* x * (-1) = -x : x is a value *)
  | Binop('*',Val(Num.Int(x)),Val(Num.Int(-1))) 
  | Binop('*',Val(Num.Int(-1)),Val(Num.Int(x))) -> Val(Num.Int(-x))

  (* x * (-1) = -x :  x is an expression *)
  | Binop('*',x,Val(Num.Int(-1))) 
  | Binop('*',Val(Num.Int(-1)),x) -> simpl_unop(Unop('-',simpl(x)))

  (* x * y  = -(x*y) if and only if (x > 0, y < 0) *)
  |Binop('*',Val(Num.Int(x)),Val(Num.Int(y)))
      when (x > 0 && y < 0) -> simpl_unop(Unop('-',Binop('*',Val(Num.Int(x)),
							 Val(Num.Int(-y)))))
  (* x * y  = -(x*y) if and only if (x < 0, y > 0) *)
  |Binop('*',Val(Num.Int(x)),Val(Num.Int(y)))
      when (x < 0 && y > 0) -> simpl_unop(Unop('-',Binop('*',Val(Num.Int(-x)),
							 Val(Num.Int(y)))))
  (* x * y when x < 0 and y < 0 *)
  | Binop('*',Val(Num.Int(x)),Val(Num.Int(y))) 
      when (x < 0 && y < 0) -> Binop('*',Val(Num.Int(-x)),Val(Num.Int(-y)))

  (* e(a) * e(b) = e⁽a+b⁾ *)
  | Binop('*',Expo(a),Expo(b))-> simpl_exp(Expo(simpl_plus(Binop('+',
								 simpl_exp(a),
								 simpl_exp(b)))))

  (* ln(a)⁽¹/²⁾ = ln(sqrt(a)) *)
  | Binop('*',Log(a),Frac(Val(Num.Int(1)),Val(Num.Int(2)))) 
  | Binop('*',Frac(Val(Num.Int(1)),Val(Num.Int(2))),Log(a)) 
    -> Log(simpl_sqrt(Sqrt(a)))

  (* n * ln(x) *)
  | Binop('*',n,Log(x)) | Binop('*',Log(x),n)-> Log(simpl_pow(Pow(x,n)))

  (* x * x = x²*)
  | Binop('*',x,y) when x = y -> Pow(simpl(x),Val(Num.Int(2)))

  (* x * x^y = x⁽y+1⁾ : y is value *)
  | Binop('*',x,Pow(z,Val(Num.Int(y)))) | Binop('*',Pow(z,Val(Num.Int(y))),x)
      when x = z -> simpl_pow(Pow(simpl(x),Val(Num.Int(y+1))))

  (* x * x^y = x⁽y+1⁾ : y is an expression *)
  | Binop('*',x,Pow(z,y)) | Binop('*',Pow(z,y),x) 
      when x = z -> simpl_pow(Pow(simpl(x),Binop('+',simpl(y),Val(Num.Int(1)))))

  (* x^a * x^b = x⁽a+b⁾ : a  and b are values *)
  | Binop('*',Pow(x,Val(Num.Int(a))),Pow(y,Val(Num.Int(b)))) 
      when x = y -> simpl_pow(Pow(simpl(x),Val(Num.Int(a+b))))

  (* x^a * x^b = x⁽a+b⁾ : a and b are expresions *)
  | Binop('*',Pow(x,a),Pow(y,b)) 
      when x = y -> simpl_pow(Pow(simpl(x),
				  simpl_plus(Binop('+',simpl(a),simpl(b)))))

  (* Product of x1 * x2 * ... * xn, x[1-n] are the same expression *)
  | Binop('*' as m,x,y) -> simpl_binop_aux m x y
  | _ as o -> o

    
(* 
   Auxiliary function of simpl_binop when an operation 
   is applied on the same term n times ('+','-', '*')
*)
and simpl_binop_aux op x y = 
  let t = simpl x in let z = simpl y in 
		     let ex = (Binop(op,t,z)) in
		     match op with   
		       | '+' -> if z <> y then simpl_plus(ex) else ex
		       | '-' -> (*if z <> y then simpl_minus(ex) else ex*)
			 (
			   match t,z with (* We have Binop of '-' * t * z *)
			     | ((Binop('*',Val(Num.Int(i)),a),b))
				 when a = b -> Binop('*',Val(Num.Int(i-1)),a)
			     | (_,_) -> ex
			 )  
		       | '*' -> if z <> y then simpl_mult(ex) else ex
		       | _ -> failwith "Invalid Operation to simplify"


(* Simplify a fraction *)
and simpl_fract = function
  (** TODO simplify the fraction *)
  (* exp(a)/exp(b) -> exp(a-b) *)
  | Frac(Expo(a) , Expo(b) ) -> Expo(Binop('-',(simpl a),(simpl b)))
    
  (* 1/exp(b) -> exp(-a) *)
  | Frac(Val(Num.Int(1)),Expo(a) ) -> Expo(Unop('-', (simpl a)))

  (*  simplification de base 5/25 -> 1/5   |  25/5  -> 5  |  74/12 -> 37/6 *)
  | Frac(Val(Num.Int(a as a2) ),Val(Num.Int(b as b2) ) ) as fract-> 
    begin match a2,b2 with
      | (a,b) when a mod b=0 -> Val(Num.Int(a/b))
      | (a,b) when b mod a=0 -> Frac(Val(Num.Int(1)),Val(Num.Int(b/a)) )
      | (a,b) -> let coeff=(pgcd a b) in if coeff=0 then fract
	else Frac(
	  Val(Num.Int(a/coeff)) ,	Val(Num.Int(b/coeff))
	)
    end

  (* 1/log(a) -> -log(a) *)
  | Frac(Val(Num.Int(1)),Log(a)) -> Unop('-', Log(simpl a))
  | _ as o -> o 

(* Simplify a power *)
(** TODO simplify the power *)
and simpl_pow = function
  (* x^(-1) = 1/x *)
  | Pow(x,Val(Num.Int(n))) when n < 0 -> Frac(Val(Num.Int(-n)),(simpl x))

  (* 0^0 = 0 *)
  | Pow(Val(Num.Int(0)),Val(Num.Int(0))) -> Val(Num.Int(1))

  (* x^0 = 1 *)
  | Pow(x,Val(Num.Int(0))) -> Val(Num.Int(1))

  (* 0^n = 0 *)
  | Pow(Val(Num.Int(0)) as x,n) -> simpl(x)

  (* 1^n = 1 *)
  | Pow((Val(Num.Int(1)) as x),n) -> simpl(x)

  (* x^n *)
  | Pow(x,n) -> 
    (
      let xx = simpl x in 
      let yy = simpl n in 
      if xx = x && yy = n then Pow(xx,yy) 
      else simpl_pow(Pow(xx,yy))
    )
  | _ as o -> o 

(* Simplify a square root *)
and simpl_sqrt = function
  | _ as o -> o 

(* Simplify a exponential function *)
and simpl_exp = function
  (** TODO simplify the exponential *)
  (* exp(0) -> 1    | exp(1) -> e *)
  |Expo(Val(Num.Int(0))) -> Val(Num.Int(1))
  |Expo(Val(Num.Int(1))) -> Exp0
  | _ as o -> o 

(* Simplify the logarithm *)
and simpl_log = function
(** TODO simplify the normal logarithm *)
  (* log(1) -> 0 *)
  | Log( Val(Num.Int(x)) ) when x=1 -> Val(Num.Int(0))

	(* log (e) -> 1 *)
  | Log( Exp0 ) -> Val(Num.Int(1))
  | _ as o -> o 

(* Simplify a trigonometric function *)
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
