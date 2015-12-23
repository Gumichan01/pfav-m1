

(** Derivative and integration *)

open Mathexpr;;


(* Derive an expression
let rec derive : math_expr -> string -> math_expr = 
fun x s -> match x with
  | _ -> failwith "TODO derive : math_expr -> string -> math_expr ";;
 *)



(* TODO revoir derivation, signature non conforme vis-à-vis de l'énoncé *)
let rec derive : math_expr -> math_expr = 
  fun x -> match x with

    (* The derived form of constant values is 0 *)
    | Pi | Exp0 | Val(_) -> Val(Num.Int(0))
    (* The derived form of a variable is 1 *)
    | Var(_) -> Val(Num.Int(1))
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


and derive_unop = function
   | Unop(op,x) -> Unop (op, (derive x))
   | _ as o -> o


and derive_binop = function
   |Binop ('+',a,b) -> Binop ('+', (derive a) , (derive b))
   |Binop ('-',a,b) -> Binop ('-', (derive a) , (derive b))
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


(* OPTIONAL :  give the n derivation of an math_expr *)
let  rec derive_n : math_expr -> int -> math_expr = 
  fun x y -> match y with
    | 0 -> x
(*    | y when y<0 -> raise Invalid_derive_n_Argument ("argument de derivation_n inferieur a 0 ") *)
    | y -> derive_n  (derive x) (y-1)





(* Integration of an expression *)
let rec integ : math_expr -> string -> math_expr -> math_expr -> math_expr = 
fun x s a b -> match x with
  | _ -> failwith "TODO integ : math_expr -> string -> math_expr -> math_expr -> math_expr ";;
