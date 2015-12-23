
open Mathexpr;;

(* Calculation of the Greatest common dividor *)
let rec gcd x y = if y = 0 then x else gcd  y (x mod y) ;;

let rec extend_gcd x y =
	if y = 0 
	then 
	  (1, 0, x)
	else 
	let q = (x/y) in 
	let (u,v,g) = extend_gcd y (x - q*y) in
	(v,u-q * v,g)
;;

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
    | _ as o -> simpl_trigo o

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
  (* x + y : x and y are constant values *)
  | Binop('+',Val(Num.Int(x)),Val(Num.Int(y))) as b
    when x <> 0 && y <> 0 -> b
  
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

  (* Sum of x1 + x2 + ... + xn, x[1-n] are the same expression *)
  | Binop('+' as p,x,y) -> simpl_binop_aux p x y

  | _ as o -> o

(* Simplify substractions *)
and simpl_minus = function
  (* x - y : x and y are non-zero constant values *)
  | (Binop('-',Val(Num.Int(x)),Val(Num.Int(y))) as b) 
      when x <> 0 && y <> 0 -> b
  
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

  (* exp(a)/exp(b) -> exp(a-b) : a and b are constant values *)
  | Frac(Expo(Val(Num.Int(a))),
	 Expo(Val(Num.Int(b)))) when a <> b -> simpl_exp(Expo(Val(Num.Int(a-b))))

  (* exp(a)/exp(b) -> exp(a-b) : a and b are expressions *)
  | Frac(Expo(a),Expo(b)) when a <> b -> Expo(Binop('-',(simpl a),(simpl b)))

  (* x^y / x^z : = x^(y-z) y and z are constant values *)
  | Frac(Pow(x,Val(Num.Int(a))),Pow(y,Val(Num.Int(b)))) 
      when (x = y && a <> b) -> Pow(x,Val(Num.Int(a-b)))

  (* (x^y)/(x^z) : = x^(y-z) y and z are expression *)
  | Frac(Pow(x,a),Pow(y,b)) 
      when (x = y && a <> b) -> Pow(x,simpl_minus(Binop('-',simpl(a),simpl(b))))

  (* a/b = 1 when a = b  *)
  | Frac(a,b) when a = b -> Val(Num.Int(1))

  (* 1/exp(b) = exp(-a) *)
  | Frac(Val(Num.Int(1)),Expo(a)) -> Expo(Unop('-', (simpl a)))

  (* Reduce the fraction 3/9 = 1/3 *)
  | Frac(Val(Num.Int(x)),Val(Num.Int(y))) as f-> reduce_fraction f x y 

  (* 1/log(a) = -log(a) *)
  | Frac(Val(Num.Int(1)),Log(a)) -> Unop('-', Log(simpl a))
  | _ as o -> o 



and reduce_fraction frac x y =
  match (x mod y) with
    | 0 -> Val(Num.Int(x/y))
    | _ -> let gcd_ = (gcd x y) in 
	   if gcd_ = 0 
	   then 
	     frac
	   else 
	     Frac(Val(Num.Int(x/gcd_)),Val(Num.Int(y/gcd_)))



(* Simplify a power *)
(** TODO simplify the power : a^(1/n) = sqrt nième de a *)
and simpl_pow = function
  (* x^1 = x*)
  | Pow(x,Val(Num.Int(1)))-> simpl(x)

  (* x^(-1) = 1/x *)
  | Pow(x,Val(Num.Int(-1))) 
  | Pow(x,Unop('-',Val(Num.Int(1))) ) -> Frac(Val(Num.Int(1)),(simpl x))

  (* x^(-n) = 1/x *)
  | Pow(x,Unop('-',y)) -> Frac(Val(Num.Int(1)),Pow((simpl x),y))

  (* 0^0 = 0 by convention *)
  | Pow(Val(Num.Int(0)),Val(Num.Int(0))) -> Val(Num.Int(1))

  (* x^0 = 1 *)
  | Pow(x,Val(Num.Int(0))) -> Val(Num.Int(1))

  (* 0^n = 0 *)
  | Pow(Val(Num.Int(0)),n) -> Val(Num.Int(0))

  (* 1^n = 1 *)
  | Pow(Val(Num.Int(1)),n) -> Val(Num.Int(1))

  (* x^(1/2) = sqrt(x) *)
  | Pow(x,Frac(Val(Num.Int(1)),Val(Num.Int(2)))) -> Sqrt((simpl x))

  (* x^(a*b) = (x^a)^b *)
  | Pow(x,Binop('*',a,b)) -> Pow(Pow(simpl(x),simpl(a)),simpl(b))

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

	(* exp*)
  | _ as o -> o 

(* Simplify the logarithm *)
and simpl_log = function
(** TODO simplify the normal logarithm *)
  (* ln(1) = 0 *)
  | Log(Val(Num.Int(1))) -> Val(Num.Int(0))

  (* ln(e) = 1 *)
  | Log(Exp0) -> Val(Num.Int(1))

  (* ln(exp(x)) *)
  | Log(Expo(x)) -> simpl(x)
  | _ as o -> o

(* Simplify a trigonometric function *)
and simpl_trigo = function
  | _ as o -> o 

;;
