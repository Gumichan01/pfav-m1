open Expr;;
open Mathexpr;;
open Graphics;;


(* create a string from 4 int to be use at argument of open_graph *)
let fourIntToStringDimension a b c d =
	let horizon=abs(a)+abs(b)
	in
	let verti=abs(c)+abs(d)
	in
	" "^(string_of_int horizon)^"x"^(string_of_int verti);;
	

let plotExt:math_expr -> string -> int -> int -> int -> int-> unit =fun exp x a b c d->
	if not(plotTest exp x) then failwith "The Expr have more than only 1 VAR " else

	print_string "\nWelcome to Plot\n";	
	
	(* Dimension WINDOWS *)
	let a1 =200 in let b1 =150 in let c1 =300 in let d1 =120 in

	let dim = fourIntToStringDimension a1 b1 c1 d1 in

	(* number of point to eval *)
	let n = (abs(a)+abs(b)) in

	let  evalPoint =
		let x1 = n in
		let x1String =string_of_int x1 in
		let expSub = subst exp x1String (Val(Num.Int(x1))) in 
		let y1Float = eval expSub in
		let y1 =int_of_float y1Float in
		(x1,y1) in

	let data = Array.init n (fun i -> evalPoint ) in

(*	let compare (x1,y1 ) (x2,y2) = x1-x2 in *)

(*	let () = Array.sort compare data in *)

	open_graph dim;
	(* Axe vertical et Horizontal *)
	set_line_width 1;
	moveto 0 (( abs(c1)+abs(d1))/2);
	set_color red;
	lineto (abs(c1)+abs(d1)) (( abs(c1)+abs(d1))/2);
	moveto (( abs(a1)+abs(b1))/2) 0;
	set_color blue;
	lineto (( abs(a1)+abs(b1))/2) (( abs(c1)+abs(d1)));
	set_line_width 2;

	(* Ligne reliant les Points resultat de evalPoint et stocke dans Array *)
	set_color green;
	moveto 0 0;
	let (x0,y0) = data.(0) in moveto x0 y0;
	for i=1 to n-1 do 
		let (x,y) = data.(i) in 
		lineto x y
	done;
	ignore (read_key ());;
	

let plot:math_expr -> string-> unit =fun exp x ->
	(plotExt exp x (-5) 5 (-5) 5);;
