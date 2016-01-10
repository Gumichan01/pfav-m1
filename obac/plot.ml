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
	
let my_ref = ref 0;;

let augmenter () = my_ref:=(!my_ref)+1;;

let plotExt:math_expr -> string -> int -> int -> int -> int-> unit =fun exp x a b c d->
	if not(plotTest exp x) then failwith "The Expr have more than only 1 VAR " else

	print_string "\nWelcome to Plot\n";

	(* Dimension WINDOWS *)
	let coeff = 1 in
	let a1 =(a*coeff) in let b1 =(b*coeff) in let c1 =(c*coeff) in let d1 =(d*coeff) in

	let dim = fourIntToStringDimension a1 b1 c1 d1 in

	(* number of point to eval	*)
	let n = (abs(a)+abs(b)) in

	(* eval f(i) and return a tuple	*) 
	let  evalPoint i=
		let x1 = i in
		let expSub = subst exp x (Val(Num.Int(x1))) in 
(*		print_string("math_expr: "^(print_tree_of_math expSub)^"\n");	*)
		let y1Float = eval expSub in
		let y1 =int_of_float y1Float in
		print_string ":";
		print_int y1;
		(x1,y1) in


	(* array with the points 	*)
	let data =Array.make (n+1) (0,0) in

	(* init array*)
	for i=a to b do
		print_int i;

		data.(!my_ref) <- evalPoint i;
		print_string "\n";
		augmenter();
	done;
(*	let data = Array.init n (fun i -> evalPoint ) in *)

(*	let compare (x1,y1 ) (x2,y2) = x1-x2 in *)

(*	let () = Array.sort compare data in *)

	open_graph dim;
	(* Axe vertical et Horizontal *)
	set_line_width 1;
	let haut =abs c1 in
	let centre =abs a1 in
	moveto 0 haut;
	set_color red;
	lineto ( abs(a1)+abs(b1) ) haut;
	moveto centre 0;
	set_color blue;
	lineto centre (( abs(c1)+abs(d1)));
	set_line_width 2;

	(* Ligne reliant les Points resultat de evalPoint et enregistré dans Array *)
	set_color green;
	moveto 0 0;
	let (x0,y0) = data.(0) 
	in moveto (x0) (y0);
	for i=1 to n-1 do 
		let (x,y) = data.(i) in 
		lineto (x*coeff) (y*coeff)
	done;
	ignore (read_key ());;
	

let plot:math_expr -> string-> unit =fun exp x ->
	(plotExt exp x (-5) 5 (-5) 5);;
