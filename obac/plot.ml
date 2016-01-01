open Expr;;
(*#use "expr.mli";*)
open Graphics;;


let display () =
print_string "\nWelcome to Plot\nGive a Number of point : ";	
let n = read_int () in

let read_pair () =
	let x = read_int () in
	let y = read_int () in
	(x,y) in

let data = Array.init n (fun i -> read_pair ()) in

let compare (x1,y1 ) (x2,y2) = x1-x2 in

let () = Array.sort compare data in

	open_graph " 200x200";
	set_line_width 3;
	let (x0,y0) = data.(0) in moveto x0 y0;
	for i=1 to n-1 do 
		let (x,y) = data.(i) in 
		lineto x y
	done;
	ignore (read_key ());;
