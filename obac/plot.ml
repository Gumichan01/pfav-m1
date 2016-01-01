open Expr;;
open Mathexpr;;

open Graphics;;

let plot exp x =

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
	set_color black;
	set_line_width 1;
	moveto 0 100;
	lineto 200 100;
	moveto 100 0;
	lineto 100 200;
	set_line_width 2;
	set_color green;
	moveto 0 0;
	let (x0,y0) = data.(0) in moveto x0 y0;
	for i=1 to n-1 do 
		let (x,y) = data.(i) in 
		lineto x y
	done;
	ignore (read_key ());;
