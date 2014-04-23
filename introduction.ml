(* x^2 *)
(* val square : int -> int = <fun> *)
let square x =
	x*x;;

(* x+1 *)
(* val succ : int -> int = <fun> *)
let succ x =
	x + 1;;

(* f g x *)
(* val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)
let comp f g x = 
	f (g x);;

(* x^2+x^2 *)
(* val sumsquare : int -> int = <fun> *)
let sumsquare x = 
	square x + square x;;

(* (x+1)^2 *)
(* val succsquare : int -> int = <fun> *)
let succsquare x =
	comp square succ x;;
	
(* (x^2+1)^2 *)
(* val squaresuccsquare : int -> int = <fun> *)
let squaresuccsquare x = 
	comp square (comp succ square) x;;
