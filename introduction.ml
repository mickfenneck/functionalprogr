(* x^2 *)
let square x =
	x*x;;
(* x+1 *)
let succ x =
	x + 1;;
(* f g x *)
let comp f g x = 
	f (g x);;
(* x^2+x^2 *)
let sumsquare x = 
	square x + square x;;
(* (x+1)^2 *)
let succsquare x =
	comp square succ x;;
(* (x^2+1)^2 *)
let squaresuccsquare x = 
	comp square (comp succ square) x;;
