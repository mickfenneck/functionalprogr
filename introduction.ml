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

(* greater than *)
(* val greaterthan : 'a -> 'a -> bool = <fun> *)
let greaterthan x y =
	x > y;;
	
(* creazione operatore 'ipotenusa' *)
(* val ( /& ) : float -> float -> float = <fun> *)
let (/&) x y =
	sqrt(x**2.0 +. y**2.0);;	
(* call example: 2. /& 4. *)

(* coppie e currificazione *)
(* val first1 : 'a * 'b -> 'a = <fun> *)
let first1 (x,y) =
	x;;
(* val first2 : 'a -> 'b -> 'a = <fun> *)
let first2 x y = 
	x;;
	
(* ESERCIZI *)

(* val swap : 'a * 'b -> 'b * 'a = <fun> *)
let swap (x,y) =
	(y,x);;
(* val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b = <fun> *)
let map f (x,y) =
	(f x, f y);;
(* val plus1 : int * int -> int * int = <fun> *)
let plus1 (x,y) = 
	map succ (x,y);;
(*  val max : 'a * 'a -> 'a = <fun> *)
let max (x,y) = 
	if (x > y)
		then x
	else 
		y;;
(* val pi : float = 3.14159265358979312 *)
let pi = 4. *. atan 1.;;
(* val areacerchio : float -> float = <fun> *)
let areacerchio r = 
	pi *. (r**2.0);;
(* val diffcerchi : float -> float -> float = <fun> *)
let diffcerchi a b = 
	if (a > b)
		then ((a/.(2.*.pi))**2.0)*.pi -. ((b/.(2.*.pi))**2.0)*.pi
	else  ((b/.(2.*.pi))**2.0)*.pi -. ((a/.(2.*.pi))**2.0)*.pi;;
	
(* TEST INFERENZA DI TIPI *)
(* val f1 : ('a -> 'b) -> 'a -> ('a -> 'b) * 'a * 'b = <fun> *)
let f1 g f =
	(g,f, g f);;
(* val f2 : int -> string -> bool = <fun> *)
let f2 x y = 
	String.length y = x;;
(* val f3 : (float -> float) -> float -> float = <fun> *)
let f3 g x = 
	x +. (g x);;
(* : 'a -> ('a -> 'b) -> 'b = <fun> *)	
(fun x y -> y x);;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	





