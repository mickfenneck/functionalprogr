(* even number *)
(* val even : int -> bool = <fun> *)
let even n = 
	if n mod 2 = 0
		then true 
	else
		false;;

(* odd number *)
(* val odd : int -> bool = <fun> *)
let odd n = 
	not (even n);;

(* Print whether a number is divisible by 2 and by 3 *)
(* val print_divisibility : int -> unit = <fun> *)
let print_divisibility n =
	if even n then
		let out = "Pari" in
			if n mod 3 = 0
				then print_string (out^" e divisibile per 3")
			else
			 	 print_string (out^" ma non divisibile per 3")
	else
		let out = "Pari" in
			if n mod 3 = 0
				then print_string (out^" e divisibile per 3")
			else
			 	 print_string (out^" ma non divisibile per 3");;

(* val apply : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun> *)
let apply f (n,m) =
	(f n m);;

(* val conditional_apply : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a = <fun> *)
let conditional_apply f g x =
	if f x
		then g x
	else x;;
