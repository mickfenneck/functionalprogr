let lista = [1;2;3;4];;

lista;;

(* int list = [1; 2] *)
1::[2];;
(* int list = [1] *)
1::[];;
(* int list = [1] *)
List.hd lista;;
(* nt list = [2; 3; 4] *)
List.tl lista;;

let rec length lst = 
	if lst = [] then 0
	else 1 + length(List.tl lst);;
	
let rec length = function
	| [] -> 0
	| x::rest -> 1 + length rest;;
	
let rec sumof = function
	| [] -> 0
	| x::rest -> x + sumof rest;;
	
exception EmptyList;;

let rec maxList = function 
	| [] -> raise EmptyList
	| [x] -> x
	| x::rest -> max x (maxList rest);;
	
maxList lista;;

let rec maxl = function 
	| [] -> raise EmptyList
	| [x] -> x
	| x::y::rest -> maxl((max x y) ::rest);;
	
let rec upto (x,y) =
	if x > y then []
	else x::upto(x+1,y);;
	
let rec take n = function
	| [] -> []
	| x::xs -> if n <= 0 then [] else
		x::take (n-1) xs;; 
		
(* forma non currificata *)
let rec take (n,list) = 
	match list with
	| [] -> []
	| x::rest -> if n <= 0 then [] else
		x::take ((n-1), rest);; 

let rec drop n = function
	| [] -> []
	| x::xs -> if n <= 0 then x::xs
		else drop (n-1) xs;;

(* pattern matching con AS *)
let rec drop n = function
	| [] -> []
	| _::xs as lst -> if n <= 0 then lst
		else drop (n-1) xs;;

let rec append 	prima seconda =
	match prima with
	| [] -> seconda
	| x::rest -> x::append rest seconda;;
	
(* concatenazione predefinita in ocaml con prima @ seconda *)

let in_coda x lst =
	lst @ [x];;

let rec reverse = function
	| [] -> []
	| x::xs -> (reverse xs) @ [x];;

exception Nth;;

let rec nth n = function
	| [] -> raise Nth
	| x ::xs -> if n = 0 then x
	else nth (n-1) xs;;

let itlen lst =
	let rec aux result = function
		| [] -> result
		| _::xs -> aux (result+1) xs
	in aux 0 lst;;
	

















		

