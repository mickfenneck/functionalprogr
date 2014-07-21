(*somma ore: esempio did ivisione in sottoproblemi*)
let somma_ore (ore1,minuti1) (ore2,minuti2) =
	let somma_minuti =
		minuti1 + minuti2
	in (ore1 + ore2 + somma_minuti / 60, somma_minuti mod 60);;
	
let orario = somma_ore (1,15) (2,47);;

(*NO INTERNET!!!! cerco gestione date ocaml *)
(*let giorno_succ (giorno,mese) =
	if febbraio
		then if
	else if mese = novembre or mese = aprile or mese = giugno or mese = settembre
		then
	else *)

(*RICORSIONE*)

let rec fact = function
	0 -> 1
	| n -> n * fact (n-1);;
	
fact 4;;

let rec max_n n = 
	if n <= 0 then 0
	else let x = read_int()
		in if n = 1 then x
		else max x (max_n (n-1));;
		
(* max_of: unit -> int *)
let rec max_of () =
	let x = read_int()
	in 	if x <= 0 then x
		else max x (max_of());;

(* tutti monori di _ ver1 *)		
let rec tutti_minori_di n =
	let stringa = read_line()
	in 	if stringa = "" then true
		else let k = int_of_string stringa
				in k < n && tutti_minori_di n;;

(* tutti monori di _ ver2 *)		
let rec tutti_minori_di n =
	let stringa = read_line()
	in 	if stringa = "" then true
		else let k = int_of_string stringa
				in let result = tutti_minori_di n
				in k < n && result;;		
				
let rec sum_to n =
	if n = 0 then 0
	else n+(sum_to n-1);;


let rec sum_to_neg n =
	if n = 0 then 0
	else n+(sum_to_neg n+1);;	


exception Fermati;;
(* somma dei numeri negativi *)
let rec sum_to_neg n =
	if n > 0 then raise Fermati
	else if	n = 0 then 0
	else n + sum_to_neg (n+1);; (* <---- WTF *)
	
sum_to_neg (-5);;
	
	
	
	
let rec ripeti_stringa str n = 
	if n = 0 then ""
	else str^(ripeti_stringa str (n-1));;

let rec ripeti_stringa (str,n) = 
	if n = 0 then ""
	else str^ripeti_stringa (str,n-1);;
	
	
(* k ^ n *)
let rec pow (k,n) =
	if n < 0 then raise NegativeNumber
	else if n = 0 then 1
	else k*pow(k,n-1);;

pow (2,5);;	
pow(2,-7);;	
pow(-2,5);;

(* sum between n and m, n < m *)
let rec sum_between (n,m) = 
	if n > m then 0
	else if m = n then m
	else m + sum_between(n,m-1);;
	
sum_between (5,7);;
	
				
						
(* TORRE DI HANOI *)
let move (x,y) =
	"Sposto un disco da "^x^" a "^y^"\n";;
	
let rec hanoi_generale (n,inizio,fine,appoggio) =
	if n = 0 then ""
	else 	hanoi_generale(n-1,inizio,appoggio,fine)
			^ (move (inizio,fine))
			^ (hanoi_generale (n-1,appoggio,fine,inizio));;
			
hanoi_generale (4,"a","b","c");;
	
	
(* EXCEPTION *)
exception NegativeNumber;;

let rec fact n =
	if n < 0 then raise NegativeNumber 
	else if n = 0 then 1
	else n * fact(n-1);;
	
fact(-1);;

(* cattura eccezioni *)
try 4 * fact(-1) 
	with NegativeNumber -> 0;;

let rec aux tot =
	try
		let x = read_int()
		in aux (tot+x)
	with Failure "int_of_string" -> tot
	in aux 0;;
	
	









