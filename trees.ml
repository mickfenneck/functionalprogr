type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let rec count = function
	Empty -> 0
	| Tr(_,s,d) -> 1 +count s + count d;;
	
	
let tr = 
Tr(7,
	Tr(5,
		Tr(6,Empty,Empty),
		Tr(2,Empty,Empty)),
	Tr(8,
		Tr(2, 
			Tr(1,Empty,Empty), 
			Empty),
		Empty)
	);;
	
let rec preorder = function
	Empty -> []
	| Tr(x, l, r) ->
		x::(preorder l @ preorder r);;

exception EmptyTree;;

let is_empty = function
	| Empty -> raise EmptyTree
	| _ -> false;;
	
let root = function
	| Empty -> raise EmptyTree
	| Tr(x,_,_) -> x;;
	
let left = function
	| Empty -> raise EmptyTree
	| Tr(_,t,_) -> t;;
	
let right = function 
	| Empty -> raise EmptyTree
	| Tr(_,_,t) -> t;;
	
let rec size = function
	| Empty -> 0
	| Tr(_,t1,t2) -> 1 + size t1 + size t2;;
	
let rec height = function
	| Empty -> 0
	| Tr(_,t1,t2) -> 1 + max(height t1) (height t2);;
	
let rec reflect = function
	| Empty -> Empty
	| Tr(x,t1,t2) -> Tr(x,reflect t2, reflect t1);;
	
(* costruzione albero completo *)
let fulltree n =
	let rec aux (k,n) = 
		if n = 0 then Empty
		else Tr(k,aux(2*k,n-1),aux(2*k+1,n-1))
	in aux (1,n);;
	
(* tree print *)
let treeprint t = 
	let rec aux = function
		| (ind,Empty) -> print_string (ind ^ "Empty")
		| (ind,Tr(x,Empty,Empty)) ->
			print_string (ind^"Tr(" ^ string_of_int x ^ ",Empty,Empty)")
		| (ind,Tr(x,t1,t2)) ->
			(print_string(ind^"Tr(" ^ string_of_int x ^ ",\n");
			aux("   "^ind,t1); print_newline();
			aux("   "^ind,t2); print_string ")")
	in aux("",t); print_newline();;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

