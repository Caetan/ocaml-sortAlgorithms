

(***********************************************************************


	Developed by Caetán Tojeiro Carpente (caetantojeiro95@gmail.com)


************************************************************************)



(*----------------------------------------*)

(*------------AUX FUNCTIONS-------------*)


let rec rlist r n =
	if n<=0 then []
	else Random.int r :: rlist r (n-1);;
	
	
let rlist_t r n =
	let rec aux r n acc = 
		if n<=0 then acc
		else aux r (n-1) (Random.int r ::acc)
	in aux r n [];;
	
	
let rec insert x = function
	[] -> [x]
	| h::t -> if x<= h then x::h::t
				else h::insert x t;;
				
let insert_t x l =
	let rec aux x l l2 flag = match l,flag with
		_,false -> List.rev_append l2 l
		| [],_ -> List.rev_append  l2 [x]
		| h::t, true -> if x<=h then aux x t (h::x::l2) false
					else aux x t (h::l2) true
	in aux x l [] true;;

(*----------------------------------------------------------*)

		
		(*ISORT*)
let rec isort = function
	[] -> []
	| h::t -> insert h (isort t);;
	
	

let insert_gen_t f x l =
	let rec aux f x l l2 flag = match l,flag with
		_,false -> List.rev_append l2 l
		| [],_ -> List.rev_append  l2 [x]
		| h::t, true -> if f x h then aux f x t (h::x::l2) false
					else aux f x t (h::l2) true
	in aux f x l [] true;;


let rec isort_gen f = function
	[] -> []
	| h::t -> insert_gen_t f h (isort_gen f t);;


let isort_t l = 
	let rec aux l acc = match l with
		[] -> acc
		| h::t -> aux t (insert_t h acc)
	in aux l [];;
	

	
let isort_t_gen f l = 
	let rec aux f l acc = match l with
		[] -> acc
		| h::t -> aux f t (insert_gen_t f h acc)
	in aux f l [];;


(*----------------------------------------------------------*)

let rec divide = function
	h1::h2::t -> let t1,t2 = 
					divide t
				in h1::t1, h2::t2
	| l -> l, [];;

let divide_t l =
  let rec aux acc acc2 = function
					  [h] -> (List.rev (h::acc), List.rev acc2)
					| [] -> (List.rev acc, List.rev acc2)
					| h::t -> aux (h::acc) (List.hd t::acc2) (List.tl t)
				in aux [] [] l;;		


let rec merge l1 l2 = match l1,l2 with
	[], l | l,[] -> l
	| h1::t1, h2::t2 -> if h1<=h2 then h1::merge t1 l2
						else h2::merge l1 t2;;
						
						
let merge_t l1 l2 = match l1,l2 with
	[], l | l,[] -> l
	| [], [] -> []
	| h1::t1, h2::t2 -> let rec aux l1 l2 acc = match l1,l2 with
							[h], [] | [], [h] -> List.rev (h::acc)
							| [], h::t -> aux [] t (h::acc)
							| h::t,[] -> aux t [] (h::acc)
							| h1::t1, h2::t2 -> if h1<=h2 then aux t1 l2 (h1::acc)
												else aux l1 t2 (h2::acc)
						in aux l1 l2 [];;
(*----------------------------------------------------------*)						
	
	
	(*MSORT*)					
let rec msort l = match l with
	[] | [_] -> l
	| _ -> let l1,l2 = 
				divide l 
			in merge (msort l1) (msort l2);;
			
			
let merge_gen_t f l1 l2 = match l1,l2 with
	[], l | l,[] -> l
	| [], [] -> []
	| h1::t1, h2::t2 -> let rec aux f l1 l2 acc = match l1,l2 with
							[h], [] | [], [h] -> List.rev (h::acc)
							| [], h::t -> aux f [] t (h::acc)
							| h::t,[] -> aux f t [] (h::acc)
							| h1::t1, h2::t2 -> if f h1 h2 then aux f t1 l2 (h1::acc)
												else aux f l1 t2 (h2::acc)
						in aux f l1 l2 [];;
						
			
let rec msort_gen f l = match l with
	[] | [_] -> l
	| _ -> let l1,l2 = 
				divide l 
			in merge_gen_t f (msort_gen f l1) (msort_gen f l2);;


let rec msort_qt l = match l with
	[] -> []
	| [h] -> [h]
	| _ -> let drch, izq =
				divide_t l
			in merge_t (msort_qt drch) (msort_qt izq);;
			
			
let rec msort_qt_gen f l = match l with
	[] -> []
	| [h] -> [h]
	| _ -> let drch, izq =
				divide_t l
			in merge_gen_t f (msort_qt_gen f drch) (msort_qt_gen f izq);;


(*----------------------------------------------------------*)

	(*QSORT*)
let rec qsort l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition ((<=) h) t 
			in qsort l2 @ (h::qsort l1);;
			
			
let rec qsort_gen f l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition ((f) h) t 
			in (qsort_gen f l2) @ (h::qsort_gen f l1);;
			

let rec qsort_qt l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition ((<=) h) t 
			in let append l1 l2 =
				List.rev_append (List.rev l1) l2
			in append (qsort_qt l2) (h::qsort_qt l1);;
		
			
		
let rec qsort_qt_gen f l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition (f h) t 
			in let append l1 l2 =
				List.rev_append (List.rev l1) l2
			in append (qsort_qt_gen f l2) (h::qsort_qt_gen f l1);;
