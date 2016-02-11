type 'a heap = 'a list (* ANSWER *);;

let empty () = [] (* ANSWER *);;

let insert x xs =
	let rec insert_in_order y lst = 
		match lst with 
		| [] -> x :: []
		| hd :: tl -> 
			if (y < hd) then y :: hd :: tl
			else hd :: (insert_in_order y tl)
	in insert_in_order x xs (* ANSWER *);;

let find_min xs = 
	match xs with
	| [] -> None
	| x :: xs -> Some (x, xs) (* ANSWER *);;

let as_sorted_list xs = xs (* ANSWER *);;