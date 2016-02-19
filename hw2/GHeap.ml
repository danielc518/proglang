type 'a heap = Empty | Node of 'a * 'a heap * 'a heap (* ANSWER *);;

let empty () = Empty (* ANSWER *);;

let insert x xs =
	let rec insert_heap new_value heap = 
		match heap with 
		| Empty -> Node(new_value, Empty, Empty)
		| Node(value, left_node, right_node) -> 
			if new_value <= value then 
				let new_left_node = insert_heap value right_node in
				Node(new_value, new_left_node, left_node)
			else 
				let new_left_node = insert_heap new_value right_node in
				Node(value, new_left_node, left_node)
	in insert_heap x xs (* ANSWER *);;

let find_min xs = 
	let rec remove_min xs =
		match xs with
		| Empty -> None
		| Node(value, left_node, Empty) -> Some (value, left_node)
		| Node(value, Empty, right_node) -> Some (value, right_node)
		| Node(value, Node(left_val, _, _) as l, Node(right_val, _, _) as r) ->
			if left_val <= right_val then Some(value, Node(left_val, remove_min l, r))
			else Some(value, Node(right_val, l, remove_min r))
	in remove_min xs (* ANSWER *);;

let as_sorted_list xs = 
	let rec to_list xs acc =
		match xs with
		| Empty -> [] 
		| Node(_, _, _) as node ->
			match find_min node with
			| None -> acc
			| Some(value, node) -> to_list node (acc @ [find_min node])
	in to_list xs [];;