type 'a heap

(* The method returns a *new* empty heap instance *)
val empty : unit -> 'a heap

(* Insert a new value into the heap *)
val insert : 'a -> 'a heap -> 'a heap

(* Return the smallest value in the heap and the heap that results when it is removed. Return None if the heap is empty. *)
val find_min : 'a heap -> ('a * 'a heap) option

(* Return a list containing the elements of the heap in ascending order. *)
val as_sorted_list : 'a heap -> 'a list