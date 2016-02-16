(*
  600.426 - Programming Languages
  JHU Spring 2016
  Homework 2

  In this source file, you will find a number of comments containing the text
  "ANSWER".  Each of these comments indicates a portion of the source code you
  must fill in yourself.  You are welcome to include other functions for use in
  your answers.  Read the instructions for each problem and supply a segment of
  code which accomplishes the indicated task.  For your convenience, a number of
  test expressions are provided for each problem as well as a description of
  their expected values.

  In this assignment, you are permitted to complete the listed tasks using any
  of the OCaml modules/functions.  However you are still required to avoid the use of
  mutation unless explicitly specified in the question.
*)


(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : The Game of Types                                                                      *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  1. For the next several problems, you will be asked to produce an expression which
     has a given type.  It does not matter what expression you provide as long as it
     has that type; there may be numerous (or even infinite) answers for each
     question.  Your answer may *not* produce a compiler warning.  You are *not*
     permitted to use explicit type annotations (such as "fun x:'a -> x").

     Exact type variable names do not count, but must be consistent. For
     example, "'a -> 'b -> 'b" and "'b -> 'a -> 'a" are considered the same,
     but "'a -> 'a -> 'b" is different from both of them. Also, "'_a" and "'a"
     are considered different.

     [20 Points]
*)

(* Give an expression which has the following type: int ref *)
let exp1 = ref 0;; (* ANSWER *)

(* Give an expression which has the following type: '_a -> unit

   HINT: You may want to refer to this section of Real World Ocaml:
         https://realworldocaml.org/v1/en/html/imperative-programming-1.html#side-effects-and-weak-polymorphism
*)
let exp2 = let a = ref None in
    (fun b ->
       match !a with
       | _ -> ());; (* ANSWER *)

(* Give an expression which has the following type: unit -> 'a *)
let exp3 = fun () -> raise (Failure "");; (* ANSWER *)

(* Give an expression which has the following type: 'a list -> ('a -> 'b) -> 'b list *)
let exp4 = fun lst fn -> 
	match lst with
	| [] -> []
	| hd :: tl -> (fn hd) :: [];; (* ANSWER *)

(* Give an expression which has the following type: 'a list -> 'b list -> ('a * 'b) list *)
let exp5 = fun xs ys -> 
	match xs with
	| [] -> []
	| x :: xs -> 
		match ys with
		| [] -> []
		| y :: ys -> (x, y) :: [];; (* ANSWER *)

(* Give an expression which has the following type: (int -> 'a -> 'b) -> 'a list -> ('b * 'b) list *)
let exp6 = fun fn lst ->
	match lst with
	| [] -> []
	| hd :: tl -> 
		let b = (fn 0 hd) in (b, b) :: [];; (* ANSWER *)

(* Give an expression which has the following type: 'a list -> ('a -> bool) -> 'a list * 'a list *)
let exp7 = fun lst fn ->
	match lst with
	| [] -> (lst, lst)
	| hd :: tl -> if (fn hd) then (hd :: tl, hd :: tl) else (lst, lst);; (* ANSWER *)

type ('a, 'b) foobar = Foo of 'a | Bar of 'b ;;

(* Give an expression which has the following type: 'a -> ('b, 'c) foobar *)
let exp8 = fun a -> Bar (raise (Failure ""));; (* ANSWER *)

(*
  Give an expression which has the following type:
  ('a, 'b) foobar list -> ('a -> 'c) -> ('b -> 'c) -> 'c list
*)
let exp9 = fun lst fn1 fn2 ->
	match lst with 
	| [] -> []
	| Foo a :: tl -> (fn1 a) :: []
	| Bar b :: tl -> (fn2 b) :: [];; (* ANSWER *)

(* Give an expression which has the following type: 'a -> 'b *)
let exp10 = fun a -> raise (Failure "");; (* ANSWER *)


(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : An ode to being lazy                                                                   *)
(* -------------------------------------------------------------------------------------------------- *)

(*
   [20 Points]
*)

(*
  OCaml is an eager language. So you cannot create infinite lists/sequences directly. However you can
	encode them fairly easily in the language.
	
	For the purpose of this exercise we will encode (potentially) infinite lists using the following type.
*)
type 'a sequence = Nil | Sequence of 'a * (unit -> 'a sequence);;

(* As a hint, here how you would write a an infinite sequence of zeroes *)
let rec zeroes = Sequence(0, function () -> zeroes);;

(* And a finite sequence 1, 2 *)
let one_and_two = Sequence(1, fun () -> Sequence(2, fun () -> Nil)) ;;


(*
  2a. Write a function to convert a sequence to a list. Of course if you try to evaluate this on an infinite
	    sequence, it will not finish. But we will assume sanity on the caller's part and ignore that issue
	
*)
let rec list_of_sequence s = 
	match s with
	| Nil -> []
	| Sequence(elem, fn) -> 
		elem :: (list_of_sequence (fn ())) (* ANSWER *) ;;

(*
# list_of_sequence one_and_two ;;
- : int list = [1; 2]
*)

(*
  2b. While it is nice to have these infinite sequences, it is often useful to "cut" them to a fixed size. Write
	    a function that cuts off a sequence after a fixed number of values. The return value is a finite sequence
	    of the same type.
			
			(Treat the given count 'n' as the maximum number of elements allowed in the output sequence. So if the input
			is a finite sequence and its length is less than the specified count, the output sequence can have less than
			'n' values)
	
*)
let rec cut_sequence n s = 
	match s with
	| Nil -> Nil
	| Sequence(elem, fn) -> 
		if n > 0 then 
			Sequence(elem, fun () -> cut_sequence (n - 1) (fn ())) 
		else Nil (* ANSWER *) ;;

(*
# list_of_sequence (cut_sequence 5 zeroes) ;;
- : int list = [0; 0; 0; 0; 0]
*)


(*
  2c. You can also encode finite sequences directly using the above type. For this question encode the sequence
	    corresponding to an geometric progression given an initial value, the end value (inclusive) and the common
	    factor (the step size of the sequence)
	
*)	
let rec gp initv endv step = 
	let fn () = 
		if (initv * step > endv) then Nil 
		else gp (initv * step) endv step 
		in Sequence(initv, fn) (* ANSWER *);;

(*
# list_of_sequence (gp 1 12 3) ;;
- : int list = [1; 3; 9]
# list_of_sequence (gp 2 20 2)  ;;
- : int list = [2; 4; 8; 16]
*)

(*
  2d. Now write two infinite sequences: one of factorials, one of triangle numbers.
*)
let factorials = 
	let rec factorial a b = 
		Sequence(a * b, fun() -> factorial (a * b) (b + 1)) in
		Sequence(1, fun () -> Sequence(1, fun () -> factorial 1 2)) (* ANSWER *) ;;
let triangles = 
	let rec triangle a b = 
		Sequence(a + b, fun() -> triangle (a + b) (b + 1)) in
		Sequence(0, fun () -> Sequence(1, fun () -> triangle 1 2)) (* ANSWER *) ;;

(*	
# list_of_sequence (cut_sequence 8 factorials) ;;
- : int list = [1; 1; 2; 6; 24; 120; 720; 5040]
# list_of_sequence (cut_sequence 10 triangles) ;;
- : int list = [0; 1; 3; 6; 10; 15; 21; 28; 36; 45]
*)
	
(*
  2e. Write a filter-map function which takes a function and a sequence as input
      and returns a new sequence where the values have been mapped using the input
      function and potentially dropped.

      Note that if the filter part always returns None and the input sequence is infinite,
      this function will not terminate. Ignore this issue.
*)

let rec filter_map_sequence : ('a -> 'b option) -> 'a sequence -> 'b sequence =
    fun f s ->
			match s with
			| Nil -> Nil
			| Sequence(elem, fn) ->
				match f elem with
				| None -> filter_map_sequence f (fn ())
				| Some x -> Sequence(x, fun () -> filter_map_sequence f (fn ())) (* ANSWER *) ;;

(*
  2f. Write a map and filter functions (analogous to List.map and List.filter)
      using your implementation of filter_map_sequence.
*)

let rec map_sequence fn s = 
	let f = fun x -> Some (fn x)
	in filter_map_sequence f s (* ANSWER *) ;;

(*
# list_of_sequence(map_sequence (fun x -> x + 3) (gp 2 10 2)) ;;
- : int list = [5; 7; 11]
*)

let rec filter_sequence fn s = 
	let f = fun x -> 
		if (fn x) then Some x else None
	in filter_map_sequence f s (* ANSWER *) ;;

(*
# list_of_sequence(filter_sequence (fun x -> x mod 3 = 2) (gp 2 12 2)) ;;
- : int list = [2; 8]
# list_of_sequence(cut_sequence 4 (filter_sequence (fun x -> x mod 5 = 0)
triangles)) ;;
- : int list = [0; 10; 15; 45]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 3 : Making Modules                                                                         *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  3a. Create a simple heap (priority queue) module that matches the interface provided below.

      In the section below, fill out the signature and the implementation details. You must explicitly leave any
      types in the signature abstract. (This is good practice in the software engineering sense. By not explicitly
      binding the types on the interface, you allow different implementations to choose types best suited
      for their goals)

      Your heap implementation does not need to be efficient.
      You are not permitted to just wrap the OCaml heap in this interface.

      You should use the generic compare function; there are better ways, but they're more
      complicated.

      NOTE: When you query the type of your functions in the top loop, it might return the fully qualified type signatures.
      E.g: empty : unit -> 'a GHeap.heap instead of just unit -> 'a heap. This is fine.
		
      [20 Points]
*)


(* This is a functional heap, no mutation is used. *)
(* This is also found in GHeap.mli *)
(*
module GHeap =
  sig
    type 'a heap

    (* The method returns a *new* empty heap instance *)
    val empty : unit -> 'a heap

    (* Insert a new value into the heap *)
    val insert : 'a -> 'a heap -> 'a heap

    (* Return the smallest value in the heap and the heap that results when it is removed. Return None if the heap is empty. *)
    val find_min : 'a heap -> ('a * 'a heap) option

    (* Return a list containing the elements of the heap in ascending order. *)
    val as_sorted_list : 'a heap -> 'a list
  end
;;
*)

(*
$ ocamlc GHeap.mli
$ ocamlc -c GHeap.ml
$ ocaml
# #load "GHeap.cmo";;
# let q1 = GHeap.empty () ;;
val q1 : '_a GHeap.heap = <abstr>
# GHeap.find_min q1 ;;
- : ('_a * '_a GHeap.heap) option = None
# let q2 = GHeap.insert 1 q1 ;;
val q2 : int GHeap.heap = <abstr>
# GHeap.find_min q2 ;;
- : (int * int GHeap.heap) option = Some (1, <abstr>)
# let q3 = GHeap.insert 2 q2 ;;
val q3 : int GHeap.heap = <abstr>
# let Some (x, q4) = GHeap.find_min q3 ;;
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
None
val x : int = 1
val q4 : int GHeap.heap = <abstr>
# GHeap.find_min q4 ;;
- : (int * int GHeap.heap) option = Some (2, <abstr>)
# GHeap.as_sorted_list q3 ;;
- : int list = [1; 2]
*)

(* Write the following functions for your heap; you should not need to add them to your interface. *)

(* This function takes a list of elements and inserts each of them into the heap *)
let rec insert_many (xs : 'a list) (h : 'a heap) : 'a heap = 
	match xs with
	| [] -> h
	| x :: xs -> insert_many xs (GHeap.insert x h) (* ANSWER *);;

(* This function returns the smallest element in the heap *)
let rec get_min (h : 'a heap) : 'a = 
	match GHeap.find_min h with 
	| None -> raise (Failure "Heap is empty!")
	| Some (x, xs) -> x (* ANSWER *);;

(* This function returns the heap with its smalles element deleted *)
let rec delete_min (h : 'a heap) : 'a heap = 
	match GHeap.find_min h with 
	| None -> h
	| Some (x, xs) -> xs (* ANSWER *);;

(* Now write "heap sort" (or at least, it would be heap sort if your heap were efficient) *)

let rec heap_sort (xs : 'a list) : 'a list = 
	let heap = GHeap.empty ()
	in GHeap.as_sorted_list (insert_many xs heap) (* ANSWER *);;

(* -------------------------------------------------------------------------------------------------- *)
(* Section 4 : Symbolic Computations                                                                  *)
(* -------------------------------------------------------------------------------------------------- *)

(*
   Recall that a polynomial function is one that can be represented as
   f(x) = a_0 * x^0 + a_1 * x^1 + ... + a_n * x^n
   for some sequence of integers a_0, a_1, ..., a_n;
   https://en.wikipedia.org/wiki/Polynomial#Polynomial_functions
   In this section, we're going to represent polynomials as non-empty lists of
   integers [a_0; a_1; ...; a_n].

   [30 Points]
*)

(* 4a. Write a function to evaluate a polynomial at a particular input *)
let rec eval_polynomial p n = 
	let f = fun value coeffcients -> value * n + coeffcients
	in List.fold_left f 0 (List.rev p) (* ANSWER *) ;;

(*
# eval_polynomial [0] 4 ;;
- : int = 0
# eval_polynomial [1;0;1] 4 ;;
- : int = 17
# eval_polynomial [3;2;1] (-2) ;;
- : int = 0
*)

(* 4b. Write a function compute the derivative of a polynomial

   https://en.wikipedia.org/wiki/Polynomial#Calculus *)
let rec deriv p = 
	let rec derivative lst n = 
		match lst with
		| [] -> []
		| hd :: tl -> 
			if n = 0 then 
				match tl with
				| [] -> [0]
				| h :: t -> derivative tl 1
			else (n * hd) :: derivative tl (n + 1)
	in derivative p 0 (* ANSWER *) ;;
(*
# deriv [1] ;;
- : int list = [0]
# deriv [1;2;3] ;;
- : int list = [2;6]
*)

(*
  4c. Write a function to add two polynomials
*)
let rec add_poly p1 p2 = 
	match (p1, p2) with 
	| [], [] -> []
	| [], y :: ys -> y :: ys
	| x :: xs, [] -> x :: xs
	| x :: xs, y :: ys -> (x + y) :: (add_poly xs ys) (* ANSWER *) ;;

(*
# add_poly [1;2] [3;0;4] ;;
- : int list = [4;2;4]
*)

(*
  4d. Write a function to multiply two polynomials.
*)

let rec mul_poly p1 p2 = 
	if p1 = [] || p2 = [] then [] else
	let rec shift_right lst n = if n = 0 then lst else shift_right (0 :: lst) (n - 1) in
	let rec mult_mono lst x n = shift_right (List.map (fun c -> x * c) lst) n in
	let rec multiply lst1 lst2 acc n = 
		match lst2 with
		| [] -> acc
		| hd :: tl -> multiply lst1 tl (add_poly acc (mult_mono p1 hd n )) (n + 1) in
	multiply p1 p2 [0] 0 (* ANSWER *) ;;

(*
# mul_poly [-1] [3;0;4] ;;
- : int list = [-3;0;-4]
# mul_poly [1;1] [3;0;4] ;;
- : int list = [3;3;4;4]
*)


(*
  4e. Write a function to divide a polynomial by another, giving back a pair of
      a quotient and a remainder.
*)

let rec remove_zeros lst = 
	match lst with
	| [] -> []
	| hd :: tl -> 
		if hd = 0 then remove_zeros tl
		else lst ;;

let rec add_zeroes_right n l = 
	if n > 0 then add_zeroes_right (n - 1) (l @ [0]) else l ;;

let rec add_zeroes_left n l = 
		if n > 0 then add_zeroes_left (n - 1) (0 :: l) else l ;;

let rec div_poly p1 p2 = 
	let map f lst1 lst2 = 
		let diff = (List.length lst1) - (List.length lst2) in
		List.map2 f (add_zeroes_left (-diff) lst1) (add_zeroes_left diff lst2) in
		let rec divide p_1 p_2 quo_acc =
			let degree_p_1 = List.length p_1 - 1 in
			let degree_p_2 = List.length p_2 - 1 in
			let degree_diff = degree_p_1 - degree_p_2 in
			if degree_diff < 0 then (quo_acc, p_1) 
			else
				if List.hd p_2 = 0 then ([], []) else
				let r = (List.hd p_1) / (List.hd p_2) in
				let flt_r = float (List.hd p_1) /. float (List.hd p_2) in
				if float r <> flt_r then 
					let rem = map (-) (List.rev p1) (List.rev p2) in
					if List.for_all (fun x -> x > -1) rem then ([1], rem)
					else ([], List.rev p1)
				else
				let r_p2 = List.map (( * ) r) (add_zeroes_right degree_diff p_2) in
				let new_quo_acc = map (+) quo_acc (add_zeroes_right degree_diff [r]) in
				let new_p1 = remove_zeros (List.tl (map (-) p_1 r_p2)) in
				divide new_p1 p_2 new_quo_acc in
				match divide (List.rev p1) (List.rev p2) [] with
				| ( x :: xs, y :: ys) -> 
					let quotient  = List.rev (remove_zeros (x :: xs)) in
					let remainder = List.rev (remove_zeros (y :: ys)) in
					(quotient, remainder) 
				| ( x :: xs, []) -> (List.rev (remove_zeros (x :: xs)), [0])
				| (_, y :: ys) -> ([0], List.rev (remove_zeros (y :: ys))) 
				| ([], []) -> ([], []) (* ANSWER *) ;;
		

(*
# div_poly [3;3;4;4] [3;0;4] ;;
- : (int list * int list) = ([1;1], [0])
# div_poly [3;3;4;4] [0;0;4] ;;
- : (int list * int list) = ([1;1], [3;3])
*)

(*
  4f. Write a function to compute the Greates Common Divisor (gcd) of two
      polynomials.

      https://en.wikipedia.org/wiki/Greatest_common_divisor
*)
let rec gcd p1 p2 = 
	match p2 with
	| [] -> p1
	| x :: xs -> 
		if List.length p2 = 1 && x = 0 then p1
		else 
			if List.length p1 < List.length p2 then gcd p2 p1 
			else
				let (q1, r1) = div_poly p1 p2 in 
				let (q2, r2) = div_poly p2 p1 in 
				if r1 = p1 && r2 = p2 then 
					if List.length p1 = List.length p2 then [1]
					else 
						if List.for_all (fun x -> x mod 2 = 0) p2 then
							let (q, r) = (div_poly p2 [2]) in gcd p1 q
						else if List.for_all (fun x -> x mod 3 = 0) p2 then
							let (q, r) = (div_poly p2 [3]) in gcd p1 q
						else
							[1]
				else 
					if r1 = p1 then gcd p1 r2 
					else gcd p2 r1 (* ANSWER *) ;;

(*
# gcd [3;3;4;4] [3;0;4] ;;
- : int list = [3;0;4]
# gcd [2;4] [3;6] ;;
- : int list = [1;2]
# gcd [1;2;1] [2;2] ;;
- : int list = [1;1]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 5 : Mutable State and Memoization                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  5. Cache: Pure functions (those without side effects) always produces the same value
     when invoked with the same parameter. So instead of recomputing values each time,
     it is possible to cache the results to achieve some speedup.

     The general idea is to store the previous arguments the function was called
     on and its results. On a subsequent call if the same argument is passed,
     the function is not invoked - instead, the result in the cache is immediately
     returned.

     Note: you will need to use mutable state in some form to implement the cache.

     [10 Points]
*)

(*
  Given any function f as an argument, create a function that returns a
  data structure consisting of f and its cache
*)
let new_cached_fun f = (f, ref []) (* ANSWER *) ;;

(*
  Write a function that takes the above function-cache data structure,
  applies an argument to it (using the cache if possible) and returns
  the result
*)
let apply_fun_with_cache cached_fn x = 
	let (f, lst) = cached_fn in
	try 
		Pervasives.snd (List.find (fun (k, _) -> k = x) !lst)
	with Not_found -> let value = f x in
		lst := (x, value) :: !lst; value (* ANSWER *) ;;

(*
  The following function makes a cached version for f that looks
  identical to f; users can't see that values are being cached
*)

let make_cached_fun f =
  let cf = new_cached_fun f in
    function x -> apply_fun_with_cache cf x
;;


(*
let f x = x + 1;;
let cache_for_f = new_cached_fun f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 2;;
cache_for_f;;
apply_fun_with_cache cache_for_f 5;;
cache_for_f;;
let cf = make_cached_fun f;;
cf 4;;
cf 4;;


# val f : int -> int = <fun>
# val cache_for_f : ...
# - : int = 2
# - : ...
# - : int = 2
# - : ...
# - : int = 3
# - : ...
# - : int = 6
# - : ...
# val cf : int -> int = <fun>
# - : int = 5
# - : int = 5
*)