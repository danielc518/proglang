open Fbast;;

(*
 * Replace this with your interpreter code.
 *)

exception TypeMismatch;;
exception NotClosed;;

let rec contains x xs =
	match xs with
	| [] -> false
	| hd :: tl -> 
		if hd = x then true 
		else contains x tl ;;

let check_closed e =
	let rec check_helper idents expr =
		let check = check_helper idents in
		match expr with
		| Var ident -> contains ident idents
		| Not e -> check e
		| Int _ -> true
		| Bool _ -> true
		| And (e1, e2) -> (check e1) && (check e2)
		| Or (e1, e2) -> (check e1) && (check e2)
		| Plus (e1, e2) -> (check e1) && (check e2)
		| Minus (e1, e2) -> (check e1) && (check e2)
		| Equal (e1, e2) -> (check e1) && (check e2)
		| If (e1, e2, e3) -> (check e1) && (check e2) && (check e3)
		| Appl (e1, e2) -> (check e1) && (check e2)
		| Function (ident, body) -> check_helper (ident :: idents) body
		| _ -> false
	in check_helper [] e ;;

let rec subst ident expr body =
	let substitute = subst ident expr in
	match body with
	| Var x -> if x = ident then expr else Var x
	| Int x -> Int x
	| Bool x -> Bool x
	| Not e -> Not (substitute e)
	| And (e1, e2) -> And (substitute e1, substitute e2)
	| Or (e1, e2) -> Or (substitute e1, substitute e2)
	| Plus (e1, e2) -> Plus (substitute e1, substitute e2)
	| Minus (e1, e2) -> Minus (substitute e1, substitute e2)
	| Equal (e1, e2) -> Equal (substitute e1, substitute e2)
	| If (e1, e2, e3) -> If (substitute e1, substitute e2, substitute e3) 
	| Appl (e1, e2) -> Appl (substitute e1, substitute e2)
	| Function (i, b) -> if i = ident then body else Function (i, substitute b) 
	| _ -> raise TypeMismatch ;;
	
let rec eval e = 
	if not (check_closed e) then raise NotClosed else
	match e with
	| Var _ -> raise NotClosed
	| Not expr -> 
		( match eval expr with
		| Bool value -> Bool (not value)
		| _ -> raise TypeMismatch )
	| And (expr1, expr2) ->
		( match eval expr1, eval expr2 with
		| Bool value1, Bool value2 -> Bool (value1 && value2)
		| _ -> raise TypeMismatch )
	| Or (expr1, expr2) ->
		( match eval expr1, eval expr2 with
		| Bool value1, Bool value2 -> Bool (value1 || value2)
		| _ -> raise TypeMismatch )
	| Plus (expr1, expr2) ->
		( match eval expr1, eval expr2 with
		| Int value1, Int value2 -> Int (value1 + value2)
		| _ -> raise TypeMismatch )
	| Minus (expr1, expr2) ->
		( match eval expr1, eval expr2 with
		| Int value1, Int value2 -> Int (value1 - value2)
		| _ -> raise TypeMismatch )
	| Equal (expr1, expr2) ->
		( match eval expr1, eval expr2 with
		| Int value1, Int value2 -> Bool (value1 = value2)
		| _ -> raise TypeMismatch )
	| If (expr1, expr2, expr3) ->
		( match eval expr1 with
		| Bool true ->  eval expr2
		| Bool false -> eval expr3
		| _ -> raise TypeMismatch )
	| Appl (expr1, expr2) ->
		( match eval expr1 with
		| Function (ident, body) -> eval (subst ident (eval expr2) body)
		| _ -> raise TypeMismatch )
	| _ -> e ;;
