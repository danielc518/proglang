(*
    600.426 - Programming Languages
    JHU Spring 2016
    Homework 3

    Now that you have completed your Fb interpreter, you will need to write some
    programs in Fb.

    Note: We will be testing your Fb code with the canonical interpreter (binary) that was
    provided to you. So it is worth your while to test your code against that prior to submitting
    it.
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*

  Name                  : Sanghyun Choi
  List of team Members  : N/A
  List of discussants   : Alex Rozenshteyn

*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : Operational Semantics and Proofs                                                       *)
(* 15 Points                                                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  1a. The operational semantics for Fb provide a set of proof rules which are
  used by the interpreter to perform evaluation. In this problem, you will build
  a proof by hand. Write an operational semantics proof which demonstrates that
  the Fb expression "(Fun f -> Fun y -> y f) (Fun z -> If z Then 1 Else 0) True"
  evaluates to 1.

  Your proof should follow the Fb operational semantics rules correctly and not
  skip any steps.

*)

(* 
Due to the size of the proof, it is broken into multiple parts. 
We use v => * as an abbreviation for v => v (when v is lengthy) for brevity.

Part 1.

--------------------------  ------------  ---------------------
Fun f -> Fun y -> y f => *  True => True  Fun y -> y True => *
---------------------------------------------------------------
(Fun f -> Fun y -> y f) True => Fun y -> y True

Part 2.

                                                                                               ------------  ------
                                                                                               True => True  1 => 1
                                               --------------------------------  ------------  ---------------------------
                                               Fun z -> If z Then 1 Else 0 => *  True => True  If True Then 1 Else 0 => 1
-----------  --------------------------------  ---------------------------------------------------------------------------
(by part 1)  Fun z -> If z Then 1 Else 0 => *  (Fun z -> If z Then 1 Else 0) True => 1
--------------------------------------------------------------------------------------------------------------------------
(Fun f -> Fun y -> y f) True (Fun z -> If z Then 1 Else 0) => 1

*)

(*
  1b. Many languages provide a way to chain If-Then-Else expressions. Fb
  doesn't, per se, but due to its parse rules
  "If x = 0 Then 1 Else If x = 1 Then 2 Else If x = 2 Then 4 Else 0"
  evaluates the way you'd expect.

  Write operational semantics for "If e Then e (ElseIf e Then e)* Else e End",
  where (ElseIf e Then e)* means that the parenthesized grammar term may be
  repeated 0 or more times (the parentheses are not part of the syntax).

  They should be properly short-circuiting and their evaluation should look
  similar to the evaluation of the "Else If" chain in Fb.
*)

(*

Define "c" to be a new non-terminal

c ::= ElseIf e Then e | ε

e ::= ... | If e Then e c Else e End

(Note: "c" is not an expression, ε is a symbol meaning "empty string")

If-Then:

e1 => True    e2 => v2
---------------------------------
If e1 Then e2 c Else e3 End => v2

If-Else:

e1 => False    e3 => v3
---------------------------------
If e1 Then e2 Else e3 End => v3

If-ElseIf:

e1 => False    c = ElseIf e4 Then e5 c'    If e4 Then e5 c' Else e3 End => v
----------------------------------------------------------------------------
If e1 Then e2 c Else e3 End => v

*)

(*
  1c. The Freeze and Thaw operations are defined as macros in the book. But it
  is equally feasible to augment Fb with new operations "Freeze e" and "Thaw e"
  with a similar behavior. Assuming we add these 2 operations to Fb, write out
  the operational semantics for both.
*)

(*

Add the following values and expressions:
v ::= ... | Frozen e
e ::= ... | Freeze e | Thaw e

--------------------
Freeze e => Frozen e

e1 => Frozen e2, e2 => v
------------------------
Thaw e1 => v

*)


(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : (En)Coding in Fb                                                                       *)
(* 20 Points                                                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(*
    The answers for this section must be in the form of Fb ASTs. You may assume that
    "fbdktoploop.ml" has been loaded before this code is executed; thus, you may use
    the parse function to create your answer if you like. Alternately you can create
    ASTs directly.

    For instance, the two definitions of the identity function in Fb are equivalent. (See below)
    The second one directly declares the datastructure produced by the first expression.

    You may use whichever form you please; the parse form is somewhat more readable, but
    the AST form allows you to create and reuse subtrees by declaring OCaml variables.

    For questions in this section you are not allowed to use the Let Rec syntax even if you
    have implemented it in your interpreter. Any recursion that you use must entirely be in
    terms of Functions. Feel free to implement an Fb Y-combinator here.  For examples and
    hints, see the file "src/Fb/fbexamples.ml" in the FbDK project.

    Remeber to test your code against the standard Fb binaries (and not just your own
    implementation of Fb) to ensure that your functions work correctly.
*)

let fb_identity_parsed = parse "Function x -> x";;

let fb_identity_ast = Function(Ident("x"), Var(Ident("x")));;

let combY = parse "Function body -> 
    (Function fun -> Function arg -> fun fun arg)
      (Function this -> Function arg -> body (this this) arg)";;

(*
  2a. Fb is such a minimalisitc language that it does not even include a
      less-than operation. But it is possible to create one of your own.

      But we're not going to ask you to create a less-than operation (that is, a
      function which takes an integer and produces a boolean). We're going to
      ask you to create a function that expects 4 arguments: an Int and 3
      Functions; depending on whether the Int is negative, zero, or positive, it
      calls the first, second, or third function, passing it the Int.

      Hint: a) You can call upon your powers of recursion ;) b) We dont really
      care about efficiency.

      [5 Points]
*)

let fbCond = Appl(Appl(combY, parse "Function this -> Function arg ->
	  Function x -> Function f1 -> Function f2 -> Function f3 ->
    If x = 0 
			Then f2 0 
		Else 
			If (x + arg) = 0 
				Then f1 x 
			Else 
				If (x - arg) = 0 
					Then f3 x 
				Else (this (arg + 1) x f1 f2 f3)"), Int 0);; (* ANSWER *)

(*
# ppeval (Appl(parse
  "Fun fbCond -> fbCond 3 (Fun a -> 1) (Fun b -> 0) (Fun c -> c + 1)"
  , fbCond)) ;;
==> 4
- : unit = ()
# ppeval (Appl(parse
  "Fun fbCond -> fbCond 0 (Fun a -> 1) (Fun b -> 0) (Fun c -> c + 1)"
  , fbCond)) ;;
==> 0
- : unit = ()
*)

(*
  2b. Fb is a simple language. But even it contains more constructs than
      strictly necessary For example, you dont really need integer values at
      all! They can be encoded using just functions using what is called
      Church's encoding http://en.wikipedia.org/wiki/Church_encoding

      Essentially this encoding allows us to represent integers as functions.
      For example:

        0 --> Function f -> Function x -> x
        1 --> Function f -> Function x -> f x
        2 --> Function f -> Function x -> f (f x)

      We will write 4 functions that work with church numerals in this section.
      Remember that all your answers should generate Fb ASTs.

      You can assume that we are dealing with only non-negative integers in this
      question.
*)

(* Write a Fb function to convert a church encoded value to an Fb native integer.*)
let fbUnChurch = parse "Function church -> church (Function n -> n + 1) 0" ;; (* ANSWER *)

(* Write a Fb function to convert an Fb native integer to a Church encoded value *)
let fbChurch = Function(Ident("n"), 
                 Function(Ident("f"), 
                   Function(Ident("x"), 
                     Appl(Appl(combY, parse "Function this -> Function n -> 
                     If n = 0 Then x Else f (this (n-1))"), Var(Ident("n")))
                   )
                 )
               ) ;; (* ANSWER *)

(*
# let church2 = parse "Function f -> Function x -> f (f x)";;
val church2 : Fbast.expr =
  Function (Ident "f",
   Function (Ident "x",
    Appl (Var (Ident "f"), Appl (Var (Ident "f"), Var (Ident "x")))))
# ppeval (Appl(fbUnChurch,church2));;
==> 2
- : unit = ()
# ppeval (Appl(fbUnChurch,Appl(fbChurch,Int(12))));;
==> 12
- : unit = ()
# ppeval (Appl(Appl(Appl(fbChurch,Int(4)),(parse "Function n -> n + n")),Int(3)));;
==> 48
- : unit = ()
*)

(* For these two functions, you are not allowed to call fbUnChurch, do the math,
   and then fbChurch the result *)

(* Write a function to add two church encoded values *)
let fbChurchAdd = parse "Function a -> Function b -> Function f -> Function x -> a f (b f x)" ;; (* ANSWER *)

(* Write a function to multiply two church encoded values *)
let fbChurchMul = parse "Function a -> Function b -> Function f -> Function x -> a (b f) x" ;; (* ANSWER *)

(*
# let church2 = parse "Function f -> Function x -> f (f x)" ;;
# let church3 = parse "Function f -> Function x -> f (f (f x))" ;;
# ppeval (Appl(fbUnChurch, (Appl(Appl(fbChurchAdd, church3), church2))));;
==> 5
- : unit = ()a
# ppeval (Appl(fbUnChurch, (Appl(Appl(fbChurchMul, church3), church2))));;
==> 6
- : unit = ()
*)

(*
  2c. Church encoding is not the only way to represent natural numbers in lambda
      calculus (basically the same as Fb without integers or booleans). There is
      also something called Scott encoding:
      https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding

      Although it is more verbose than the Church encoding, it makes it much
      easier to manipulate the resulting functions as though they were just data
      (like OCaml data-types).

      Write the same four functions you from above for Scott numerals, and also
      write the predecessor function.

      0  -->  Fun s -> Fun z -> z
      1  -->  Fun s -> Fun z -> s 1  --> Fun s -> Fun z -> s (Fun s -> Fun z -> z)
      2  -->  Fun s -> Fun z -> s 2  -->
                   Fun s -> Fun z -> s (Fun s -> Fun z -> s (Fun s -> Fun z -> z))
*)

(* Write a Fb function to convert a Scott encoded value to an Fb native integer.*)
let fbUnScott = Appl(parse "Function f -> Function scott -> scott f 0", Appl(combY, parse "Function this -> Function f -> 1 + (f this 0)")) ;; (* ANSWER *)

(* Write a Fb function to convert an Fb native integer to a Scott encoded value *)
let fbScott = Function(Ident("n"), Appl(Appl(combY, parse "Function this -> Function n -> Function s -> Function z ->
                     If n = 0 Then z Else s (this (n-1))"), Var(Ident("n")))) ;; (* ANSWER *)

(*
# let scott2 = parse "Function s -> Function z -> s (Function s -> Function z -> s (Function s -> Function z -> z))";;
val scott2 : Fbast.expr =
  Function (Ident "s",
   Function (Ident "z",
    Appl (Var (Ident "s"),
     Function (Ident "s",
      Function (Ident "z",
       Appl (Var (Ident "s"),
        Function (Ident "s", Function (Ident "z", Var (Ident "z")))))))))
# ppeval (Appl(fbUnScott,scott2));;
==> 2
- : unit = ()
# let scott1 = parse "Function s -> Function z -> s (Function s -> Function z -> z)";;
val scott1 : Fbast.expr =
  Function (Ident "s",
   Function (Ident "z",
    Appl (Var (Ident "s"),
     Function (Ident "s", Function (Ident "z", Var (Ident "z"))))))
# ppeval (Appl(fbUnScott,scott1));;
==> 1
- : unit = ()
# let scott0 = parse "Function s -> Function z -> z";;
val scott0 : Fbast.expr =
  Function (Ident "s", Function (Ident "z", Var (Ident "z")))
# ppeval (Appl(fbUnScott,scott0));;
==> 0
- : unit = ()
# ppeval (Appl(fbUnScott,Appl(fbScott,Int(12))));;
==> 12
- : unit = ()
# ppeval (Appl(fbUnScott,Appl(fbScott,Int(48))));;
==> 48
- : unit = ()
*)

(* For these three functions, you are not allowed to call fbUnScott, do the math,
   and then fbScott the result *)

(* Write a function to add two church encoded values *)
let fbScottAdd = Appl(parse "Function f -> Function a -> Function b -> a (f b) b", 
                   Appl(combY, parse "Function this -> Function b -> Function f -> Function s -> Function z -> s (f (this b) b)")) ;; (* ANSWER *)

(* Write a function to multiply two church encoded values *)
let fbScottMul = Appl(Appl(parse "Function f -> Function add -> Function a -> Function b -> a (f add (Function s -> Function z -> z) b) (Function s -> Function z -> z)", 
                   Appl(combY, parse "Function this -> Function add -> Function acc -> Function b -> Function f -> f (this add (add acc b) b) (add acc b)")), fbScottAdd) ;; (* ANSWER *)

(*
# let scott2 = parse "Function s -> Function z -> s (Function s -> Function z -> s (Function s -> Function z -> z))";;
# let scott3 = parse "Function s -> Function z -> s (Function s -> Function z -> s (Function s -> Function z -> s (Function s -> Function z -> z)))";;
# ppeval (Appl(fbUnScott, (Appl(Appl(fbScottAdd, scott3), scott2))));;
==> 5
- : unit = ()a
# ppeval (Appl(fbUnScott, (Appl(Appl(fbScottMul, scott3), scott2))));;
==> 6
- : unit = ()
*)

(* Write the predecessor function. *)

let fbPred = parse "Fun x -> If x = 0 Then 0 Else x - 1"

(* The Church encoding of predecessor is hard; the Scott encoding is relatively
   straightforward. *)
let fbScottPred = Appl(parse "Function f -> Function scott -> Function s -> Function z -> scott (f s z) z", 
                   parse "Function s -> Function z -> Function f -> f s z") ;; (* ANSWER *)


(*
  2d. Now try your hand at a list encoding. The two functions which follow
      convert between *OCaml* lists and encoded Fb lists.
*)

let pair = parse "Function x -> Function y -> Function z -> z x y" ;;
let first = parse "Function p -> p (Function x -> Function y -> x)" ;;
let second = parse "Function p -> p (Function x -> Function y -> y)" ;;
let nil = Appl(Appl(pair, Bool true), Bool true) ;;
let cons = Appl(Appl(parse "Function pair -> Function isnil -> Function h -> Function t -> pair isnil (pair h t)", pair), Bool false) ;;
let head = Appl(Appl(parse "Function first -> Function second -> Function z -> first (second z)", first), second) ;;
let tail = Appl(parse "Function second -> Function z -> second (second z)", second) ;;

(* Produce an expression that evaluates to an Fb encoded list given an OCaml
   list. The elements of the OCaml list are Fb expressions, but your list
   should contain only values.
   
   Note: there are two primary ways to do this:
       1. Have your OCaml code evaluate the expressions before putting them in
          the Fb AST (or string which you will parse to an AST).
       2. Leave the expressions as expressions in the Fb code, relying on the
          interpreter to evaluate them; if you do this, make sure they do get
          evaluated.

   Running `eval (fbList [parse "1"; parse "1 + True"])` should result in an Fb
   runtime error. If it does not, you are not evaluating the expressions.

   If you are taking the first approach, you should produce Fb code which will
   have a runtime error in this case.
*)
let fbList ocaml_list_of_Fb_expressions = 
	let rec iterate ocaml_list fb_list =
		match ocaml_list with
		| [] -> fb_list
		| x :: xs -> iterate xs (Appl(Appl(cons, x), fb_list)) 
	in iterate ocaml_list_of_Fb_expressions nil;; (* ANSWER *)

(* Produce an OCaml list of Fb values given an an expression which evaluates to
   an Fb encoded list, possibly containing expressions. You will need to call
   `eval` in this function.
   
   Throw an exception if you encounter an Fb error, or if the Fb expression
   does not evalute to an Fb list.

   `fun x -> let y = fbUnList x in map eval y = y` should be true for any input
   unless an exception is thrown.

   Again, there are two primary ways to do this:
       1. Have OCaml evaluate the expression and pull it apart, evaluating the
          components as necessary.
       2. Write an Fb function which takes a list and evaluates each element of
          it, constructing a new list of values; then use eval once to call
          this function on your input before using OCaml to pull apart your Fb
          encoded list, secure in the knowledge that its elements are values.
*)
let fbUnList encoded_Fb_list = 
	let rec iterate fb_list ocaml_list = 
		if (Bool false) = (eval (Appl(first, fb_list))) 
		  then iterate (Appl(tail, fb_list)) ((eval (Appl(head, fb_list))) :: ocaml_list)
		else ocaml_list
	in iterate encoded_Fb_list [];; (* ANSWER *)
	
(*
fbUnList (fbList [Int 1; Int 2; Int 3; Int 4; Int 5]);;
- : Fbast.expr list = [Int 1; Int 2; Int 3; Int 4; Int 5]
*)
