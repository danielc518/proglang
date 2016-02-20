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
  List of discussants   : N/A

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

(* ANSWER *)

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

(* ANSWER *)

(*
  1c. The Freeze and Thaw operations are defined as macros in the book. But it
  is equally feasible to augment Fb with new operations "Freeze e" and "Thaw e"
  with a similar behavior. Assuming we add these 2 operations to Fb, write out
  the operational semantics for both.
*)

(* ANSWER *)


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

(*
  1a. Fb is such a minimalisitc language that it does not even include a
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

let fbCond = parse "" ;; (* ANSWER *)

(*
# ppeval (Appl(parse
  "Fun fblt -> fblt 3 (Fun a -> 1) (Fun b -> 0) (Fun c -> c + 1)"
  , fblt)) ;;
==> 4
- : unit = ()
# ppeval (Appl(parse
  "Fun fblt -> fblt 0 (Fun a -> 1) (Fun b -> 0) (Fun c -> c + 1)"
  , fblt)) ;;
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
let fbUnChurch = parse "" ;; (* ANSWER *)

(* Write a Fb function to convert an Fb native integer to a Church encoded value *)
let fbChurch = parse "" ;; (* ANSWER *)

(*
# let church2 = parse "Function f -> Function x -> f (f x)";;
val church2 : Fbast.expr =
  Function (Ident "f",
   Function (Ident "x",
    Appl (Var (Ident "f"), Appl (Var (Ident "f"), Var (Ident "x")))))
# ppeval (Appl(fbUnchurch,church2));;
==> 2
- : unit = ()
# ppeval (Appl(fbUnchurch,Appl(fbChurch,Int(12))));;
==> 12
- : unit = ()
# ppeval (Appl(Appl(Appl(fbChurch,Int(4)),(parse "Function n -> n + n")),Int(3)));;
==> 48
- : unit = ()
*)

(* For these two functions, you are not allowed to call fbUnChurch, do the math,
   and then fbChurch the result *)

(* Write a function to add two church encoded values *)
let fbChurchAdd = parse "" ;; (* ANSWER *)

(* Write a function to multiply two church encoded values *)
let fbChurchMul = parse "" ;; (* ANSWER *)

(*
# let church2 = parse "Function f -> Function x -> f (f x)" ;;
# let church3 = parse "Function f -> Function x -> f (f (f x))" ;;
# ppeval (Appl(fbUnchurch, (Appl(Appl(fbChurchAdd, church3), church2))));;
==> 5
- : unit = ()a
# ppeval (Appl(fbUnchurch, (Appl(Appl(fbChurchMult, church3), church2))));;
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
let fbUnScott = parse "" ;; (* ANSWER *)

(* Write a Fb function to convert an Fb native integer to a Scott encoded value *)
let fbScott = parse "" ;; (* ANSWER *)

(* For these three functions, you are not allowed to call fbUnScott, do the math,
   and then fbScott the result *)

(* Write a function to add two church encoded values *)
let fbScottAdd = parse "" ;; (* ANSWER *)

(* Write a function to multiply two church encoded values *)
let fbScottMul = parse "" ;; (* ANSWER *)

(* Write the predecessor function. *)

let fbPred = parse "Fun x -> If x = 0 Then 0 Else x - 1"

(* The Church encoding of predecessor is hard; the Scott encoding is relatively
   straightforward. *)
let fbScottPred = parse "" ;; (* ANSWER *)


(*
  2d. Now try your hand at a list encoding. The two functions which follow
      convert between *OCaml* lists and encoded Fb lists.
*)

(* Produce an Fb encoded list given an OCaml list. The elements of the OCaml
   list are Fb values *)
let fbList ocaml_list_of_Fb_expressions = parse "" ;; (* ANSWER *)

(* Produce an OCaml list of Fb values given an Fb encoded list. *)
let fbUnList encoded_Fb_list = parse "" ;; (* ANSWER *)