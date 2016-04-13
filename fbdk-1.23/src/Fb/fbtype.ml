open Fbast;;

(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = false;;

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *)
exception TypeInferenceFailure of string;;
exception TypeError of string;;

(* Fresh variable name starts with 'a *)
let ascii = ref 97;;

(* Generates fresh type variable *)
let freshTypeVar () = ascii := !ascii + 1; TVar (Char.escaped (Char.chr (!ascii - 1)));;

(* Merge two sets of type equatios *)
let rec merge xs ys =
  match xs with 
    | [] -> ys 
    | x :: xs -> if List.mem x ys then (merge xs ys) else (merge xs (x :: ys));;

(* Lookup the type in Gamma *)
let rec lookup x gamma =
  match gamma with
  | [] -> (match x with Ident v -> raise (TypeInferenceFailure ("Could not find type for unbound variable " ^ v)))
  | hd :: tl -> if (fst hd) = x then snd hd else lookup x tl;;

(* Generates a 'type \ E' pair for the given expression *)
let rec generate e gamma =
  match e with
  | Int _ -> (TInt, [])
  | Bool _ -> (TBool, [])
  | Var x -> (lookup x gamma, [])
  | Not e -> let p = generate e gamma in (TBool, merge (snd p) [(fst p, TBool)])
  | And (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in
        (TBool, (merge (merge (snd lhs) (snd rhs)) [ if (fst lhs) = (fst rhs) then ((fst rhs), TBool) else ((fst lhs), TBool); ((fst rhs), TBool) ]))
  | Or (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in
        (TBool, (merge (merge (snd lhs) (snd rhs)) [ if (fst lhs) = (fst rhs) then ((fst rhs), TBool) else ((fst lhs), TBool); ((fst rhs), TBool) ]))
  | Equal (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in
        (TInt, (merge (merge (snd lhs) (snd rhs)) [ if (fst lhs) = (fst rhs) then ((fst rhs), TInt) else ((fst lhs), TInt); ((fst rhs), TInt) ]))
  | Plus (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in
        (TInt, (merge (merge (snd lhs) (snd rhs)) [ if (fst lhs) = (fst rhs) then ((fst rhs), TInt) else ((fst lhs), TInt); ((fst rhs), TInt) ]))
  | Minus (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in
        (TInt, (merge (merge (snd lhs) (snd rhs)) [ if (fst lhs) = (fst rhs) then ((fst rhs), TInt) else ((fst lhs), TInt); ((fst rhs), TInt) ]))
  | If (e1, e2, e3) -> let p1 = generate e1 gamma and p2 = generate e2 gamma and p3 = generate e3 gamma in let newTypeVar = freshTypeVar () in
        (newTypeVar, (merge (merge (merge (snd p1) (snd p2)) (snd p3)) [ ((fst p1), TBool); ((fst p2), newTypeVar); ((fst p3), newTypeVar) ]))
  | Function (x, e) -> let newTypeVar = freshTypeVar () in let p = generate e (merge gamma [ (x, newTypeVar) ]) in
        ((TArrow (newTypeVar, (fst p))), (snd p))
  | Appl (e1, e2) -> let lhs = generate e1 gamma and rhs = generate e2 gamma in let newTypeVar = freshTypeVar () in 
        (newTypeVar, (merge (merge (snd lhs) (snd rhs)) [ ((fst lhs), (TArrow ((fst rhs), newTypeVar))) ]))
  | _ -> raise (TypeInferenceFailure ("Could not find type for given expression"));;
  
(* Perform the closure on E *)
let rec performClosure eqns =
  let getTransitive t1 t2 f1 f2 eqns =
      let eqns1 = List.map (fun x -> (t1, f1 x)) (List.filter (fun x -> (f2 x) = t2) eqns) in
      let eqns2 = List.map (fun x -> (f1 x, t2)) (List.filter (fun x -> (f2 x) = t1) eqns) in
      (eqns1, eqns2)
  in let rec add eqns =
      match eqns with
      | [] -> []
      | (t1, t2) :: tl -> let set1 = (match (t1, t2) with 
                                       | (TArrow (t_1, t_2), TArrow (t_3, t_4)) -> [ (t_1, t_3); (t_2, t_4) ]
                                       | _ -> []) in 
                          let trans1 = getTransitive t1 t2 snd fst (set1 @ eqns) and trans2 = getTransitive t1 t2 fst snd (set1 @ eqns) in
                          let set2 = merge (merge (merge (fst trans1) (snd trans1)) (fst trans2)) (snd trans2) in
                          merge (merge (merge set1 set2) [ (t1, t2) ]) (add tl)
  in let eqns1 = add eqns in let eqns2 = add eqns1
  in let mismatch = fun eqn -> match eqn with (t1, t2) -> t1 != t2
  in List.filter mismatch (if (List.length eqns1) = (List.length eqns2) then eqns1 else performClosure eqns2);;

(* Check the closure for immediate inconsistencies *) 
let rec closed eqns =
  match eqns with
  | [] -> true
  | (TArrow (_, _), TInt) :: tl -> false
  | (TArrow (_, _), TBool) :: tl -> false
  | (TInt, TArrow (_, _)) :: tl -> false
  | (TBool, TArrow (_, _)) :: tl -> false
  | (TInt, TBool) :: tl -> false
  | (TBool, TInt) :: tl -> false
  | _ :: tl -> closed tl;;

(* Solve equations *)
let rec solve t eqns =
  let subHelper t1 t2 = (match (t1, t2) with
                       | (_, TInt) -> t2
                       | (_, TBool) -> t2
                       | (TVar _, TArrow (_, _)) -> t2
                       | (TVar a, TVar b) -> if (Char.code b.[0]) > (Char.code a.[0]) then t2 else t1
                       | _ -> t1) in
  let rec substituteVar t eqns = (match eqns with
                                  | [] -> t
                                  | (t1, t2) :: tl -> let tVar = if t = t1 then subHelper t t2 else (if t = t2 then subHelper t t1 else t) in
                                                      substituteVar tVar tl) in
  let rec substitute t eqns = (match t with
                               | TInt -> t
                               | TBool -> t
                               | TArrow (t1, t2) -> TArrow (substitute t1 eqns, substitute t2 eqns)
                               | TVar _ -> substituteVar t eqns
                               | _ -> raise (TypeError ("Unrecognized type"))) in
  let t1 = substitute t eqns in
  let t2 = substitute t1 eqns in
  if t1 = t2 then t2 else solve t2 eqns;;

(* Perform typecheck *)
let typecheck e =
  ascii := 97; 
  let eqTypes = generate e [] in
  let eqns = performClosure (snd eqTypes) in
  if closed eqns then solve (fst eqTypes) eqns else raise (TypeInferenceFailure "immediately inconsistent types");;
