type tree_formula =
  | TTrue
  | TFalse
  | TVar of int
  | TMetaVar of int
  | TNot of tree_formula
  | TAnd of tree_formula * tree_formula
  | TOr of tree_formula * tree_formula
  | TForall of int * tree_formula
  | TExists of int * tree_formula
(* A rajouter les prédicats ? *)

type tree =
  | Nil
  | Unary of tree_formula * bool * tree
  | Binary of tree_formula * bool * tree * tree


let rec tree_formula_of_formula form =
  let open Logic_formulas in
  match form with
  | True -> TTrue
  | False -> TFalse
  | Var i -> TVar i
  | And (f1, f2) -> TAnd (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Or (f1, f2) -> TOr (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Forall (i, f) -> TForall (i, tree_formula_of_formula f)
  | Exists (i, f) -> TExists (i, tree_formula_of_formula f)

let rec tree_of_formula_list l =
  match l with 
  | [] -> Nil
  | a::q -> Unary(tree_formula_of_formula a, false, tree_of_formula_list q)
    

let rec search_alpha l = 
  match l with  
  | [] -> failwith "No alpha term"
  | TAnd(f1,f2)::q -> (TAnd(f1,f2),q)
  | node::q -> let a,b = search_alpha q in (a,node::b)

let rec search_delta l = 
  match l with  
  | [] -> failwith "No delta term"
  | TExists(v,f)::q -> (TExists(v,f),q)
  | node::q -> let a,b = search_alpha q in (a,node::b)

let rec search_beta l = 
  match l with  
  | [] -> failwith "No beta term"
  | TOr(f1,f2)::q -> (TOr(f1,f2),q)
  | node::q -> let a,b = search_alpha q in (a,node::b)

let rec search_gamma l = 
  match l with  
  | [] -> failwith "No gamma term"
  | TForall(v,f)::q -> (TForall(v,f),q)
  | node::q -> let a,b = search_alpha q in (a,node::b)
