type tree_formula =
  | TTrue
  | TFalse
  | TVar of int
  | TMetaVar of int
  | TAnd of tree_formula * tree_formula
  | TOr of tree_formula * tree_formula
  | TForall of int * tree_formula
  | TExists of int * tree_formula
(* A rajouter les prédicats ? *)

type tree =
  | Leaf of tree_formula
  | Unary of tree_formula * tree
  | Binary of tree_formula * tree * tree


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

let tree_of_formula_list l =
  let mapped = List.map tree_formula_of_formula l in
  assert false
