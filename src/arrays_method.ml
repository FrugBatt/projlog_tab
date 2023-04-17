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
