type formula =
  | True
  | False
  | Var of int
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Forall of int * formula
  | Exists of int * formula
(* A rajouter les prédicats ? *)

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
  | TMetaFunction of int list


let rec meta_formula_of_var_formula i = function
  | TTrue -> TTrue
  | TFalse -> TFalse
  | TVar j -> 
      if i = j then TMetaVar i
      else TVar j
  | TMetaVar j -> TMetaVar j
  | TNot f -> TNot (meta_formula_of_var_formula i f)
  | TAnd (f1, f2) -> TAnd (meta_formula_of_var_formula i f1, meta_formula_of_var_formula i f2)
  | TOr (f1, f2) -> TOr (meta_formula_of_var_formula i f1, meta_formula_of_var_formula i f2)
  | TForall (j, f) ->
      if i = j then TForall (j, f)
      else TForall (j, meta_formula_of_var_formula i f)
  | TExists (j, f) ->
      if i = j then TExists (j, f)
      else TExists (j, meta_formula_of_var_formula i f)
  | TMetaFunction l -> TMetaFunction l


