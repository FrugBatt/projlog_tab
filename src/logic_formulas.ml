type formula =
  | True
  | False
  | Var of int
  | And of formula * formula
  | Or of formula * formula
  | Forall of int * formula
  | Exists of int * formula
(* A rajouter les prédicats ? *)
