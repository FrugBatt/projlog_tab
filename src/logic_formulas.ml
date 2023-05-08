(** Types & Modules **)

type formula =
  | True
  | False
  | Var of int
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Forall of int * formula
  | Exists of int * formula
  | Predicate of int * formula list

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
  | TMetaFunction of int * tree_formula list
  | TPredicate of int * tree_formula list

module IntSet = Set.Make (Int)


(** Values **)

let get_meta_val =
  let counter = ref (-1) in
  fun () -> incr counter; !counter


(** MetaVar **)

let rec get_meta f =
  let open IntSet in
  match f with
  | TTrue -> empty
  | TFalse -> empty
  | TVar _ -> empty
  | TMetaVar i -> singleton i
  | TNot f -> get_meta f
  | TAnd (f1, f2) -> union (get_meta f1) (get_meta f2)
  | TOr (f1, f2) -> union (get_meta f1) (get_meta f2)
  | TForall (_, f) -> get_meta f
  | TExists (_, f) -> get_meta f
  | TMetaFunction _ -> empty
  | TPredicate (_, l) -> List.fold_left (fun s f -> union s (get_meta f)) empty l

let rec meta_formula_of_var_formula ivar imeta = function
  | TTrue -> TTrue
  | TFalse -> TFalse
  | TVar j -> 
      if ivar = j then TMetaVar imeta
      else TVar j
  | TMetaVar j -> TMetaVar j
  | TNot f -> TNot (meta_formula_of_var_formula ivar imeta f)
  | TAnd (f1, f2) -> TAnd (meta_formula_of_var_formula ivar imeta f1, meta_formula_of_var_formula ivar imeta f2)
  | TOr (f1, f2) -> TOr (meta_formula_of_var_formula ivar imeta f1, meta_formula_of_var_formula ivar imeta f2)
  | TForall (j, f) ->
      if ivar = j then TForall (j, f)
      else TForall (j, meta_formula_of_var_formula ivar imeta f)
  | TExists (j, f) ->
      if ivar = j then TExists (j, f)
      else TExists (j, meta_formula_of_var_formula ivar imeta f)
  | TMetaFunction (j, l) -> TMetaFunction (j, l)
  | TPredicate (j, l) -> TPredicate (j, List.map (meta_formula_of_var_formula ivar imeta) l)


(** Substitution **)

(* let rec substitute_var i form frep = match form with *)
(*   | TTrue -> TTrue *)
(*   | TFalse -> TFalse *)
(*   | TVar j -> *)
(*       if i = j then frep *)
(*       else TVar j *)
(*   | TMetaVar j -> TMetaVar j *)
(*   | TNot f -> TNot (substitute_var i f frep) *)
(*   | TAnd (f1, f2) -> TAnd (substitute_var i f1 frep, substitute_var i f2 frep) *)
(*   | TOr (f1, f2) -> TOr (substitute_var i f1 frep, substitute_var i f2 frep) *)
(*   | TForall (j, f) -> *)
(*       if i = j then TForall (j, f) *)
(*       else TForall (j, substitute_var i f frep) *)
(*   | TExists (j, f) -> *)
(*       if i = j then TExists (j, f) *)
(*       else TExists (j, substitute_var i f frep) *)
(*   | TMetaFunction (j, l) -> TMetaFunction (j, l) *)
(*   | TPredicate (j, l) -> TPredicate (j, List.map (fun f -> substitute_var i f frep) l) *)

let rec substitute form a b =
  match form with
  | f when f = a -> b
  | TTrue -> TTrue
  | TFalse -> TFalse
  | TVar i -> TVar i
  | TMetaVar i -> TMetaVar i
  | TNot f -> TNot (substitute f a b)
  | TAnd (f1, f2) -> TAnd (substitute f1 a b, substitute f2 a b)
  | TOr (f1, f2) -> TOr (substitute f1 a b, substitute f2 a b)
  | TForall (i, f) ->
    if a = TVar i then TForall (i, f)
    else TForall (i, substitute f a b)
  | TExists (i, f) ->
    if a = TVar i then TExists (i, f)
    else TExists (i, substitute f a b)
  | TMetaFunction (i, l) -> TMetaFunction (i, List.map (fun f -> substitute f a b) l)
  | TPredicate (i, l) -> TPredicate (i, List.map (fun f -> substitute f a b) l)

let substitute_var i form frep = substitute form (TVar i) frep

(** Conversion **)

let rec tree_formula_of_formula form =
  match form with
  | True -> TTrue
  | False -> TFalse
  | Not f -> TNot (tree_formula_of_formula f)
  | Var i -> TVar i
  | And (f1, f2) -> TAnd (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Or (f1, f2) -> TOr (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Forall (i, f) -> TForall (i, tree_formula_of_formula f)
  | Exists (i, f) -> TExists (i, tree_formula_of_formula f)
  | Predicate (i, l) -> TPredicate (i, List.map tree_formula_of_formula l)


(** Fonctions de simplification **)

let rec simple_form tf = 
  match tf with
  | TNot (TAnd (f1,f2)) -> TOr (simple_form (TNot f1), simple_form (TNot f2))
  | TNot (TOr (f1,f2)) -> TAnd (simple_form (TNot f1), simple_form (TNot f2))
  | TNot (TNot f) -> simple_form f
  | TNot (TForall (i,f)) -> TExists (i,simple_form (TNot f))
  | TNot (TExists (i,f)) -> TForall (i,simple_form (TNot f))
  | TNot f -> TNot (simple_form f)
  | TOr (f1,f2) -> TOr (simple_form f1, simple_form f2)
  | TAnd (f1,f2) -> TAnd (simple_form f1, simple_form f2)
  | TForall (i,f) -> TForall (i,simple_form f)
  | TExists (i,f) -> TExists (i,simple_form f)
  | tf -> tf


