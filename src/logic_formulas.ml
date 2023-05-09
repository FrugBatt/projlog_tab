(** Types & Modules **)

type formula =
  | True
  | False
  | Var of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Forall of string * formula
  | Exists of string * formula
  | Predicate of string * formula list
  | Impl of formula * formula

type tree_formula =
  | TTrue
  | TFalse
  | TVar of string
  | TMetaVar of string
  | TNot of tree_formula
  | TAnd of tree_formula * tree_formula
  | TOr of tree_formula * tree_formula
  | TForall of string * tree_formula
  | TExists of string * tree_formula
  | TMetaFunction of string * tree_formula list
  | TPredicate of string * tree_formula list
[@@deriving show]

module StringSet = Set.Make (String)


(** Values **)

let set_string s i c = String.mapi (fun j c' -> if i = j then c else c') s

let incr_string s =
  let n = String.length s in
  let rec aux i =
    if i = -1 then
      "A" ^ s
    else if s.[i] = 'Z' then
      let ns = aux (i + 1) in
      set_string ns i 'A'
    else
      set_string s i (Char.chr (Char.code s.[i] + 1))
  in
  aux (n-1)

let get_meta_val =
  let counter = ref "" in
  fun () -> counter := incr_string !counter; !counter


(** MetaVar **)

let rec get_meta f =
  let open StringSet in
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

let rec is_modifiable f =
  match f with
  | TTrue -> false
  | TFalse -> false
  | TVar _ -> false
  | TMetaVar _ -> true
  | TNot f -> is_modifiable f
  | TAnd (f1, f2) -> is_modifiable f1 || is_modifiable f2
  | TOr (f1, f2) -> is_modifiable f1 || is_modifiable f2
  | TForall (_, f) -> is_modifiable f
  | TExists (_, f) -> is_modifiable f
  | TMetaFunction _ -> true
  | TPredicate (_, l) -> List.exists is_modifiable l

(** Substitution **)

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
  | Impl (f1, f2) -> TOr (TNot (tree_formula_of_formula f1), tree_formula_of_formula f2)


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


