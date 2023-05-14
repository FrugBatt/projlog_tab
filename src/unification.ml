open Logic_formulas

(** Set of meta variables **)

module FormulaCmp = struct
  type t = tree_formula
  let compare = compare
end

module FormulaSet = Set.Make(FormulaCmp)


(** Unification algorithm **)

let substitute_unif l a b = List.map (fun (f1, f2) -> (substitute f1 a b, substitute f2 a b)) l

let rec has_formula f a =
  match f with
  | f when f = a -> true
  | TTrue -> false
  | TFalse -> false
  | TVar _ -> false
  | TMetaVar _ -> false
  | TNot f -> has_formula f a
  | TAnd (f1, f2) -> has_formula f1 a || has_formula f2 a
  | TOr (f1, f2) -> has_formula f1 a || has_formula f2 a
  | TForall (_, f) -> has_formula f a
  | TExists (_, f) -> has_formula f a
  | TMetaFunction (_, l) -> List.exists (fun f -> has_formula f a) l
  | TPredicate (_, l) -> List.exists (fun f -> has_formula f a) l

let rec unification l =
  match l with
  | [] -> true
  | (f1, f2) :: t when f1 = f2 -> unification t
  | (f1, f2) :: _ when not (is_modifiable f1) && not (is_modifiable f2) -> false
  | (f1, f2) :: t when not (is_modifiable f1) -> unification ((f2, f1) :: t)
  | (TMetaVar i, f) :: t ->
    if has_formula f (TMetaVar i) then false
    else
      unification (substitute_unif t (TMetaVar i) f)
  | (TMetaFunction (i, []), f) :: t ->
    if is_modifiable f then false
    else unification (substitute_unif t (TMetaFunction (i, [])) f)
  | (TMetaFunction (i, l1), TMetaFunction (j, l2)) :: t ->
    if i = j then false
    else
      let ivars = FormulaSet.of_list l1 and jvars = FormulaSet.of_list l2 in
      let intersection = FormulaSet.inter ivars jvars in
        if FormulaSet.is_empty intersection then false
        else
          let new_form = TMetaFunction (get_meta_val (), List.of_seq (FormulaSet.to_seq intersection)) in
          unification (substitute_unif (substitute_unif t (TMetaFunction (i, l1)) new_form) (TMetaFunction (j, l2)) new_form)
  | (f, TMetaFunction (i, l)) :: t -> unification ((TMetaFunction (i, l), f) :: t)
  | (TMetaFunction (i, l), f) :: t ->
    let ivars = FormulaSet.of_list l in
    let depvars = FormulaSet.filter (fun f' -> has_formula f f') ivars in
    if FormulaSet.is_empty depvars then false
    else
      unification (substitute_unif t (TMetaFunction (i, l)) f)
  | (TPredicate (i, l1), TPredicate (j, l2)) :: t ->
    if i <> j || (List.length l1) <> (List.length l2) then false
    else unification ((List.combine l1 l2) @ t)
  | _ -> false

