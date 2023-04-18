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

type tree =
  | Nil
  | Node of { formula: tree_formula; broke: bool; left: tree; right: tree; father: tree }

let rec tree_formula_of_formula form =
  let open Logic_formulas in
  match form with
  | True -> TTrue
  | False -> TFalse
  | Not f -> TNot (tree_formula_of_formula f)
  | Var i -> TVar i
  | And (f1, f2) -> TAnd (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Or (f1, f2) -> TOr (tree_formula_of_formula f1, tree_formula_of_formula f2)
  | Forall (i, f) -> TForall (i, tree_formula_of_formula f)
  | Exists (i, f) -> TExists (i, tree_formula_of_formula f)


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

let tree_of_formula f = Node {formula = f; broke = false; left = Nil; right = Nil; father = Nil}

let rec tree_of_formula_list l =
  match l with
  | [] -> Nil
  | a::q -> Node {formula = tree_formula_of_formula a; broke = false; left = tree_of_formula_list q; right = Nil; father = Nil} (**TODO**)

let rec leaf_append_one tree tapp =
  match tree with
  | Nil -> Nil
  | Node ({left = Nil; right = Nil; _} as n) -> Node {n with left = tapp}
  | Node n -> Node {n with left = leaf_append_one n.left tapp; right = leaf_append_one n.right tapp}

let rec leaf_append_two tree tapp1 tapp2 =
  match tree with
  | Nil -> Nil
  | Node ({left = Nil; right = Nil; _} as n) -> Node {n with left = tapp1; right = tapp2}
  | Node n -> Node {n with left = leaf_append_two n.left tapp1 tapp2; right = leaf_append_two n.right tapp1 tapp2}

let rec delta_break = function
  | Nil -> (Nil, false)
  | Node ({formula = TExists (_, _); broke = false; _} as n) ->
      let _ = Node {n with broke = true} in assert false
  | Node n ->
      let t1', b1 = delta_break n.left in
      if b1 then (Node {n with left = t1'}, true)
      else
        let t2', b2 = delta_break n.right in
        (Node {n with right = t2'}, b2)

let rec beta_break = function
  | Nil -> (Nil, false)
  | Node ({formula = TOr (f1, f2); broke = false; _} as n) ->
      let t = Node {n with broke = true} and n1 = tree_of_formula f1 and n2 = tree_of_formula f2 in
      (leaf_append_two t n1 n2, true)
  | Node n ->
      let t1', b1 = beta_break n.left in
      if b1 then (Node {n with left = t1'}, true)
      else
        let t2', b2 = beta_break n.right in
        (Node {n with right = t2'}, b2)

let rec gamma_break = function
  | Nil -> (Nil, false)
  | Node ({formula = TForall (i, f); broke = false; _} as n) ->
      let t = Node {n with broke = true} and n = Node {formula = meta_formula_of_var_formula i f; broke = false; left = Nil; right = Nil; father = Nil} in (** TODO **)
      (leaf_append_one t n, true)
  | Node n ->
      let t1', b1 = gamma_break n.left in
      if b1 then (Node {n with left = t1'}, true)
      else
        let t2', b2 = gamma_break n.right in
        (Node {n with right = t2'}, b2)
