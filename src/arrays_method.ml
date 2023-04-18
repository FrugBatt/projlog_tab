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

type tree = Nil | Node of tree_formula * bool * tree * tree

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

let tree_of_formula f = Node (f, false, Nil, Nil)

let rec tree_of_formula_list l =
  match l with
  | [] -> Nil
  | a :: q ->
      Node (tree_formula_of_formula a, false, tree_of_formula_list q, Nil)

let rec leaf_append_one tree tapp =
  match tree with
  | Nil -> tapp
  | Node (form, b, Nil, Nil) -> Node (form, b, tapp, Nil)
  | Node (form, b, t, Nil) -> Node (form, b, leaf_append_one t tapp, Nil)
  | Node (form, b, Nil, t) -> Node (form, b, Nil, leaf_append_one t tapp)
  | Node (form, b, t1, t2) ->
      Node (form, b, leaf_append_one t1 tapp, leaf_append_one t2 tapp)

let rec leaf_append_two tree tapp1 tapp2 =
  match tree with
  | Nil -> failwith "Empty tree"
  | Node(form,b,Nil,Nil) -> Node(form,b,tapp1,tapp2)
  | Node(form,b,t,Nil) -> Node(form,b,leaf_append_two t tapp1 tapp2,Nil)
  | Node (form,b,Nil,t) -> Node(form,b,Nil,leaf_append_two t tapp1 tapp2)
  | Node (form,b,t1,t2)-> Node(form,b,leaf_append_two t1 tapp1 tapp2,leaf_append_two t2 tapp1 tapp2)

let rec search_delta l = 
  match l with  
  | [] -> failwith "No delta term"
  | TExists(v,f)::q -> (TExists(v,f),q)
  | node::q -> let a,b = search_delta q in (a,node::b)

let rec beta_break = function
  | Nil -> (Nil, false)
  | Node (TOr (f1, f2), false, t1, t2) ->
      let t = Node (TOr (f1, f2), true, t1, t2) and n1 = tree_of_formula f1 and n2 = tree_of_formula f2 in
      (leaf_append_two t n1 n2, true)
  | Node (f, b, t1, t2) ->
      let t1', b1 = beta_break t1 in
      if b1 then (Node (f, b, t1', t2), true)
      else
        let t2', b2 = beta_break t2 in
        (Node (f, b, t1, t2'), b2)

let rec gamma_break = function
  | Nil -> (Nil, false)
  | Node (TForall (i, f), false, t1, t2) ->
      let t = Node (TForall (i, f), true, t1, t2) and n = Node (meta_formula_of_var_formula i f, false, Nil, Nil) in
      (leaf_append_one t n, true)
  | Node (f, b, t1, t2) ->
      let t1', b1 = gamma_break t1 in
      if b1 then (Node (f, b, t1', t2), true)
      else
        let t2', b2 = gamma_break t2 in
        (Node (f, b, t1, t2'), b2)

let rec alpha_break tree =
  match tree with
  | Nil -> (Nil, false)
  | Node (TAnd (f1, f2), false, t1, t2) ->
      let t = Node (TAnd (f1, f2), true, t1, t2) in
      let n = Node (f1, false, Node (f2, false, Nil, Nil), Nil) in
      (leaf_append_one t n, true)
  | Node (f, b, t1, t2) ->
      let t12, b1 = alpha_break t1 in
      if b1 then (Node (f, b, t12, t2), b1)
      else
        let t22, b2 = alpha_break t2 in
        (Node (f, b, t1, t22), b2)
