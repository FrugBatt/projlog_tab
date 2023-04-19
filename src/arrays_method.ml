open Logic_formulas

(** Types **)

type tree =
  | Nil
  | Node of { formula: tree_formula; broke: bool; left: tree; right: tree }

(** Fonctions de conversion **)


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

let tree_of_formula f = Node {formula = f; broke = false; left = Nil; right = Nil}

let rec tree_of_formula_list l =
  match l with
    | [] -> Nil
    | a::q -> Node {formula = tree_formula_of_formula a; broke = false; left = tree_of_formula_list q; right = Nil}

let rec tree_of_tree_formula_list l =
  match l with
    | [] -> Nil
    | a::q -> Node {formula =  a; broke = false; left = tree_of_tree_formula_list q; right = Nil}


(** Fonctions d'ajout **)    


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
  

(** Fonctions de parcours **)

let get_meta_higher t n =
  let rec aux = function
    | Nil -> ([], false)
    | Node ({formula = TMetaVar i; _} as node) ->
        let metas, b = aux node.left in
        if b then
          if List.mem i metas then (metas, true)
          else (i::metas, true)
        else
          let metas', b' = aux node.right in
            if b' then
              if List.mem i metas' then (metas, true)
              else (i::metas, true)
            else ([], false)
    | Node node as t ->
        if t == n then ([], true)
        else
          let metas, b = aux node.left in
          if b then (metas, true)
          else
            aux node.right
  in fst (aux t)

let rec replace_vars i form frep = match form with
  | TTrue -> TTrue
  | TFalse -> TFalse
  | TVar j ->
      if i = j then frep
      else TVar j
  | TMetaVar j -> TMetaVar j
  | TNot f -> TNot (replace_vars i f frep)
  | TAnd (f1, f2) -> TAnd (replace_vars i f1 frep, replace_vars i f2 frep)
  | TOr (f1, f2) -> TOr (replace_vars i f1 frep, replace_vars i f2 frep)
  | TForall (j, f) ->
      if i = j then TForall (j, f)
      else TForall (j, replace_vars i f frep)
  | TExists (j, f) ->
      if i = j then TExists (j, f)
      else TExists (j, replace_vars i f frep)
  | TMetaFunction l -> TMetaFunction l


(** Fonctions de destruction **)

let delta_break t =
  let rec delta_break_aux = function
  | Nil -> (Nil, false)
  | Node ({formula = TExists (i, f); broke = false; _} as n) as node ->
      let t' = Node {n with broke = true} in
      let l = get_meta_higher t node in
      let n' = Node {formula = replace_vars i f (TMetaFunction l); broke = false; left = Nil; right = Nil} in
      (leaf_append_one t' n', true)
  | Node n ->
      let t1', b1 = delta_break_aux n.left in
      if b1 then (Node {n with left = t1'}, true)
      else
        let t2', b2 = delta_break_aux n.right in
        (Node {n with right = t2'}, b2)
  in delta_break_aux t

let rec alpha_break t = 
  match t with
    | Nil -> (Nil,false)
    | Node ({formula=TAnd(f1,f2) ; broke = false ; _ } as t) -> 
      let n = Node { formula = f1 ; broke = false ; left = tree_of_formula f2 ; right = Nil } in 
      (leaf_append_one (Node {t with broke = true}) n,true)
    | Node ({ left = t1 ; right = t2 ; _ } as t) -> 
      let t12, b1 = alpha_break t1 in 
      if b1 then 
        Node {t with left = t12},b1 
      else 
        let t22, b2 = alpha_break t2 in 
        Node {t with right = t22},b2

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
      let t = Node {n with broke = true} and node = Node {formula = meta_formula_of_var_formula i f; broke = false; left = Nil; right = Nil} in
      (leaf_append_one t node, true)
  | Node n ->
      let t1', b1 = gamma_break n.left in
      if b1 then (Node {n with left = t1'}, true)
      else
        let t2', b2 = gamma_break n.right in
        (Node {n with right = t2'}, b2)

(** Fonctions de simplification **)

let rec simple_form tf = 
  match tf with
    |TNot(TAnd(f1,f2)) ->TOr(simple_form (TNot f1), simple_form (TNot f2))
    |TNot(TOr(f1,f2)) ->TAnd(simple_form (TNot f1), simple_form (TNot f2))
    |TNot(TNot f) -> simple_form f
    |TNot(TForall(i,f)) -> TExists(i,simple_form (TNot f))
    |TNot(TExists(i,f)) -> TForall(i,simple_form (TNot f))
    |TNot(f) -> TNot(simple_form f)
    |TOr (f1,f2) -> TOr (simple_form f1, simple_form f2)
    |TAnd (f1,f2) -> TAnd (simple_form f1, simple_form f2)
    |TForall (i,f) -> TForall (i,simple_form f)
    |TExists (i,f) -> TExists (i,simple_form f)
    | tf -> tf

(** Fonction de génération de l'arbre (avant remontée) **)

let tree_break t = 
  let t_work = ref t in let inchange = ref false in 
    while not !inchange do 
      let a,b = alpha_break !t_work in 
      if not b then 
        let a,b = delta_break !t_work in 
        if not b then 
          let a,b = beta_break !t_work in 
          if not b then 
            let a,b = gamma_break !t_work in inchange := not b ; t_work:=a
        else t_work:=a
      else t_work:= a
    else t_work:=a
done ;
!t_work
;;

let tree_init_single form =
  let tform = tree_formula_of_formula form in
  tree_break (tree_of_formula (simple_form (TNot tform)))

let tree_init (form_list : formula list) =
  tree_break
    (tree_of_tree_formula_list
       (List.map simple_form (List.map tree_formula_of_formula form_list)))