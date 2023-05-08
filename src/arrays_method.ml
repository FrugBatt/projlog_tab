open Logic_formulas

(** Types **)

type tree =
  | Nil
  | Node of { formula: tree_formula; mutable broke: bool; mutable left: tree; mutable right: tree; mutable father: tree }


(** Fonctions de modification **)

let set_left t left =
  match t with
  | Nil -> ()
  | Node n -> n.left <- left

let set_right t right =
  match t with
  | Nil -> ()
  | Node n -> n.right <- right 

let set_broke t broke =
  match t with
  | Nil -> ()
  | Node n -> n.broke <- broke

let set_father t father =
  match t with
  | Nil -> ()
  | Node n -> n.father <- father

let get_formula t =
  match t with
  | Nil -> TTrue
  | Node n -> n.formula

let rec depth tree n =
  if (get_formula tree) = (get_formula n) then 0
  else match tree with
    | Nil -> -1
    | Node node ->
      let ld = depth node.left n and rd = depth node.right n in
        if ld = -1 && rd = -1 then -1
        else if ld = -1 then rd + 1
        else ld + 1

let rec size tree =
  match tree with
  | Nil -> 0
  | Node node -> 1 + max (size node.left) (size node.right)

(** Fonctions de conversion **)

let tree_of_formula f = Node {formula = f; broke = false; left = Nil; right = Nil; father = Nil}

let rec tree_of_tree_formula_list l =
  match l with
    | [] -> Nil
    | a::q ->
      let t' = tree_of_tree_formula_list q in
      let t = Node {formula = a; broke = false; left = t'; right = Nil; father = Nil} in
      set_father t' t;
      t


(** Fonctions d'ajout **)    


let rec leaf_append_one tree tapp =
  match tree with
  | Nil -> ()
  | Node {left = Nil; right = Nil; _} -> set_left tree tapp; set_father tapp tree
  | Node n ->
    leaf_append_one n.left tapp;
    leaf_append_one n.right tapp

let rec leaf_append_two tree tapp1 tapp2 = 
  match tree with
  | Nil -> ()
  | Node {left = Nil; right = Nil; _} -> set_left tree tapp1; set_right tree tapp2; set_father tapp1 tree; set_father tapp2 tree
  | Node n ->
    leaf_append_two n.left tapp1 tapp2;
    leaf_append_two n.right tapp1 tapp2

let rec leaf_append_with_meta tree i f =
  match tree with
  | Nil -> ()
  | Node {left = Nil; right = Nil; _} ->
    let meta_formula = meta_formula_of_var_formula i (get_meta_val ()) f in
    let tapp = tree_of_formula meta_formula in
    set_left tree tapp;
    set_father tapp tree
  | Node n ->
    leaf_append_with_meta n.left i f;
    leaf_append_with_meta n.right i f
  

(** Fonctions de parcours **)

let get_meta_higher t =
  let open StringSet in
  let rec aux = function
  | Nil -> empty
  | Node n -> union (get_meta n.formula) (aux n.father)
  in List.of_seq (to_seq (aux t))

(** Fonctions de destruction **)

let rec alpha_break t = 
  match t with
    | Nil -> false
    | Node {formula=TAnd(f1,f2) ; broke = false ; _ } -> 
      let tapp1 = tree_of_formula f1 and tapp2 = tree_of_formula f2 in
      set_left tapp1 tapp2;
      set_father tapp2 tapp1;
      leaf_append_one t tapp1;
      set_broke t true;
      true
    | Node n -> alpha_break n.left || alpha_break n.right

let rec beta_break t =
  match t with
  | Nil -> false
  | Node {formula = TOr (f1, f2); broke = false; _} ->
      let tapp1 = tree_of_formula f1 and tapp2 = tree_of_formula f2 in
      leaf_append_two t tapp1 tapp2;
      set_broke t true;
      true
  | Node n -> beta_break n.left || beta_break n.right

let rec gamma_break t =
  match t with
  | Nil -> false
  | Node {formula = TForall (i, f); broke = false; _} ->
      leaf_append_with_meta t i f;
      set_broke t true;
      true
  | Node n -> gamma_break n.left || gamma_break n.right

let rec delta_break t =
  match t with
  | Nil -> false
  | Node {formula = TExists (i, f); broke = false; _} ->
    let metas = get_meta_higher t in
    let fmod = substitute_var i f (TMetaFunction (get_meta_val (), List.map (fun i -> TMetaVar i) metas)) in
    let tapp = tree_of_formula fmod in
    leaf_append_one t tapp;
    set_broke t true;
    true
  | Node n -> delta_break n.left || delta_break n.right


(** Fonction de génération de l'arbre (avant remontée) **)

let tree_break t = while alpha_break t || delta_break t || beta_break t || gamma_break t do () done; ()

let tree_init_single form =
  let tform = tree_formula_of_formula form in
  tree_break (tree_of_formula (simple_form (TNot tform)))

let tree_init (form_list : formula list) =
  tree_break
    (tree_of_tree_formula_list
       (List.map simple_form (List.map tree_formula_of_formula form_list)))

