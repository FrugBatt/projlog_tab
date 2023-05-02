open Logic_formulas
open Arrays_method

let render_tree_formula f =
  let rec aux f =
    match f with
    | TTrue -> "\\top"
    | TFalse -> "\\bot"
    | TVar i -> "x_" ^ (string_of_int i)
    | TMetaVar i -> "X_" ^ (string_of_int i)
    | TNot f -> "\\lnot " ^ (aux f)
    | TAnd (f1, f2) -> (aux f1) ^ " \\land " ^ (aux f2)
    | TOr (f1, f2) -> (aux f1) ^ " \\lor " ^ (aux f2)
    | TForall (i, f) -> "\\forall x_{" ^ (string_of_int i) ^ "} " ^ (aux f)
    | TExists (i, f) -> "\\exists x_{" ^ (string_of_int i) ^ "} " ^ (aux f)
    | TMetaFunction (i, l) ->
      let s = String.concat ", " (List.map (fun i -> "X_{" ^ (string_of_int i) ^ "}") l) in
      "f_{" ^ (string_of_int i) ^ "}(" ^ s ^ ")"
    | TPredicate (i, l) ->
      let s = String.concat ", " (List.map aux l) in
      "P_{" ^ (string_of_int i) ^ "}(" ^ s ^ ")"
  in "$" ^ (aux f) ^ "$"

let render_tree t =
  let prefix = "\\begin{tikzpicture}\n\\node{Nil}" and suffix = ";\n\\end{tikzpicture}" in
  let rec aux t =
    match t with
    | Nil -> ""
    | Node n ->
        let pref = "child {node {" ^ (render_tree_formula n.formula) ^ "}\n" and suff = "}\n" in
        let left_render = aux n.left and right_render = aux n.right in
        pref ^ left_render ^ right_render ^ suff
  in prefix ^ (aux t) ^ suffix
