open Logic_formulas
open Arrays_method

let render_tree_formula f =
  let rec aux f =
    match f with
    | TTrue -> "\\top"
    | TFalse -> "\\bot"
    | TVar i -> i
    | TMetaVar i -> (i ^ "'")
    | TNot f -> "¬" ^ (aux f)
    | TAnd (f1, f2) -> "(" ^ (aux f1) ^ ")∧(" ^ (aux f2) ^ ")"
    | TOr (f1, f2) -> "(" ^ (aux f1) ^ ")∨(" ^ (aux f2) ^ ")"
    | TForall (i, f) -> "∀" ^ i ^ (aux f)
    | TExists (i, f) -> "∃" ^ i ^ (aux f)
    | TMetaFunction (i, l) ->
      let s = String.concat ", " (List.map aux l) in
      i ^ "'(" ^ s ^ ")"
    | TPredicate (i, l) ->
      let s = String.concat ", " (List.map aux l) in
      i ^ "(" ^ s ^ ")"
  in "\"" ^ (aux f) ^ "\""

let render tree closer =
  let n = size tree in
  let tab = Array.make n [] in
  let rec aux t =
    match t with
    | Nil -> ()
    | Node node ->
      let d = depth tree t in
      tab.(d) <- node.formula :: tab.(d);
      aux node.left;
      aux node.right
  in aux tree;
  let le = ref [] in
  let rec aux2 t =
    match t with
    | Nil -> ()
    | Node node ->
        (match node.father with
        | Nil -> ()
        | Node f -> le := (f.formula, node.formula) :: !le);
        aux2 node.left;
        aux2 node.right
  in aux2 tree;
  let prefix = "graph {\nrankdir=\"TB\"\n" and suffix = "}\n" in
  let subgraphs = Array.mapi (fun i l -> "subgraph n" ^ (string_of_int i) ^ " {\nrank=\"same\"\n" ^ (String.concat "\n" (List.map render_tree_formula l)) ^ "\n}\n") tab in
  let subgraphs_str = String.concat "\n" (Array.to_list subgraphs) in
  let edges = List.map (fun (f1, f2) -> (render_tree_formula f1) ^ " -- " ^ (render_tree_formula f2)) !le in
  let edges_str = String.concat "\n" edges in
  let closers = List.map (fun (f1, f2) -> (render_tree_formula f1) ^ " -- " ^ (render_tree_formula f2) ^ " [style=bold]") closer in
  let closers_str = String.concat "\n" closers in
  prefix ^ subgraphs_str ^ edges_str ^ "\n" ^ closers_str ^ "\n" ^ suffix
