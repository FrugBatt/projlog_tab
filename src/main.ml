open Logic_formulas
open Closers
open Unification
open Arrays_method

let () = print_endline "Hello, World!\n"

let solve_formula tfl =
  (* let tfl = List.map tree_formula_of_formula fl in *)
  let tree = tree_of_tree_formula_list tfl in
  Printf.printf "Initial tree :\n %s\n" (Render.render_tree tree);
  tree_break tree;
  Printf.printf "Tree after breaking :\n %s\n" (Render.render_tree tree);
  let branchs = get_branchs tree in
  let closers_nondet = get_branchs_closers branchs in
  let closers = Non_deterministic.run closers_nondet in
  match List.find_opt unification closers with
  | None -> Printf.printf "No unification found\n"
  | Some cl ->
    let cl_clean = List.map (fun (f1, f2) -> (simple_form (TNot f1), f2)) cl in
      Printf.printf "Unification found :\n %s\n" (show_closers cl_clean)

let is_satisfiable f =
  let tf = tree_formula_of_formula f in
  let ntf = simple_form (TNot tf) in
  solve_formula [ntf]

let () =
  is_satisfiable (Exists (0, Impl (Predicate (0, [Var 0]), Forall (1, Predicate (0, [Var 1])))))
