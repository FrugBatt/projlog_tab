open Logic_formulas
open Closers
open Unification
open Arrays_method
open Lexer
open Parser

let () = print_endline "Hello, World!\n"

let write_file msg =
  let oc = open_out "output.dot" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc

  let nom str =
    let n = String.length str in
    if n < 6 then failwith "Nom de fichier problematique"
    else if String.sub str (n - 5) 5 <> ".prop" then failwith "Mauvaise extension"
    else String.sub str 0 (n - 4) ^ "dot"
  
  let write file str =
    let out_channel = open_out file in
    output_string out_channel str

let solve_formula tfl =
  (* let tfl = List.map tree_formula_of_formula fl in *)
  let tree = tree_of_tree_formula_list tfl in
  (* Printf.printf "Initial tree :\n %s\n" (Render.render_tree tree); *)
  tree_break tree;
  (* Printf.printf "Tree after breaking :\n %s\n" (Render.render_tree tree); *)
  let branchs = get_branchs tree in
  let closers_nondet = get_branchs_closers branchs in
  let closers = Non_deterministic.run closers_nondet in
  match List.find_opt unification closers with
  | None -> Printf.printf "No unification found\n"
  | Some cl ->
    let cl_clean = List.map (fun (f1, f2) -> (simple_form (TNot f1), f2)) cl in
    Printf.printf "Unification found :\n %s\n" (show_closers cl_clean);
    write (nom Sys.argv.(1)) (Render.render tree cl_clean)

let is_satisfiable f =
  let tf = tree_formula_of_formula f in
  let ntf = simple_form (TNot tf) in
  solve_formula [ntf]

let f1 = Exists ("x", Impl (Predicate ("R", [Var "x"]), Forall ("y", Predicate ("R", [Var "y"]))))
let f2 = Impl (Exists ("x", Predicate ("R", [Var "x"])), Forall ("y", Predicate ("R", [Var "y"])))
let f3 = Exists ("x", Impl (Predicate ("R", [Var "x"; Var "x"]), Forall ("y", Predicate ("R", [Var "x"; Var "y"]))))
let f4 = Exists ("x", Forall ("y", Impl (Impl (Impl (Predicate ("R", [Predicate ("f", [Var "x"])]), Predicate ("R", [Predicate ("f", [Var "y"])])), Predicate ("R", [Var "x"])), Predicate ("R", [Var "y"]))))

(* let () =
  is_satisfiable f4 *)

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in let lform = Parser.parse Lexer.token lexbuf in is_satisfiable lform
