open Logic_formulas
open Closers
open Unification
open Arrays_method

(** Args **)

let is_verbose = ref false
let is_latex = ref false
let input_file = ref ""
let output_file = ref ""

let anon_fun file = input_file := file

let usage = "usage: " ^ Sys.argv.(0) ^ " [-v] [-latex] file [-o output_file]"

let speclist = [
  ("-v", Arg.Set is_verbose, "Prints the effective closer");
  ("-latex", Arg.Set is_latex, "Prints the tree in latex");
  ("-o", Arg.Set_string output_file, "Sets the output file")
]


(** Solver **)

let write_file msg =
  let f = if !output_file = "" then "output.dot" else !output_file in
  let oc = open_out f in
  Printf.fprintf oc "%s\n" msg;
  close_out oc

let solve_formula tfl =
  let tree = tree_of_tree_formula_list tfl in
  tree_break tree;
  if !is_latex then Printf.printf "Tree :\n%s\n" (Render.render_tree tree);
  let branchs = get_branchs tree in
  let closers_nondet = get_branchs_closers branchs in
  let closers = Non_deterministic.run closers_nondet in
  match List.find_opt unification closers with
  | None -> Printf.printf "No unification found\n"
  | Some cl ->
    let cl_clean = List.map (fun (f1, f2) -> (simple_form (TNot f1), f2)) cl in
    if !is_verbose then Printf.printf "Unification found :\n%s\n" (show_closers cl_clean);
    write_file (Render.render tree cl_clean)

let is_satisfiable f =
  let tf = tree_formula_of_formula f in
  let ntf = simple_form (TNot tf) in
  solve_formula [ntf]


(** Main **)

let _ = 
  Arg.parse speclist anon_fun usage;
  if !input_file = "" then (Arg.usage speclist usage; exit 1);
  let lexbuf = Lexing.from_channel (open_in !input_file) in let lform = Parser.parse Lexer.token lexbuf in is_satisfiable lform
