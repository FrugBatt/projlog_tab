open Logic_formulas
open Arrays_method

(** Types **)

type closers = (tree_formula * tree_formula) list
[@@deriving show]

(** Closers **)

let rec get_branchs t =
  match t with
  | Nil -> []
  | Node {left = Nil; right = Nil; _} -> [t]
  | Node n -> (get_branchs n.left) @ (get_branchs n.right)

let rec get_branch_formulas branch =
  match branch with
  | Nil -> []
  | Node ({broke = false; _ } as n) -> n.formula :: get_branch_formulas n.father
  | Node n -> get_branch_formulas n.father

let get_branch_closers branch =
  let open Non_deterministic in
  take_two (get_branch_formulas branch) >>= fun (f1, f2) ->
    let nf1 = simple_form (TNot f1) in
    return (nf1, f2)

let rec every_closers closers =
  match closers with
  | [] -> [[]]
  | h :: t ->
    let rval = every_closers t in
    List.flatten (List.map (fun elt -> List.map (fun elt2 -> elt :: elt2) rval) h)

let get_branchs_closers branchs =
  let branch_closers = List.map (fun b -> Non_deterministic.run (get_branch_closers b)) branchs in
  let closers = every_closers branch_closers in
  let closers_monad = List.map Non_deterministic.return closers in
  Non_deterministic.choice closers_monad


(** Unification algorithm **)
(* let rec eval_eq f1 f2 = *)
(*   match (f1, f2) with *)
(*   | (TTrue, TTrue) -> ETrue *)
(*   | (TTrue, f) -> if is_modifiable f then EUnification else EFalse *)
(*   | (TFalse, TFalse) -> ETrue *)
(*   | (TFalse, f) -> if is_modifiable f then EUnification else EFalse *)
(*   | (TVar i, TVar j) when i = j -> ETrue *)
(*   | (TVar _, _) -> EFalse *)
(*   | (TMetaVar _, _) -> EUnification *)
(*   | (TNot f1, TNot f2) -> eval_eq f1 f2 *)
(*   | (TNot _, TMetaVar _) -> EUnification *)
(*   | (TNot _, TMetaFunction _) -> EUnification *)
(*   | (TNot _, _) -> EFalse *)
(*   | (TAnd _, _) -> EFalse (* déjà consomé *) *)
(*   | (TOr _, _) -> EFalse (* déjà consomé *) *)
(*   | (TForall _, _) -> EFalse (* déjà consomé *) *)
(*   | (TExists _, _) -> EFalse (* déjà consomé *) *)
(*   | (TMetaFunction _, _) -> EUnification *)
(*   | (TPredicate (i, l1), TPredicate (j, l2)) when i = j  -> if (List.exists is_modifiable l1) || (List.exists is_modifiable l2) then EUnification else if l1 = l2 then ETrue else EFalse *)
(*   | (TPredicate _, _) -> EFalse *)


