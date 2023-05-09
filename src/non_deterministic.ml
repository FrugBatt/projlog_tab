(** A simple monad for nondeterministic computations. **)
type 'a t = 'a list

let return x = [x]

let rec bind m f =
  match m with
  | [] -> []
  | h :: t -> f h @ bind t f

let (>>=) = bind

let choice l = List.flatten l

let fail = []

let either a b = a @ b

let (|||) = either

let run m = List.sort_uniq compare m


let take_one l =
  let rec aux acc res = function
    | [] -> res
    | h :: t -> aux (h :: acc) (return (h, acc @ t) :: res) t
  in choice (aux [] [] l)

let take_two l =
  take_one l >>= fun (h1, t1) ->
  take_one t1 >>= fun (h2, _) ->
    return (h1, h2)
