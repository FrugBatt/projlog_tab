type 'a t

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val choice : 'a t list -> 'a t
val fail : 'a t
val either : 'a t -> 'a t -> 'a t
val (|||) : 'a t -> 'a t -> 'a t
val run : 'a t -> 'a list

val take_two : 'a list -> ('a * 'a) t
