(** The main evaluator type *)
type t

val init : string -> t
val evaluate : Node.expression -> t -> bool
