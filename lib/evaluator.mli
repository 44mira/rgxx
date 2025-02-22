(** The main evaluator type *)
type t

val init : string -> t
val evaluate : Node.expression -> t -> bool
val make_pattern : string -> Node.expression
