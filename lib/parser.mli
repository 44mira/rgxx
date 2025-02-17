(** Private parser type *)
type t

val init : Lexer.t -> t
val parse : t -> (t * Node.expression, string) result

(** Pretty printing *)

val pp : Format.formatter -> t -> unit
val show : t -> string
