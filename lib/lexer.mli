(** Abstract lexer type *)
type t

(** Public functions *)

val init : string -> t
val next_token : t -> t * Token.t option

(** Pretty-printing *)

val pp : Format.formatter -> t -> unit
val show : t -> string
