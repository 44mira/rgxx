(** Abstract lexer type *)
type lexer

(** Public functions *)

val init : string -> lexer
val next_token : lexer -> lexer * Token.token option

(** Pretty-printing *)

val pp_lexer : Format.formatter -> lexer -> unit
val show_lexer : lexer -> string
