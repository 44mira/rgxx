open Sexplib.Std

type expression =
  | Term of term
  | Alternate of
      { lhs : expression
      ; rhs : expression
      }
[@@deriving show { with_path = false }, sexp]

and term =
  | Letter of char
  | Concat of term list
  | Group of expression
  | Repeat of term
  | Lambda
[@@deriving show { with_path = false }, sexp]
