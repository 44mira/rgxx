open Sexplib.Std

type expression = Expression of term list [@@deriving show { with_path = false }, sexp]
and term = Term of unit' list [@@deriving show { with_path = false }, sexp]

and unit' =
  | Unit of
      { primary : primary
      ; repeat : bool
      }
[@@deriving show { with_path = false }, sexp]

and primary =
  | Letter of char
  | Lambda
  | Group of expression
[@@deriving show { with_path = false }, sexp]
