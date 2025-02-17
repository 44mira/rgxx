type t =
  { lexer : Lexer.t
  ; curr : Token.t option
  ; peek : Token.t option
  }
[@@deriving show]

let ( let* ) = Result.bind

let advance parser =
  let lexer, next = Lexer.next_token parser.lexer in
  { lexer; curr = parser.peek; peek = next }
;;

let init lexer =
  let parser = { lexer; curr = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser
;;

let rec parse parser =
  let* parser, expr = parse_expression parser in
  Ok (parser, expr)

and parse_expression parser =
  let* parser, term = parse_term parser in
  let parser, terms = collect_expression parser [ term ] in
  Ok (parser, Node.Expression terms)

and parse_term parser =
  let* parser, unit' = parse_unit' parser in
  let parser, units = collect_term (advance parser) [ unit' ] in
  Ok (parser, Node.Term units)

and parse_unit' parser =
  let* parser, primary = parse_primary parser in
  match parser.peek with
  | Some Star -> Ok (advance parser, Node.Unit { primary; repeat = true })
  | _ -> Ok (parser, Node.Unit { primary; repeat = false })

and parse_primary parser =
  match parser.curr with
  | Some Lambda -> Ok (parser, Node.Lambda)
  | Some (Letter a) -> Ok (parser, Node.Letter a)
  | _ -> Error "Invalid primary character."

and collect_expression parser terms =
  match parser.curr with
  | Some Alternate ->
    (match parse_term (advance parser) with
     | Ok (parser, term) -> collect_expression parser (term :: terms)
     | Error _ -> parser, List.rev terms)
  | _ -> parser, List.rev terms

and collect_term parser units =
  match parse_unit' parser with
  | Ok (parser, unit') -> collect_term (advance parser) (unit' :: units)
  | Error _ -> parser, List.rev units
;;

(** Tests {{{*)
let iparse a =
  match parse @@ init @@ Lexer.init a with
  | Error _ -> Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
  | Ok (_, a) -> a
;;

let%test "Parser can parse a single letter" =
  iparse "a" = Expression [ Term [ Unit { primary = Letter 'a'; repeat = false } ] ]
;;

let%test "Parser can parse repeats" =
  iparse "a*" = Expression [ Term [ Unit { primary = Letter 'a'; repeat = true } ] ]
;;

let%test "Parser can parse multiple letters and repeats" =
  iparse "ab*c"
  = Expression
      [ Term
          [ Unit { primary = Letter 'a'; repeat = false }
          ; Unit { primary = Letter 'b'; repeat = true }
          ; Unit { primary = Letter 'c'; repeat = false }
          ]
      ]
;;

let%test "Parser can parse alternates" =
  iparse "ab + ba"
  = Expression
      [ Term
          [ Unit { primary = Letter 'a'; repeat = false }
          ; Unit { primary = Letter 'b'; repeat = false }
          ]
      ; Term
          [ Unit { primary = Letter 'b'; repeat = false }
          ; Unit { primary = Letter 'a'; repeat = false }
          ]
      ]
;;
(*}}}*)
