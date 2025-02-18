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
  let* parser, terms = collect_expression parser [ term ] in
  Ok (parser, Node.Expression terms)

and parse_term parser =
  let* parser, unit' = parse_unit' parser in
  let* parser, units = collect_term (advance parser) [ unit' ] in
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
  | Some LeftParen -> collect_group (advance parser)
  | _ -> Error "Invalid primary character."

and collect_expression parser terms =
  match parser.curr with
  | Some Alternate ->
    let* parser, term = parse_term (advance parser) in
    collect_expression parser (term :: terms)
  | _ -> Ok (parser, List.rev terms)

and collect_term parser units =
  match parser.curr with
  | Some Star | Some Alternate | Some RightParen -> Ok (parser, List.rev units)
  | None -> Ok (parser, List.rev units)
  | _ ->
    let* parser, unit' = parse_unit' parser in
    collect_term (advance parser) (unit' :: units)

and collect_group parser =
  let* parser, expr = parse_expression parser in
  match parser.curr with
  | Some RightParen -> Ok (parser, Node.Group expr)
  | _ -> Error "Invalid grouping."
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

let%test "Parser can parse groups" =
  iparse "(a* + b)"
  = Expression
      [ Term
          [ Unit
              { primary =
                  Group
                    (Expression
                       [ Term [ Unit { primary = Letter 'a'; repeat = true } ]
                       ; Term [ Unit { primary = Letter 'b'; repeat = false } ]
                       ])
              ; repeat = false
              }
          ]
      ]
;;

let%test "Parser can parse groups as units" =
  iparse "(ab + a) + a"
  = Expression
      [ Term
          [ Unit
              { primary =
                  Group
                    (Expression
                       [ Term
                           [ Unit { primary = Letter 'a'; repeat = false }
                           ; Unit { primary = Letter 'b'; repeat = false }
                           ]
                       ; Term [ Unit { primary = Letter 'a'; repeat = false } ]
                       ])
              ; repeat = false
              }
          ]
      ; Term [ Unit { primary = Letter 'a'; repeat = false } ]
      ]
;;

let%test "Parser can parse groups as units and then repeat" =
  iparse "(ab)* + a"
  = Expression
      [ Term
          [ Unit
              { primary =
                  Group
                    (Expression
                       [ Term
                           [ Unit { primary = Letter 'a'; repeat = false }
                           ; Unit { primary = Letter 'b'; repeat = false }
                           ]
                       ])
              ; repeat = true
              }
          ]
      ; Term [ Unit { primary = Letter 'a'; repeat = false } ]
      ]
;;

(*}}}*)

(** Negative tests {{{*)

let%test "Parser recognizes invalid expression `*`" =
  iparse "*" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `**`" =
  iparse "**" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `a(**)`" =
  iparse "a(**)" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `a++`" =
  iparse "a++" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `()`" =
  iparse "()" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `((`" =
  iparse "((" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;

let%test "Parser recognizes invalid expression `a**`" =
  iparse "a**" = Node.Expression [ Term [ Unit { primary = Lambda; repeat = false } ] ]
;;
(*}}}*)
