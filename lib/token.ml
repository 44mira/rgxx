(**
  unquoted braces { }             => 0 or more occurrences
  pipe |                          => alternate
  brackets with 2 dots [ x .. y ] => range of literals from x to y, inclusive
  brackets [ ]                    => 0 or 1 occurrence
  single-quoted symbols           => literals

  ** whitespace is ignored 

  Regex Grammar:

  <expr>      ::= <term> { '+' <term> }
  <term>      ::= <unit> { <unit> }

  <unit>      ::= <primary> [ '*' ]

  <primary>   ::= [a .. z]
              |   <LAMBDA>
              |   '(' <expr> ')'

  <LAMBDA>    ::= 'L'
 *)

type t =
  | Letter of char
  | LeftParen
  | RightParen
  | Alternate
  | Star
  | Lambda
[@@deriving show]
