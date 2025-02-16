(**
  unquoted braces { }             => 0 or more occurrences
  pipe |                          => alternate
  brackets with 2 dots [ x .. y ] => range of literals from x to y, inclusive
  single-quoted symbols           => literals

  ** whitespace is ignored 

  Regex Grammar:

  <expr>      ::= <term>
              |   <alternate>

  <alternate> ::= <term> '+' <term> { '+' <term> }

  <term>      ::= <letter> { <letter> }
              |   '(' <expr> ')'
              |   <repeat>

  <repeat>    ::= <term> '*'

  <letter>    ::= ['a' .. 'z']
              |   <LAMBDA>

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
