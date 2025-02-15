type lexer =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let is_whitespace = function
  | None -> false
  | Some ch -> List.exists (fun x -> x = ch) [ ' '; '\t'; '\n' ]
;;

(** Create a lexer type from a string. Is used in conjunction with next_token *)
let init = function
  | "" -> { input = ""; position = 0; ch = None }
  | input -> { input; position = 0; ch = Some (String.get input 0) }
;;

(** Return a pair of the new lexer state and the token consumed.
    ignores whitespaces. *)
let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | '(' -> advance lexer, LeftParen
      | ')' -> advance lexer, RightParen
      | '+' -> advance lexer, Alternate
      | '*' -> advance lexer, Star
      | 'L' -> advance lexer, Lambda
      | ch -> advance lexer, Letter ch
    in
    lexer, Some token

(** Moves the given lexer forward by one position and updates the 
    current character. *)
and advance lexer =
  if lexer.position >= String.length lexer.input - 1 then
    { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })

(** Advances the lexer until it finds a non-whitespace character or hits EOL. *)
and skip_whitespace lexer =
  if is_whitespace lexer.ch then
    skip_whitespace @@ advance lexer
  else
    lexer
;;

(** Consumes a lexer, token pair to produce the next pair, discarding the 
    previous token.

    This is more or less just used for testing parsed tokens. *)
let chomp = function
  | lexer, _ -> next_token lexer
;;

(* Tests {{{*)
let%test_module "Lexer tests" =
  (module struct
    let test_lexer = init "(ab*+c)"

    let%test "Lexer can initialize a non-empty string" =
      test_lexer = { input = "(ab*+c)"; position = 0; ch = Some '(' }
    ;;

    let%test "Lexer can initialize an empty string" =
      init "" = { input = ""; position = 0; ch = None }
    ;;

    let%test "Lexer can parse a left parenthesis" =
      next_token test_lexer
      = ({ test_lexer with position = 1; ch = Some 'a' }, Some LeftParen)
    ;;

    let%test "Lexer can parse a letter" =
      chomp @@ next_token test_lexer
      = ({ test_lexer with position = 2; ch = Some 'b' }, Some (Letter 'a'))
    ;;
  end)
;;
(*}}}*)
