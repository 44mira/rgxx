type t =
  { str : string (* the input string *)
  ; pos : int (* current position of evaluator *)
  ; ch : char option (* current letter at the current position *)
  }

let init = function
  | "" -> { str = ""; pos = 0; ch = None }
  | str -> { str; pos = 0; ch = Some (String.get str 0) }
;;

let rec evaluate ast evaluator =
  match evaluate_expression ast evaluator with
  | Some { ch = None; _ } -> true
  | _ -> false

and evaluate_expression ast (evaluator : t) : t option =
  let open Node in
  let open List in
  let (Expression expr) = ast in

  map (fun term -> evaluate_term term evaluator) expr
  |> find_opt Option.is_some (* Find first valid branch *)
  |> Option.join (* collapse `Some (Some x)` into `Some x` *)

and evaluate_term term evaluator =
  match term with
  | Term [] -> None (* For completeness, term list should never be empty *)
  | Term (h :: t) ->
    let f acc unit' =
      match acc with
      | None -> None
      | Some evaluator -> evaluate_unit unit' evaluator
    in
    List.fold_left f (evaluate_unit h evaluator) t

and evaluate_unit unit' evaluator =
  let rec evaluate_repeat p evaluator =
    match evaluate_primary p evaluator with
    | None -> Some evaluator
    | Some e -> evaluate_repeat p e
  in

  let (Unit { primary = p; repeat = r }) = unit' in
  if r then evaluate_repeat p evaluator else evaluate_primary p evaluator

and evaluate_primary primary evaluator =
  match primary with
  | Group expr -> evaluate_expression expr evaluator
  | Lambda -> advance 'L' evaluator
  | Letter a -> advance a evaluator

and advance letter evaluator =
  let pos = evaluator.pos + 1 in
  if evaluator.ch = Some letter then
    if pos >= String.length evaluator.str then
      Some { evaluator with pos; ch = None }
    else
      Some { evaluator with pos; ch = Some (String.get evaluator.str pos) }
  else if letter = 'L' then
    Some evaluator
  else
    None
;;

(* Tests {{{*)
let make_pattern str =
  str
  |> Lexer.init
  |> Parser.init
  |> Parser.parse
  |> Result.get_ok (* Unwrap Ok values, raise on Error *)
  |> fun (_, b) -> b
;;

let eval str test = evaluate (make_pattern str) @@ init test

let%test_module "Evaluator tests" =
  (module struct
    let%test "Evaluator can match on lambda" = eval "L" "L"

    let%test "Evaluator can match on a letter" =
      let p = "a" in
      eval p "a" && (not @@ eval p "b")
    ;;

    let%test "Evaluator can match on concatenated letters" =
      let p = "abb" in
      eval p "abb" && (not @@ eval p "a")
    ;;

    let%test "Evaluator can match on alternated letters" =
      let p = "a+b" in
      eval p "a" && eval p "b"
    ;;

    let%test "Evaluator can match on repeated letters" =
      let open List in
      let p = "ab*" in
      (for_all Fun.id @@ map (eval p) [ "abbb"; "abb"; "ab"; "a" ])
      && (not @@ eval p "abba")
    ;;

    let%test "Evaluator can match on repeated letters 2" =
      let p = "ab*a" in
      eval p "abbba" && eval p "aba" && (not @@ eval p "abab")
    ;;

    let%test "Evaluator can match alternated groups" =
      let p = "(ab) + a" in
      eval p "ab" && eval p "a" && (not @@ eval p "b") && (not @@ eval p "aa")
    ;;

    let%test "Evaluator can match repeating groups" =
      let open List in
      let p = "(ab)*" in
      (for_all Fun.id @@ map (eval p) [ "ababab"; "abab"; "ab"; "" ])
      && (not @@ eval p "aba")
    ;;

    let%test "Evaluator can match repeating groups 2" =
      let p = "a(a+b)*a" in
      eval p "abbaba" && eval p "aaaa" && eval p "abba" && (not @@ eval p "bab")
    ;;

    let%test "Evaluator can match repeating groups non-greedily" =
      let p = "aa*a" in
      eval p "aaaa" && eval p "aa" && eval p "aaa"
    ;;

    let%test "Evaluator can match + lambda pattern" =
      let p = "(a + L)" in
      eval p "a" && eval p "L" && (not @@ eval p "b") && eval p ""
    ;;

    let%test "Evaluator can match + lambda pattern 2" =
      let p = "(a + L)(a + b)" in
      eval p "ab" && eval p "b" && eval p "a"
    ;;
  end)
;;
(*}}}*)
