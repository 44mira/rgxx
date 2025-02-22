open Rgxx

(** Command-line args *)

let usage_msg =
  "rgxx -p <pattern> -t <test file>\n\nNon-matching arguments will be ignored.\n"
;;

let regex_pattern = ref ""
let test_file = ref ""
let anon_fun _ = ()

let speclist =
  [ ( "-p"
    , Arg.Set_string regex_pattern
    , "The Regular expression pattern to use for validation." )
  ; "-t", Arg.Set_string test_file, "The file to read tests line by line from."
  ]
;;

(** Parse arg specifications *)
let () = Arg.parse speclist anon_fun usage_msg

let validate_line ast idx line =
  let open Evaluator in
  let evaluator = init @@ String.trim line in
  let result = evaluate ast evaluator in
  if result then
    Spectrum.Simple.printf
      "@{<yellow,italic>TEST %d: @{<green,bold>PASS@}@} @{<dim>%s@}\n"
      idx
      line
  else
    Spectrum.Simple.printf
      "@{<bg:black, yellow,italic>TEST %d: @{<red,underline,bold>FAIL@}@} @{<dim>%s@}\n"
      idx
      line
;;

(** Main *)
let () =
  (* Check for file existence *)
  if not @@ Sys.file_exists !test_file then
    raise @@ File_does_not_exist (!test_file ^ " is not a valid file");

  (* Check for valid regex pattern *)
  let parse pattern = Parser.(parse @@ init @@ Lexer.init pattern) in
  match parse !regex_pattern with
  | Error e -> raise @@ Invalid_pattern e
  | Ok (_, ast) ->
    (* Evaluate each line of textfile with respect to the abstract syntax tree 
       from the regex pattern*)
    let inp = open_in !test_file in
    Spectrum.Simple.printf
      " @{<dim>PATTERN =@} @{<yellow,bold>%s@}\n@{<black>::==::==::==::@}\n"
      !regex_pattern;
    In_channel.input_lines inp |> List.iteri (fun i x -> validate_line ast (i + 1) x)
;;
