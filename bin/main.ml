(** Command-line args *)

let usage_msg =
  "rgxx -p <pattern> -t <test file>\n\nNon-matching arguments will be ignored.\n"
;;

let regex_pattern = ref ""
let test_file = ref ""
let anon_fun _ = ()

exception File_does_not_exist of string

let speclist =
  [ ( "-p"
    , Arg.Set_string regex_pattern
    , "The Regular expression pattern to use for validation." )
  ; "-t", Arg.Set_string test_file, "The file to read tests line by line from."
  ]
;;

(** Parse arg specifications *)
let () = Arg.parse speclist anon_fun usage_msg

(** Main *)
let () =
  if not @@ Sys.file_exists !test_file then
    raise @@ File_does_not_exist (!test_file ^ " is not a valid file")
  else (
    let inp = open_in !test_file in
    List.iter print_endline @@ In_channel.input_lines inp)
;;
