open Alcotest
open CompilerCourse

let sample_dir = Filename.concat (Sys.getcwd ()) "samples"

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_parse_file language filename () =
  let path = Filename.concat sample_dir filename in
  let content = read_file path in
  let functions =
    match
      content |> Lexer.lexer language
      |> Parser.Language.parse language Parser.Cst.default_position []
    with
    | Ok functions, _ -> functions
    | Error err, _ ->
        failf "Parser failed on %s: %s" filename (Parser.Cst.print_error err)
  in
  let check_result = Checker.check_program functions in
  if check_result |> List.is_empty then ()
  else
    failf "Checker failed on %s:\n%s" filename
      (check_result
      |> List.map (fun error -> "- " ^ Checker.print_error error)
      |> String.concat "\n")

let make_tests language suffix =
  let files =
    Sys.readdir sample_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f suffix)
  in
  List.map
    (fun file -> test_case file `Quick (test_parse_file language file))
    files

let () =
  run "Parser tests"
    [
      ("lama samples", make_tests Config.Lama ".cmp");
      ("racket samples", make_tests Config.Racket ".rkt");
    ]
