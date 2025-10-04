open Alcotest
open CompilerCourse

let sample_dir = Filename.concat (Sys.getcwd ()) "samples"

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_parse_file filename () =
  let path = Filename.concat sample_dir filename in
  let content = read_file path in
  match content |> Lexer.lexer |> Parser.parse_stmts [] [] with
  | Ok _, _ -> ()
  | Error err, _ ->
      failf "Parser failed on %s: %s" filename (Parser.print_error err)

let () =
  let files =
    Sys.readdir sample_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".cmp")
  in
  let tests =
    List.map
      (fun file -> test_case file `Quick (test_parse_file file))
      files
  in
  run "Parser tests" [ "samples", tests ]
