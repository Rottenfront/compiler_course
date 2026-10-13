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
    | Ok functions, [] -> functions
    | Ok _, _ -> failf "Unconsumed input in %s" filename
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

let parse_program language source =
  match
    source |> Lexer.lexer language
    |> Parser.Language.parse language Parser.Cst.default_position []
  with
  | Ok functions, [] -> functions
  | Ok _, _ -> fail "Unexpected trailing tokens"
  | Error error, _ -> fail (Parser.Cst.print_error error)

let check_source source accepted () =
  let functions = parse_program Config.Lama source in
  check bool "accepted" accepted
    (List.is_empty (Checker.check_program functions))

let check_associativity () =
  let functions =
    parse_program Config.Lama "define main : int = 10 - 3 - 2"
    |> fun functions -> Parser.Ast.cst_to_ast functions []
  in
  let rec evaluate (expr : Parser.Ast.expr) =
    match expr with
    | TmLiteral (LitNumber n) -> n
    | TmOpApp { operator = OpSub; lhs; rhs } -> evaluate lhs - evaluate rhs
    | _ -> fail "Unexpected arithmetic expression"
  in
  check int "subtraction associates left" 5
    (evaluate (List.hd functions).expression)

let reject_trailing language source () =
  match
    source |> Lexer.lexer language
    |> Parser.Language.parse language Parser.Cst.default_position []
  with
  | Error _, _ -> ()
  | Ok _, _ -> fail "Accepted trailing input"

let () =
  run "Parser tests"
    [
      ("lama samples", make_tests Config.Lama ".cmp");
      ("racket samples", make_tests Config.Racket ".rkt");
      ( "semantic regressions",
        [
          test_case "integer equality" `Quick
            (check_source "define main : () = print_bool (1 == 1)" true);
          test_case "function result binding" `Quick
            (check_source
               "define id (x : int) : int = x define main : int = let x = id 1 \
                in x"
               true);
          test_case "unit literal" `Quick
            (check_source "define main : () = ()" true);
          test_case "mixed equality rejected" `Quick
            (check_source "define main : () = print_bool (1 == true)" false);
          test_case "higher order rejected" `Quick
            (check_source
               "define f (g : [int] int) : int = 1 define main : int = 0" false);
          test_case "duplicate parameters rejected" `Quick
            (check_source
               "define f (x : int) (x : int) : int = x define main : int = 0"
               false);
          test_case "invalid main rejected" `Quick
            (check_source "define main : bool = true" false);
          test_case "left associativity" `Quick check_associativity;
          test_case "Lama trailing input rejected" `Quick
            (reject_trailing Config.Lama "define main : int = 1 garbage");
          test_case "Racket trailing input rejected" `Quick
            (reject_trailing Config.Racket "(define (main : int) 1) garbage");
        ] );
    ]
