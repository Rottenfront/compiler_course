(* open CompilerCourse.Parser *)
(* open CompilerCourse.Compiler *)
open CompilerCourse

let usage_msg =
  "compiler_course [-verbose] -source <source_language> -arch <arch> <source> \
   -o <output>"

let verbose = ref false
let input_file = ref ""
let output_file = ref ""
let arch = ref ""
let source_language = ref ""

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ( "-arch",
      Arg.Set_string arch,
      "Target architecture (arm64-darwin, arm64-linux)" );
    ("-source", Arg.Set_string source_language, "Source language (lama, racket)");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist (fun filename -> input_file := filename) usage_msg;
  let target =
    match !arch with
    | "arm64-darwin" -> Config.Arm64Darwin
    | "arm64-linux" -> Config.Arm64Linux
    | _ -> failwith "Unknown target architecture"
  in
  let source =
    match !source_language with
    | "lama" -> Config.Lama
    | "racket" -> Config.Racket
    | _ -> failwith "Unknown source language"
  in
  let code =
    let input = open_in !input_file in
    let code = In_channel.input_all input in
    close_in input;
    code
  in
  let tokens = Lexer.lexer source code in
  let functions =
    match
      Parser.Language.parse source Parser.Cst.default_position [] tokens
    with
    | Ok functions, _ -> functions
    | Error err, _ ->
        failwith
          (Format.sprintf "Parser failed on %s" (Parser.Cst.print_error err))
  in
  let check_result = Checker.check_program functions in
  if check_result |> List.is_empty then ()
  else
    failwith
      (Format.sprintf "Checker failed:\n%s"
         (check_result
         |> List.map (fun error -> "- " ^ Checker.print_error error)
         |> String.concat "\n"));

  let ast = Parser.Ast.cst_to_ast functions [] in
  let assembly_code = Compiler.M.compile !verbose target ast in

  let file = open_out !output_file in
  Printf.fprintf file "%s" assembly_code;
  close_out file
