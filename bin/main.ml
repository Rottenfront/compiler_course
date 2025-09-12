open CompilerCourse.Parser
open CompilerCourse.Compiler

let () =
  let code = read_line () in
  let statements, names =
    code |> racket_parser |> uniquify |> MonadicRacket.remove_complex_operands
    |> ExplicateControl.order
  in
  print_string (AsmGenerator.compile_format statements names)
