(*
Passes:
- Uniquify
- Monadize
- Explicate control
- Assign homes
- Generate basic instructions
- Generate assembly
*)
open Utils
open Parser.Ast

let compile (verbose : bool) (target : Config.target_architecture)
    (functions : Parser.Ast.implementation list) =
  let declarations =
    functions
    |> List.map (fun (func : Parser.Ast.implementation) -> func.name)
    |> List.append AsmGenerator.Generator.builtin_functions
  in
  let compile_function verbose (func : Parser.Ast.implementation) :
      AsmGenerator.Generator.function_data =
    if verbose then (
      print_endline "function:";
      print_endline (Parser.Ast.print_ast func));
    let { name; parameters; expression } = func in
    let params_map =
      List.append declarations parameters
      |> List.map (fun name -> (name, name))
      |> StringMap.of_list
    in
    let expr, count = expression |> Uniquify.uniquify_expr 0 params_map in
    if verbose then (
      print_endline "uniquified expression:";
      print_endline (Parser.Ast.print_ast_expr expr));
    let params_map =
      parameters |> List.map (fun param -> (param, true)) |> StringMap.of_list
    in
    let expr, count = Monadic.remove_complex_operands params_map count expr in

    if verbose then (
      print_endline "monadized expression:";
      print_endline (Monadic.print_monadic expr));
    let statements, _ = ExplicateControl.explicate_control expr None count in
    { name; parameters; statements }
  in
  let functions = List.map (compile_function verbose) functions in
  match target with
  | Config.Arm64Darwin -> AsmGenerator.Arm64Darwin.compile_code functions
  | Config.Arm64Linux -> AsmGenerator.Arm64Linux.compile_code functions
