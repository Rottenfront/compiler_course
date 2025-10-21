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

let compile (target : Config.target_architecture)
    (functions : Parser.Ast.implementation list) =
  let declarations =
    functions
    |> List.map (fun (func : Parser.Ast.implementation) -> func.name)
    |> List.append AsmGenerator.Generator.builtin_functions
  in
  let compile_function
      ({ name; parameters; expression } : Parser.Ast.implementation) :
      AsmGenerator.Generator.function_data =
    let params_map =
      List.append declarations parameters
      |> List.map (fun name -> (name, name))
      |> StringMap.of_list
    in
    let expr, count = expression |> Uniquify.uniquify_expr 0 params_map in
    let params_map =
      parameters |> List.map (fun param -> (param, true)) |> StringMap.of_list
    in
    let expr, count = Monadic.remove_complex_operands params_map count expr in
    let statements, _ = ExplicateControl.explicate_control expr None count in
    { name; parameters; statements }
  in
  let functions = List.map compile_function functions in
  match target with
  | Config.Arm64Darwin -> AsmGenerator.Arm64Darwin.compile_code functions
  | Config.Arm64Linux -> AsmGenerator.Arm64Linux.compile_code functions
