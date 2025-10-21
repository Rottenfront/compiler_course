open Monadic
open ExplicateControl
open Utils

(* there should be parameters already *)
let rec analyze_variable_use statements variables =
  let analyze_atm atm variables =
    match atm with
    | AtmVar var -> StringMap.update var (Option.map (fun x -> x + 1)) variables
    | _ -> variables
  in
  let analyze_expression expr variables =
    match expr with
    | Atm atm -> analyze_atm atm variables
    | Function (name, atms) ->
        AtmVar name :: atms
        |> List.fold_left (fun acc atm -> analyze_atm atm acc) variables
    | Operator (_, lhs, rhs) ->
        [ lhs; rhs ]
        |> List.fold_left (fun acc atm -> analyze_atm atm acc) variables
  in
  match statements with
  | Assign (new_variable, expr) :: rest ->
      variables
      |> StringMap.add new_variable 1
      |> analyze_expression expr |> analyze_variable_use rest
  | Return expr :: rest ->
      variables |> analyze_expression expr |> analyze_variable_use rest
  | CMov (atm, _) :: rest ->
      variables |> analyze_atm atm |> analyze_variable_use rest
  | _ :: rest -> analyze_variable_use rest variables
  | [] -> variables

type reg = Reg of string | Stack of int

type assign_homes_context = {
  variables : int StringMap.t;
  register_table : string option StringMap.t;
  stack_table : int StringMap.t;
  free_stack : int list;
}

(* first 8 or 6 (based on architecture) arguments should be pushed into stack
   rest should be with indices -2, -3, -4, etc.*)
let rec assign_homes statements context (homes : reg StringMap.t) =
  let analyze_atm atm context =
    match atm with
    | AtmVar var ->
        let variables =
          StringMap.update var (Option.map (fun x -> x - 1)) context.variables
        in
        if StringMap.find var variables == 0 then
          if
            StringMap.exists
              (fun _ variable -> variable == Some var)
              context.register_table
          then
            {
              variables;
              register_table =
                StringMap.map
                  (fun variable ->
                    if variable == Some var then None else variable)
                  context.register_table;
              stack_table = context.stack_table;
              free_stack = context.free_stack;
            }
          else
            let stack_position = StringMap.find var context.stack_table in
            {
              variables;
              register_table = context.register_table;
              stack_table = StringMap.remove var context.stack_table;
              free_stack = stack_position :: context.free_stack;
            }
        else { context with variables }
    | _ -> context
  in
  let analyze_expression expr context =
    match expr with
    | Atm atm -> analyze_atm atm context
    | Function (name, atms) ->
        (if StringMap.mem name context.variables then AtmVar name :: atms
         else atms)
        |> List.fold_left (fun context atm -> analyze_atm atm context) context
    | Operator (_, lhs, rhs) ->
        [ lhs; rhs ]
        |> List.fold_left (fun context atm -> analyze_atm atm context) context
  in
  match statements with
  | [] -> homes
  | Assign (new_variable, expr) :: rest ->
      let context = analyze_expression expr context in
      let context, homes =
        if
          StringMap.for_all
            (fun _ variable -> variable != None)
            context.register_table
        then
          let index =
            if List.is_empty context.free_stack then
              StringMap.fold
                (fun _ index max_index -> max max_index (index + 1))
                context.stack_table 1
            else List.hd context.free_stack
          in
          ( {
              context with
              stack_table = StringMap.add new_variable index context.stack_table;
            },
            StringMap.add new_variable (Stack index) homes )
        else
          let reg =
            StringMap.bindings context.register_table
            |> List.filter (fun (_, var) -> var = None)
            |> List.map fst |> List.hd
          in
          ( {
              context with
              register_table =
                StringMap.update reg
                  (fun _ -> Some (Some new_variable))
                  context.register_table;
            },
            StringMap.add new_variable (Reg reg) homes )
      in
      assign_homes rest context homes
  | Return expr :: rest ->
      let context = analyze_expression expr context in
      assign_homes rest context homes
  | CMov (atm, _) :: rest ->
      let context = analyze_atm atm context in
      assign_homes rest context homes
  | _ :: rest -> assign_homes rest context homes
