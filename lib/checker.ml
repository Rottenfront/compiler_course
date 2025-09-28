open Parser
open Utils

type position = Lexer.position
type check_context = { functions : type_expr StringMap.t }

type check_error =
  | UnsupportedHighOrderFunctions of position
  | TypeMismatch of type_expr * type_expr
  | NonexistentFunction of substring
  | UnexpectedArgumentCount of position * int * int
  | NotAFunction of position * int

let get_type_parameter_count type_ =
  match type_ with
  | TyFunc (_, params, _) -> List.length params
  | _ -> failwith "unreachable"

let get_result_type type_ =
  match type_ with TyFunc (_, _, res) -> res | other -> other

let set_type_position pos type_ =
  match type_ with
  | TyFunc (_, parameters, res) -> TyFunc (pos, parameters, res)
  | TyInt _ -> TyInt pos
  | TyBool _ -> TyBool pos
  | TyUnit _ -> TyUnit pos

let rec get_type context expr =
  match expr with
  | TmLiteral (pos, lit) ->
      Some (match lit with LitNumber _ -> TyInt pos | LitBool _ -> TyBool pos)
  | TmApplication (pos, { name; arguments = _ }) ->
      if StringMap.mem name.str context.functions then
        Some (set_type_position pos (StringMap.find name.str context.functions))
      else None
  | TmOpApp (pos, { lhs = _; operator; rhs }) -> (
      match operator with
      | OpSemicolon -> get_type context rhs
      | other ->
          Some
            (match other with
            | OpAdd | OpSub | OpMul | OpDiv -> TyInt pos
            | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq | OpAnd
            | OpOr | OpXor ->
                TyBool pos
            | OpSemicolon -> failwith "unreachable"))
  | TmIf (_, _, lhs, _) -> get_type context lhs
  | TmLet (_, _, expr) -> get_type context expr
  | TmParenth expr -> get_type context expr

let rec check_expression context expected_type expr =
  let current_type = get_type context expr in
  match expr with
  | TmLiteral (_, _) -> (
      match expected_type with
      | Some lhs -> (
          match current_type with
          | Some rhs ->
              if equal_type lhs rhs then [] else [ TypeMismatch (lhs, rhs) ]
          | None -> failwith "unreachable")
      | None -> [])
  | TmApplication (pos, { name; arguments }) -> (
      let rec check_arguments context types arguments =
        match types with
        | type_ :: rest_types -> (
            match arguments with
            | expr :: rest_arguments ->
                List.append
                  (check_expression context (Some type_) expr)
                  (check_arguments context rest_types rest_arguments)
            | [] -> [])
        | [] -> []
      in
      match current_type with
      | None -> [ NonexistentFunction name ]
      | Some current -> (
          match current with
          | TyFunc (_, types, res) -> (
              let arg_errors = check_arguments context types arguments in
              let errors =
                if List.length arguments != List.length types then
                  UnexpectedArgumentCount
                    (pos, List.length types, List.length arguments)
                  :: arg_errors
                else arg_errors
              in
              match expected_type with
              | None -> errors
              | Some expected ->
                  if equal_type expected res then errors
                  else TypeMismatch (expected, res) :: errors)
          | current ->
              let errors =
                match expected_type with
                | None -> []
                | Some expected ->
                    if equal_type expected current then []
                    else [ TypeMismatch (expected, current) ]
              in
              if List.is_empty arguments then errors
              else NotAFunction (name.position, List.length arguments) :: errors
          ))
  | TmIf (pos, cond, lhs, rhs) ->
      let expected =
        match expected_type with
        | None -> current_type
        | Some expected -> Some expected
      in
      List.concat
        [
          check_expression context (Some (TyBool pos)) cond;
          check_expression context expected lhs;
          check_expression context expected rhs;
        ]
  | TmLet (_, { name; value }, expr) ->
      let value_errors = check_expression context None value in
      let value_type = get_type context value in
      let new_context =
        match value_type with
        | None -> context
        | Some type_ ->
            {
              functions =
                StringMap.remove name.str context.functions
                |> StringMap.add name.str type_;
            }
      in
      List.append value_errors (check_expression new_context expected_type expr)
  | TmOpApp (pos, { lhs; operator; rhs }) -> (
      let check_int context lhs rhs =
        let l_errors =
          check_expression context (Some (TyInt (get_position lhs))) lhs
        in
        let r_errors =
          check_expression context (Some (TyInt (get_position rhs))) rhs
        in
        List.append l_errors r_errors
      in
      let check_bool context lhs rhs =
        let l_errors =
          check_expression context (Some (TyBool (get_position lhs))) lhs
        in
        let r_errors =
          check_expression context (Some (TyBool (get_position rhs))) rhs
        in
        List.append l_errors r_errors
      in
      let match_expected expected current errors =
        match expected with
        | None -> errors
        | Some type_ ->
            if equal_type type_ current then errors
            else TypeMismatch (type_, current) :: errors
      in
      match operator with
      | OpSemicolon ->
          List.append
            (check_expression context None lhs)
            (check_expression context expected_type rhs)
      | OpAdd | OpSub | OpMul | OpDiv ->
          check_int context lhs rhs |> match_expected expected_type (TyInt pos)
      | OpEq | OpNe -> (
          match current_type with
          | Some (TyBool _) ->
              check_bool context lhs rhs
              |> match_expected expected_type (TyBool pos)
          | _ ->
              check_int context lhs rhs
              |> match_expected expected_type (TyBool pos))
      | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
          check_int context lhs rhs |> match_expected expected_type (TyBool pos)
      | OpAnd | OpOr | OpXor ->
          check_bool context lhs rhs
          |> match_expected expected_type (TyBool pos))
  | TmParenth expr -> check_expression context expected_type expr
