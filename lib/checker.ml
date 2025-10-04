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
  | DoubleDeclaration of declaration
  | DoubleImplementation of implementation
  | UnexpectedParameterCount of substring * position * int * int
  | Unimplemented of declaration
  | Undeclared of implementation

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

let rec get_type (context : declaration StringMap.t) expr =
  match expr with
  | TmLiteral (pos, lit) ->
      Some (match lit with LitNumber _ -> TyInt pos | LitBool _ -> TyBool pos)
  | TmApplication (pos, { name; arguments = _ }) ->
      if StringMap.mem name.str context then
        Some
          ((StringMap.find name.str context).type_ |> set_type_position pos
         |> get_result_type)
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

let rec check_expression (context : declaration StringMap.t) expected_type expr
    =
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
            StringMap.remove name.str context
            |> StringMap.add name.str
                 {
                   position = extend_span name.position (get_position value);
                   name;
                   type_;
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

let default_context : declaration StringMap.t =
  let add_builtin_declaration name type_ context =
    StringMap.add name
      {
        position = default_position;
        name = { str = name; position = default_position };
        type_;
      }
      context
  in
  StringMap.empty
  |> add_builtin_declaration "read" (TyInt default_position)
  |> add_builtin_declaration "print_int"
       (TyFunc
          (default_position, [ TyInt default_position ], TyUnit default_position))

let rec create_context context errors (decls : declaration list) :
    declaration StringMap.t * check_error list =
  match decls with
  | decl :: rest ->
      let context, errors =
        if StringMap.mem decl.name.str context then
          (context, DoubleDeclaration decl :: errors)
        else (StringMap.add decl.name.str decl context, errors)
      in
      create_context context errors rest
  | [] -> (context, errors)

let check_impl context impl =
  let { type_; position = _; name = _ } =
    StringMap.find impl.name.str context
  in
  let context =
    match type_ with
    | TyFunc (_, types, _) ->
        StringMap.add_seq
          (List.combine impl.parameters types
          |> List.map (fun (name, type_) ->
                 (name.str, { position = name.position; name; type_ }))
          |> List.to_seq)
          context
    | _ -> context
  in
  check_expression context (Some (get_result_type type_)) impl.expression

let rec check_functions (unimplemented : declaration StringMap.t)
    (implemented : declaration StringMap.t) errors impls =
  match impls with
  | { position; name; parameters; expression } :: rest ->
      let unimplemented, implemented, errors =
        if StringMap.mem name.str unimplemented then
          let signature = StringMap.find name.str unimplemented in
          let expected_param_count = get_type_parameter_count signature.type_ in
          let errors =
            if List.length parameters == expected_param_count then errors
            else
              UnexpectedParameterCount
                (name, position, expected_param_count, List.length parameters)
              :: errors
          in
          ( StringMap.remove name.str unimplemented,
            StringMap.add name.str signature implemented,
            errors )
        else
          let impl = { position; name; parameters; expression } in
          ( unimplemented,
            implemented,
            (if StringMap.mem name.str implemented then
               DoubleImplementation impl
             else Undeclared impl)
            :: errors )
      in
      check_functions unimplemented implemented errors rest
  | [] ->
      ( implemented,
        unimplemented
        |> StringMap.map (fun decl -> Unimplemented decl)
        |> StringMap.to_list |> List.rev
        |> List.map (fun (_, value) -> value)
        |> List.append errors )

let rec check_impls context errors impls =
  match impls with
  | [] -> errors
  | impl :: rest ->
      let errors = check_impl context impl |> List.append errors in
      check_impls context errors rest

let combined_check decls impls =
  let context = default_context in
  let decls, errors = create_context StringMap.empty [] decls in
  let decls, errors = check_functions decls StringMap.empty errors impls in
  let context = StringMap.merge (fun _ _ rhs -> rhs) context decls in
  if List.is_empty errors then check_impls context [] impls else errors
