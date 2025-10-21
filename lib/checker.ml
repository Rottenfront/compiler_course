open Parser.Cst
open Utils

type check_context = { functions : type_value StringMap.t }

type check_error =
  | UnsupportedHighOrderFunctions of position
  | TypeMismatch of type_expr * type_expr
  | DoubleDeclaration of substring
  | NonexistentFunction of substring
  | NotAFunction of substring * int
  | UnexpectedArgumentCount of substring * int * int

let print_error error =
  match error with
  | UnsupportedHighOrderFunctions pos ->
      Format.sprintf
        "High-order functions are not yet supported, position of type: %s"
        (print_position pos)
  | TypeMismatch (type1, type2) ->
      Format.sprintf
        "Mismatched types, expected: %s (source: %s), got: %s (source: %s)"
        (print_type type1.type_value)
        (print_position type1.position)
        (print_type type2.type_value)
        (print_position type2.position)
  | DoubleDeclaration name ->
      Format.sprintf "Double declaration of function '%s' on %s" name.str
        (print_position name.position)
  | NonexistentFunction name ->
      Format.sprintf "No function with name '%s' found, usage: %s" name.str
        (print_position name.position)
  | NotAFunction (name, args_count) ->
      Format.sprintf
        "'%s' (%s) is not a function, but %d arguments were provided" name.str
        (print_position name.position)
        args_count
  | UnexpectedArgumentCount (name, expected, provided) ->
      Format.sprintf
        "Function '%s' (%s) has %d parameters, but %d arguments were provided"
        name.str
        (print_position name.position)
        expected provided

let get_type_parameter_count type_ =
  match type_.type_value with
  | TyFunc (parameters, _) -> List.length parameters
  | _ -> failwith "unreachable"

let get_result_type type_ =
  match type_ with TyFunc (_, res) -> res.type_value | _ -> type_

let rec get_type (context : check_context) (expr : expr_node) : type_expr option
    =
  let position = expr.position in
  (match expr.value with
  | TmLiteral lit ->
      Some
        {
          type_value =
            (match lit with LitNumber _ -> TyInt | LitBool _ -> TyBool);
          position;
        }
  | TmApplication { name; arguments = _ } ->
      if StringMap.mem name.str context.functions then
        Some
          { type_value = StringMap.find name.str context.functions; position }
      else None
  | TmOpApp { lhs = _; operator; rhs } -> (
      match operator.type_ with
      | OpSemicolon -> get_type context rhs
      | other ->
          Some
            (match other with
            | OpAdd | OpSub | OpMul | OpDiv -> { type_value = TyInt; position }
            | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq | OpAnd
            | OpOr | OpXor ->
                { type_value = TyBool; position }
            | OpSemicolon -> failwith "unreachable"))
  | TmIf { condition = _; if_true; if_false = _ } -> get_type context if_true
  | TmLet { name = _; value = _; expression } -> get_type context expression
  | TmParenth expr -> get_type context expr)
  |> Option.map (fun { type_value; position = _ } ->
         { type_value; position = expr.position })

let rec check_expression (context : check_context) expected_type exp =
  let current_type = get_type context exp in
  match exp.value with
  | TmLiteral _ -> (
      match expected_type with
      | Some lhs -> (
          match current_type with
          | Some rhs ->
              if equal_type lhs rhs then [] else [ TypeMismatch (lhs, rhs) ]
          | None -> failwith "unreachable")
      | None -> [])
  | TmApplication { name; arguments } -> (
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
          match current.type_value with
          | TyFunc (types, res) -> (
              let arg_errors = check_arguments context types arguments in
              let errors =
                if List.length arguments != List.length types then
                  UnexpectedArgumentCount
                    (name, List.length types, List.length arguments)
                  :: arg_errors
                else arg_errors
              in
              match expected_type with
              | None -> errors
              | Some expected ->
                  if equal_type expected res then errors
                  else TypeMismatch (expected, res) :: errors)
          | _ ->
              let errors =
                match expected_type with
                | None -> []
                | Some expected ->
                    if equal_type expected current then []
                    else [ TypeMismatch (expected, current) ]
              in
              if List.is_empty arguments then errors
              else NotAFunction (name, List.length arguments) :: errors))
  | TmIf { condition; if_true; if_false } ->
      let expected =
        match expected_type with
        | None -> current_type
        | Some expected -> Some expected
      in
      List.concat
        [
          check_expression context
            (Some { type_value = TyBool; position = condition.position })
            condition;
          check_expression context expected if_true;
          check_expression context expected if_false;
        ]
  | TmLet { name; value; expression } ->
      let value_errors = check_expression context None value in
      let value_type = get_type context value in
      let new_context =
        match value_type with
        | None -> context
        | Some type_ ->
            {
              functions =
                StringMap.remove name.str context.functions
                |> StringMap.add name.str type_.type_value;
            }
      in
      List.append value_errors
        (check_expression new_context expected_type expression)
  | TmOpApp { lhs; operator; rhs } -> (
      let check_int context (lhs : expr_node) (rhs : expr_node) =
        let l_errors =
          check_expression context
            (Some { type_value = TyInt; position = lhs.position })
            lhs
        in
        let r_errors =
          check_expression context
            (Some { type_value = TyInt; position = rhs.position })
            rhs
        in
        List.append l_errors r_errors
      in
      let check_bool context (lhs : expr_node) (rhs : expr_node) =
        let l_errors =
          check_expression context
            (Some { type_value = TyBool; position = lhs.position })
            lhs
        in
        let r_errors =
          check_expression context
            (Some { type_value = TyBool; position = rhs.position })
            rhs
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
      match operator.type_ with
      | OpSemicolon ->
          List.append
            (check_expression context None lhs)
            (check_expression context expected_type rhs)
      | OpAdd | OpSub | OpMul | OpDiv ->
          check_int context lhs rhs
          |> match_expected expected_type
               { type_value = TyInt; position = exp.position }
      | OpEq | OpNe -> (
          match current_type with
          | Some { type_value = TyBool; position = _ } ->
              check_bool context lhs rhs
              |> match_expected expected_type
                   { type_value = TyBool; position = exp.position }
          | _ ->
              check_int context lhs rhs
              |> match_expected expected_type
                   { type_value = TyBool; position = exp.position })
      | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
          check_int context lhs rhs
          |> match_expected expected_type
               { type_value = TyBool; position = exp.position }
      | OpAnd | OpOr | OpXor ->
          check_bool context lhs rhs
          |> match_expected expected_type
               { type_value = TyBool; position = exp.position })
  | TmParenth expr -> check_expression context expected_type expr

let default_context : check_context =
  let add_builtin_declaration name type_ context =
    StringMap.add name type_ context
  in
  {
    functions =
      StringMap.empty
      |> add_builtin_declaration "read" TyInt
      |> add_builtin_declaration "print_int"
           (TyFunc
              ( [ { type_value = TyInt; position = default_position } ],
                { type_value = TyUnit; position = default_position } ))
      |> add_builtin_declaration "print_bool"
           (TyFunc
              ( [ { type_value = TyBool; position = default_position } ],
                { type_value = TyUnit; position = default_position } ));
  }

let decl_type decl =
  let parameters = List.map (fun (_, type_) -> type_) decl.parameters in
  TyFunc (parameters, decl.type_)

let rec create_context (context : check_context) errors
    (decls : implementation list) : check_context * check_error list =
  match decls with
  | decl :: rest ->
      let context, errors =
        if StringMap.mem decl.name.str context.functions then
          (context, DoubleDeclaration decl.name :: errors)
        else
          ( {
              functions =
                StringMap.add decl.name.str (decl_type decl) context.functions;
            },
            errors )
      in
      create_context context errors rest
  | [] -> (context, errors)

let check_impl (context : check_context) impl =
  let context =
    {
      functions =
        StringMap.add_seq
          (impl.parameters
          |> List.map (fun (name, type_) -> (name.str, type_.type_value))
          |> List.to_seq)
          context.functions;
    }
  in
  check_expression context (Some impl.type_) impl.expression

let rec check_impls context errors impls =
  match impls with
  | [] -> errors
  | impl :: rest ->
      let errors = check_impl context impl |> List.append errors in
      check_impls context errors rest

let check_program impls =
  let context, errors = create_context default_context [] impls in
  check_impls context errors impls
