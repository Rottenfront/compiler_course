open Lexer
open Utils
open Cst

let parse_in (on_first_error : char_position -> token parser)
    (parser : char_position -> ('a * position) parser)
    (cursor_pos : char_position) : ('a * position) parser =
  parse_in_parenth on_first_error parser cursor_pos
  <|> parse_in_brackets on_first_error parser cursor_pos
  <|> parse_in_braces on_first_error parser cursor_pos

let rec parse_expr (cursor_pos : char_position) : expr_node parser =
  let parse_in_expr (parser : char_position -> (expr_node * position) parser)
      (cursor_pos : char_position) : expr_node parser =
    let* expr, position =
      parse_in error_no_possible_expression parser cursor_pos
    in
    return { value = expr.value; position }
  in

  let parse_if (cursor_pos : char_position) : (expr_node * position) parser =
    let is_if = function TkIf -> true | _ -> false in

    let* if_tok =
      expect_token ~pred:is_if
        ~on_empty:(error_no_possible_expression cursor_pos)
        ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
    in

    let* condition =
      parse_required parse_expr error_condition_expected (snd if_tok.position)
    in
    let* if_true =
      parse_required parse_expr error_true_branch_expected
        (snd condition.position)
    in
    let* if_false =
      parse_required parse_expr error_false_branch_expected
        (snd if_true.position)
    in

    let position = extend_span if_tok.position if_false.position in
    return
      ({ value = TmIf { condition; if_true; if_false }; position }, position)
  in

  let parse_let (position : char_position) : (expr_node * position) parser =
    let is_let = function TkLet -> true | _ -> false in

    let parse_assignment position =
      let* name =
        expect_ident
          ~on_empty:(error_unexpected_end position)
          ~on_unexpected:error_variable_name_expected
      in
      let* value =
        parse_required parse_expr error_variable_value_expected
          (snd name.position)
      in
      let position = extend_span name.position value.position in
      return ((name, value), position)
    in
    let rec parse_assignments (acc : (substring * expr_node * position) list)
        (assignments_span : position) :
        ((substring * expr_node * position) list * position) parser =
      let* next_assignment =
        parse_nonrequired
          (parse_in error_no_possible_expression parse_assignment)
          (snd assignments_span)
      in
      match next_assignment with
      | Some ((name, value), position) ->
          parse_assignments
            ((name, value, position) :: acc)
            (extend_span assignments_span position)
      | None -> return (acc, assignments_span)
    in
    let rec construct_let (variables : (substring * expr_node * position) list)
        (expression : expr_node) (let_position : position) : expr_node =
      match variables with
      | [] -> { value = expression.value; position = let_position }
      | (name, value, position) :: rest ->
          construct_let rest
            {
              value = TmLet { name; value; expression };
              position = extend_span position expression.position;
            }
            let_position
    in

    let* let_tok =
      expect_token ~pred:is_let
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in

    let* variables, position =
      parse_in error_no_possible_expression
        (fun pos -> parse_assignments [] (pos, pos))
        (snd let_tok.position)
    in

    let* expression =
      parse_required parse_expr error_no_possible_expression (snd position)
    in
    let result = construct_let variables expression let_tok.position in
    return (result, result.position)
  in

  let parse_function (position : char_position) : (expr_node * position) parser
      =
    let rec parse_arguments (name : substring) (arguments : expr_node list)
        (cursor_pos : char_position) =
      let* argument = parse_nonrequired parse_expr cursor_pos in
      match argument with
      | Some new_arg ->
          parse_arguments name (new_arg :: arguments) (snd new_arg.position)
      | None ->
          return
            {
              value = TmApplication { name; arguments = List.rev arguments };
              position = extend_span name.position (cursor_pos, cursor_pos);
            }
    in
    let* name =
      expect_ident ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    let* result = parse_arguments name [] (snd name.position) in
    return (result, result.position)
  in

  let parse_sequence (position : char_position) : (expr_node * position) parser
      =
    let rec parse_arguments (arguments : expr_node list) (span : position) =
      let* argument = parse_nonrequired parse_expr cursor_pos in
      match argument with
      | Some new_arg ->
          parse_arguments (new_arg :: arguments)
            (extend_span span new_arg.position)
      | None ->
          return { value = TmSequence (List.rev arguments); position = span }
    in
    let is_begin = function TkBegin -> true | _ -> false in
    let* begin_tok =
      expect_token ~pred:is_begin
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    let* result =
      parse_arguments [] (snd begin_tok.position, snd begin_tok.position)
    in
    return (result, result.position)
  in

  let parse_ident (cursor_pos : char_position) : expr_node parser =
    let* name =
      expect_ident ~on_empty:(error_no_possible_expression cursor_pos)
        ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
    in
    return
      {
        value = TmApplication { name; arguments = [] };
        position = name.position;
      }
  in

  let parse_number (cursor_pos : char_position) : expr_node parser =
    let* number, position =
      expect_number ~on_empty:(error_no_possible_expression cursor_pos)
        ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
    in
    return { value = TmLiteral (LitNumber number); position }
  in

  let parse_true (cursor_pos : char_position) : expr_node parser =
    let is_true = function TkTrue -> true | _ -> false in
    let* token =
      expect_token ~pred:is_true
        ~on_empty:(error_no_possible_expression cursor_pos)
        ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
    in
    return { value = TmLiteral (LitBool true); position = token.position }
  in

  let parse_false (cursor_pos : char_position) : expr_node parser =
    let is_false = function TkFalse -> true | _ -> false in
    let* token =
      expect_token ~pred:is_false
        ~on_empty:(error_no_possible_expression cursor_pos)
        ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
    in
    return { value = TmLiteral (LitBool false); position = token.position }
  in

  let parse_basic_expr (cursor_pos : char_position) : expr_node parser =
    parse_true cursor_pos <|> parse_false cursor_pos <|> parse_number cursor_pos
    <|> parse_ident cursor_pos
  in

  parse_basic_expr cursor_pos
  <|> parse_in_expr
        (fun position ->
          parse_let position <|> parse_if position <|> parse_function position
          <|> parse_sequence position)
        cursor_pos

let rec parse_type (cursor_pos : char_position) : type_expr parser =
  let parse_basic_type (cursor_pos : char_position) : type_expr parser =
    let* { str; position = _ } =
      expect_ident ~on_empty:(error_no_possible_type cursor_pos)
        ~on_unexpected:(fun tok -> error_no_possible_type (snd tok.position))
    in
    return (TyNamed str)
  in
  let parse_parentheses (cursor_pos : char_position) : type_expr parser =
    let is_parenth_open = function TkParenOpen -> true | _ -> false in
    let is_parenth_close = function TkParenClose -> true | _ -> false in

    let* open_parenth =
      expect_token ~pred:is_parenth_open
        ~on_empty:(error_no_possible_type cursor_pos) ~on_unexpected:(fun _ ->
          error_no_possible_type cursor_pos)
    in
    let* inner = parse_nonrequired parse_type (snd open_parenth.position) in
    let inner, cursor_pos =
      match inner with
      | None -> (TyTuple [], snd open_parenth.position)
      | Some type_ -> (type_.type_value, snd type_.position)
    in
    let* close_parenth =
      expect_token ~pred:is_parenth_close
        ~on_empty:(error_unexpected_end cursor_pos)
        ~on_unexpected:error_in_expected
    in
    let position = extend_span open_parenth.position close_parenth.position in
    return { type_value = inner; position }
  in

  let parse_function_type (cursor_pos : char_position) : type_expr parser =
    let rec parse_parameters parameters span cursor_pos =
      let* parameter = parse_nonrequired parse_type cursor_pos in
      match parameter with
      | Some new_arg ->
          parse_parameters (new_arg :: parameters)
            (extend_span span new_arg.position)
            (snd new_arg.position)
      | None -> return (List.rev parameters, span)
    in

    let* parameters, position =
      parse_in_brackets error_no_possible_type
        (fun pos -> parse_parameters [] (pos, pos) pos)
        cursor_pos
    in
    let* return_type = parse_type (snd position) in
    return
      {
        type_value = TyFunc (parameters, return_type);
        position = extend_span position return_type.position;
      }
  in

  let rec parse_tuple_type (prev : type_expr list) (cursor_pos : char_position)
      : type_expr parser =
    let parse_next_type (cursor_pos : char_position) : type_expr parser =
      let is_star = function TkOperator "*" -> true | _ -> false in
      let* star =
        expect_token ~pred:is_star ~on_empty:(error_no_possible_type cursor_pos)
          ~on_unexpected:(fun _ -> error_no_possible_type cursor_pos)
      in
      let* next_type = parse_type (snd star.position) in
      return next_type
    in
    let* next_type = parse_nonrequired parse_next_type cursor_pos in
    match next_type with
    | Some next_type ->
        parse_tuple_type (next_type :: prev) (snd next_type.position)
    | None ->
        let position =
          List.fold_left
            (fun (rhs : position) (lhs : type_expr) ->
              extend_span lhs.position rhs)
            (cursor_pos, cursor_pos) prev
        in
        let types = List.rev prev in
        if List.length types = 1 then return (List.hd types)
        else return { type_value = TyTuple types; position }
  in

  let* first_type =
    parse_basic_type cursor_pos
    <|> parse_parentheses cursor_pos
    <|> parse_function_type cursor_pos
  in
  parse_tuple_type [ first_type ] (snd first_type.position)

let parse_func (cursor_pos : char_position) : (implementation * position) parser
    =
  let parse_parameter (cursor_pos : char_position) :
      ((substring * type_expr) * position) parser =
    let is_colon = function TkIdent ":" -> true | _ -> false in

    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end cursor_pos)
        ~on_unexpected:error_ident_expected
    in

    let* colon =
      expect_token ~pred:is_colon
        ~on_empty:(error_unexpected_end (snd name.position))
        ~on_unexpected:error_colon_expected
    in

    let* type_ = parse_type (snd colon.position) in

    let position = extend_span name.position type_.position in

    return ((name, type_), position)
  in

  let rec parse_parameters (cursor_pos : char_position)
      (parameters : (substring * type_expr) list) :
      (substring * type_expr) list parser =
    let* parameter =
      parse_nonrequired
        (parse_in error_no_possible_type parse_parameter)
        cursor_pos
    in
    match parameter with
    | None -> return (List.rev parameters)
    | Some ((name, type_), position) ->
        parse_parameters (snd position) ((name, type_) :: parameters)
  in

  let parse_function_data (position : position) :
      ((substring * (substring * type_expr) list * type_expr) * position) parser
      =
    let is_colon = function TkIdent ":" -> true | _ -> false in
    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end position)
        ~on_unexpected:error_function_name_expected
    in
    let* parameters = parse_parameters name.position [] in
    let last_position =
      if List.is_empty parameters then name.position
      else (List.nth parameters (List.length parameters - 1) |> snd).position
    in
    let* colon_tok =
      expect_token ~pred:is_colon
        ~on_empty:(error_unexpected_end last_position)
        ~on_unexpected:error_colon_expected
    in
    let* type_ = parse_type colon_tok.position in
    let position = extend_span name.position type_.position in
    return ((name, parameters, type_), position)
  in

  let is_define = function TkDefine -> true | _ -> false in

  let* define_tok =
    expect_token ~pred:is_define
      ~on_empty:(error_no_function position)
      ~on_unexpected:error_define_expected
  in
  let* (name, parameters, type_), position =
    parse_in error_no_possible_type parse_function_data define_tok.position
  in
  let* expression = parse_expr position in
  let expression = distribute_operator expression in
  let position = extend_span define_tok.position expression.position in
  return ({ position; name; parameters; type_; expression }, position)

let rec parse_func_statement (cursor_pos : char_position) =
 fun tokens ->
  let res, rest = parse_in error_no_function parse_func cursor_pos tokens in
  match res with
  | Ok (res, _) -> return (Some res) rest
  | Error (NoFunction _) -> return None rest
  | Error err -> fail err rest
