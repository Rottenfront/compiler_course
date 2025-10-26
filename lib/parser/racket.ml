open Lexer
open Utils
open Cst

let parse_nonrequired (parser : position -> 'a parser) (position : position) :
    'a option parser =
 fun tokens ->
  let res, rest = parser position tokens in
  match res with
  | Ok expr -> return (Some expr) rest
  | Error (NoPossibleExpression _) -> return None tokens
  | Error (NoPossibleType _) -> return None tokens
  | Error err -> fail err rest

let parse_in_parenth (on_first_error : position -> token parser)
    (parser : position -> ('a * position) parser) (position : position) :
    ('a * position) parser =
  let is_parenth_open = function TkParenOpen -> true | _ -> false in
  let is_parenth_close = function TkParenClose -> true | _ -> false in

  let* open_parenth =
    expect_token ~pred:is_parenth_open ~on_empty:(on_first_error position)
      ~on_unexpected:(fun _ -> on_first_error position)
  in
  let* inner, position = parser open_parenth.position in
  let* close_parenth =
    expect_token ~pred:is_parenth_close
      ~on_empty:(error_unexpected_end position)
      ~on_unexpected:error_paren_close_expected
  in

  let position = extend_span open_parenth.position close_parenth.position in
  return (inner, position)

let parse_in_brackets (on_first_error : position -> token parser)
    (parser : position -> ('a * position) parser) (position : position) :
    ('a * position) parser =
  let is_bracket_open = function TkBracketOpen -> true | _ -> false in
  let is_bracket_close = function TkBracketClose -> true | _ -> false in

  let* open_bracket =
    expect_token ~pred:is_bracket_open ~on_empty:(on_first_error position)
      ~on_unexpected:(fun _ -> on_first_error position)
  in
  let* inner, position = parser open_bracket.position in
  let* close_bracket =
    expect_token ~pred:is_bracket_close
      ~on_empty:(error_unexpected_end position)
      ~on_unexpected:error_bracket_close_expected
  in

  let position = extend_span open_bracket.position close_bracket.position in
  return (inner, position)

let parse_in_braces (on_first_error : position -> token parser)
    (parser : position -> ('a * position) parser) (position : position) :
    ('a * position) parser =
  let is_brace_open = function TkBraceOpen -> true | _ -> false in
  let is_brace_close = function TkBraceClose -> true | _ -> false in

  let* open_bracket =
    expect_token ~pred:is_brace_open ~on_empty:(on_first_error position)
      ~on_unexpected:(fun _ -> on_first_error position)
  in
  let* inner, position = parser open_bracket.position in
  let* close_bracket =
    expect_token ~pred:is_brace_close
      ~on_empty:(error_unexpected_end position)
      ~on_unexpected:error_brace_close_expected
  in

  let position = extend_span open_bracket.position close_bracket.position in
  return (inner, position)

let parse_in (on_first_error : position -> token parser)
    (parser : position -> ('a * position) parser) (position : position) :
    ('a * position) parser =
  parse_in_parenth on_first_error parser position
  <|> parse_in_brackets on_first_error parser position
  <|> parse_in_braces on_first_error parser position

let rec parse_expr (position : position) : expr_node parser =
  let parse_in_expr (parser : position -> (expr_node * position) parser)
      (position : position) : expr_node parser =
    let* expr, position =
      parse_in error_no_possible_expression parser position
    in
    return { value = expr.value; position }
  in

  let parse_if (position : position) : (expr_node * position) parser =
    let is_if = function TkIf -> true | _ -> false in

    let* if_tok =
      expect_token ~pred:is_if ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in

    let* condition = parse_expr if_tok.position in

    let* if_true = parse_expr condition.position in

    let* if_false = parse_expr if_true.position in
    let position = extend_span if_tok.position if_false.position in
    return
      ({ value = TmIf { condition; if_true; if_false }; position }, position)
  in

  let parse_let (position : position) : (expr_node * position) parser =
    let is_let = function TkLet -> true | _ -> false in

    let parse_assignment position =
      let* name =
        expect_ident
          ~on_empty:(error_unexpected_end position)
          ~on_unexpected:error_ident_expected
      in
      let* value = parse_expr name.position in
      let position = extend_span name.position value.position in
      return ((name, value), position)
    in
    let rec parse_assignments (acc : (substring * expr_node * position) list)
        (last_position : position) :
        ((substring * expr_node * position) list * position) parser =
      let* next_assignment =
        parse_nonrequired
          (parse_in error_no_possible_expression parse_assignment)
          last_position
      in
      match next_assignment with
      | Some ((name, value), position) ->
          parse_assignments ((name, value, position) :: acc) position
      | None -> return (acc, last_position)
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
      parse_in error_no_possible_expression (parse_assignments [])
        let_tok.position
    in

    let* expression = parse_expr position in
    let result = construct_let variables expression let_tok.position in
    return (result, result.position)
  in

  let parse_function (position : position) : (expr_node * position) parser =
    let rec parse_arguments (name : substring) (arguments : expr_node list)
        (last_position : position) =
      let* argument = parse_nonrequired parse_expr last_position in
      match argument with
      | Some new_arg ->
          parse_arguments name (new_arg :: arguments) new_arg.position
      | None ->
          return
            {
              value = TmApplication { name; arguments = List.rev arguments };
              position = extend_span name.position last_position;
            }
    in
    let* name =
      expect_ident ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    let* result = parse_arguments name [] name.position in
    return (result, result.position)
  in

  let parse_ident (position : position) : expr_node parser =
    let* name =
      expect_ident ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    return
      {
        value = TmApplication { name; arguments = [] };
        position = name.position;
      }
  in

  let parse_number (position : position) : expr_node parser =
    let* number, position =
      expect_number ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    return { value = TmLiteral (LitNumber number); position }
  in

  let parse_true (position : position) : expr_node parser =
    let is_true = function TkTrue -> true | _ -> false in
    let* token =
      expect_token ~pred:is_true
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    return { value = TmLiteral (LitBool true); position = token.position }
  in

  let parse_false (position : position) : expr_node parser =
    let is_false = function TkFalse -> true | _ -> false in
    let* token =
      expect_token ~pred:is_false
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    return { value = TmLiteral (LitBool false); position = token.position }
  in

  let parse_basic_expr (position : position) : expr_node parser =
    parse_true position <|> parse_false position <|> parse_number position
    <|> parse_ident position
  in

  parse_basic_expr position
  <|> parse_in_expr
        (fun position ->
          parse_let position <|> parse_if position <|> parse_function position)
        position

let rec distribute_operator (expression : expr_node) : expr_node =
  let rec distribute_with_operator (operator : operator) (lhs : expr_node)
      (rhs : expr_node list) : expr_node =
    match rhs with
    | rhs :: rest ->
        let position = extend_span lhs.position rhs.position in
        let lhs = { value = TmOpApp { operator; lhs; rhs }; position } in
        distribute_with_operator operator lhs rest
    | [] -> lhs
  in
  let position = expression.position in
  match expression.value with
  | TmApplication { name; arguments } -> (
      let operator =
        match name.str with
        | "+" -> Some OpAdd
        | "-" -> Some OpSub
        | "*" -> Some OpMul
        | "/" -> Some OpDiv
        | "==" -> Some OpEq
        | "!=" -> Some OpNe
        | "<" -> Some OpLess
        | ">" -> Some OpGreater
        | "<=" -> Some OpLessEq
        | ">=" -> Some OpGreaterEq
        | "||" | "or" -> Some OpOr
        | "&&" | "and" -> Some OpAnd
        | "^" | "xor" -> Some OpXor
        | "begin" -> Some OpSemicolon
        | _ -> None
      in
      match operator with
      | None ->
          {
            value =
              TmApplication
                { name; arguments = List.map distribute_operator arguments };
            position;
          }
      | Some operator ->
          let operator = { type_ = operator; position = name.position } in
          let result =
            distribute_with_operator operator (List.hd arguments)
              (List.tl arguments)
          in
          distribute_operator result)
  | TmOpApp { operator; lhs; rhs } ->
      {
        value =
          TmOpApp
            {
              operator;
              lhs = distribute_operator lhs;
              rhs = distribute_operator rhs;
            };
        position;
      }
  | TmLet { name; value; expression } ->
      {
        value =
          TmLet
            {
              name;
              value = distribute_operator value;
              expression = distribute_operator expression;
            };
        position;
      }
  | TmIf { condition; if_true; if_false } ->
      {
        value =
          TmIf
            {
              condition = distribute_operator condition;
              if_true = distribute_operator if_true;
              if_false = distribute_operator if_false;
            };
        position;
      }
  | _ -> expression

let rec parse_type (position : position) : type_expr parser =
  let parse_basic_type (position : position) : type_expr parser =
    let* type_name =
     fun tokens ->
      match tokens with
      | { type_ = TkIdent str; position } :: rest -> (Ok { str; position }, rest)
      | rest -> error_no_possible_type position rest
    in
    let* type_ =
     fun tokens ->
      match
        match type_name.str with
        | "int" -> Ok TyInt
        | "bool" -> Ok TyBool
        | other ->
            Error (UnknownType { str = other; position = type_name.position })
      with
      | Ok type_ ->
          (Ok { type_value = type_; position = type_name.position }, tokens)
      | Error e -> (Error e, tokens)
    in
    return type_
  in
  let parse_parentheses (position : position) : type_expr parser =
    let is_parenth_open = function TkParenOpen -> true | _ -> false in
    let is_parenth_close = function TkParenClose -> true | _ -> false in

    let* open_parenth =
      expect_token ~pred:is_parenth_open
        ~on_empty:(error_no_possible_type position) ~on_unexpected:(fun _ ->
          error_no_possible_type position)
    in
    let* close_parenth =
      expect_token ~pred:is_parenth_close
        ~on_empty:(error_unexpected_end open_parenth.position)
        ~on_unexpected:error_in_expected
    in
    let position = extend_span open_parenth.position close_parenth.position in
    return { type_value = TyUnit; position }
  in

  let parse_function_type (position : position) : type_expr parser =
    let rec parse_parameters parameters last_position =
      let* parameter = parse_nonrequired parse_type last_position in
      match parameter with
      | Some new_arg ->
          parse_parameters (new_arg :: parameters) new_arg.position
      | None -> return (List.rev parameters, last_position)
    in

    let* parameters, position =
      parse_in error_no_possible_type (parse_parameters []) position
    in
    let* return_type = parse_type position in
    return
      {
        type_value = TyFunc (parameters, return_type);
        position = extend_span position return_type.position;
      }
  in
  parse_basic_type position <|> parse_parentheses position
  <|> parse_function_type position

let parse_func (position : position) : (implementation * position) parser =
  let parse_parameter (position : position) :
      ((substring * type_expr) * position) parser =
    let is_colon = function TkIdent ":" -> true | _ -> false in

    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end position)
        ~on_unexpected:error_ident_expected
    in

    let* colon =
      expect_token ~pred:is_colon
        ~on_empty:(error_unexpected_end name.position)
        ~on_unexpected:error_colon_expected
    in

    let* type_ = parse_type colon.position in

    let position = extend_span name.position type_.position in

    return ((name, type_), position)
  in

  let rec parse_parameters (position : position)
      (parameters : (substring * type_expr) list) :
      (substring * type_expr) list parser =
    let* parameter =
      parse_nonrequired
        (parse_in error_no_possible_type parse_parameter)
        position
    in
    match parameter with
    | None -> return (List.rev parameters)
    | Some ((name, type_), position) ->
        parse_parameters position ((name, type_) :: parameters)
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

let rec parse_stmts position funcs =
 fun tokens ->
  let res, rest = parse_in error_no_function parse_func position tokens in
  match res with
  | Ok (res, position) -> parse_stmts position (res :: funcs) rest
  | Error (NoFunction _) -> return funcs rest
  | Error err -> fail err rest
