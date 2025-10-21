open Lexer
open Utils
open Cst

let rec parse_expr (in_function : bool) (position : position) : expr_node parser
    =
  let parse_required_expr (position : position) : expr_node parser =
   fun tokens ->
    let res, rest = parse_expr false position tokens in
    match res with
    | Ok expr -> return expr rest
    | Error (NoPossibleExpression _) -> fail (UnexpectedEnd position) rest
    | Error err -> fail err rest
  in

  let parse_if (position : position) : expr_node parser =
    let is_if = function TkIf -> true | _ -> false in
    let is_then = function TkThen -> true | _ -> false in
    let is_else = function TkElse -> true | _ -> false in

    let* if_tok =
      expect_token ~pred:is_if ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in

    let* condition = parse_required_expr if_tok.position in

    let* then_tok =
      expect_token ~pred:is_then
        ~on_empty:(error_unexpected_end condition.position)
        ~on_unexpected:error_then_expected
    in

    let* if_true = parse_required_expr then_tok.position in

    let* else_tok =
      expect_token ~pred:is_else
        ~on_empty:(error_unexpected_end if_true.position)
        ~on_unexpected:error_else_expected
    in

    let* if_false = parse_required_expr else_tok.position in
    let position = extend_span if_tok.position if_false.position in
    return { value = TmIf { condition; if_true; if_false }; position }
  in

  let parse_let (position : position) : expr_node parser =
    let is_let = function TkLet -> true | _ -> false in
    let is_set = function TkOperator "=" -> true | _ -> false in
    let is_in = function TkIn -> true | _ -> false in

    let* let_tok =
      expect_token ~pred:is_let
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in

    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end let_tok.position)
        ~on_unexpected:error_ident_expected
    in

    let* set_tok =
      expect_token ~pred:is_set
        ~on_empty:(error_unexpected_end name.position)
        ~on_unexpected:error_set_expected
    in

    let* value = parse_required_expr set_tok.position in

    let* in_tok =
      expect_token ~pred:is_in
        ~on_empty:(error_unexpected_end value.position)
        ~on_unexpected:error_in_expected
    in
    let* expression = parse_required_expr in_tok.position in
    let position = extend_span let_tok.position expression.position in
    return { value = TmLet { name; value; expression }; position }
  in

  let parse_parenth (position : position) : expr_node parser =
    let is_parenth_open = function TkParenOpen -> true | _ -> false in
    let is_parenth_close = function TkParenClose -> true | _ -> false in

    let* open_parenth =
      expect_token ~pred:is_parenth_open
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    let* inner = parse_required_expr open_parenth.position in
    let* close_parenth =
      expect_token ~pred:is_parenth_close
        ~on_empty:(error_unexpected_end inner.position)
        ~on_unexpected:error_in_expected
    in

    let position = extend_span open_parenth.position close_parenth.position in
    return { value = TmParenth inner; position }
  in

  let parse_function (position : position) : expr_node parser =
    let parse_nonrequired_expr (position : position) : expr_node option parser =
     fun tokens ->
      let res, rest = parse_expr true position tokens in
      match res with
      | Ok expr -> return (Some expr) rest
      | Error (NoPossibleExpression _) -> return None tokens
      | Error err -> fail err rest
    in

    let rec parse_arguments (name : substring) (arguments : expr_node list)
        (last_position : position) =
      let* argument = parse_nonrequired_expr last_position in
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
    parse_arguments name [] name.position
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

  let parse_literal (position : position) : expr_node parser =
    parse_true position <|> parse_false position <|> parse_number position
  in

  let parse_operator (lhs : expr_node) : expr_node parser =
    let* operator =
     fun tokens ->
      match tokens with
      | { type_ = TkOperator str; position } :: rest ->
          (Ok { str; position }, rest)
      | _ -> error_no_possible_expression lhs.position tokens
    in
    let* operator =
     fun tokens ->
      match
        match operator.str with
        | "+" -> Ok OpAdd
        | "-" -> Ok OpSub
        | "*" -> Ok OpMul
        | "/" -> Ok OpDiv
        | "==" -> Ok OpEq
        | "!=" -> Ok OpNe
        | "<" -> Ok OpLess
        | ">" -> Ok OpGreater
        | "<=" -> Ok OpLessEq
        | ">=" -> Ok OpGreaterEq
        | "||" | "or" -> Ok OpOr
        | "&&" | "and" -> Ok OpAnd
        | "^" | "xor" -> Ok OpXor
        | ";" -> Ok OpSemicolon
        | other ->
            Error
              (UnknownOperator { str = other; position = operator.position })
      with
      | Ok operator_type ->
          (Ok { type_ = operator_type; position = operator.position }, tokens)
      | Error e -> (Error e, tokens)
    in
    let* rhs = parse_required_expr operator.position in
    let position = extend_span lhs.position rhs.position in
    return { value = TmOpApp { operator; lhs; rhs }; position }
  in

  let* lhs =
    parse_parenth position <|> parse_literal position
    <|>
    if in_function then parse_ident position
    else parse_let position <|> parse_if position <|> parse_function position
  in
  if in_function then return lhs else parse_operator lhs <|> return lhs

let operator_level operator =
  match operator with
  | OpAdd -> 2
  | OpSub -> 2
  | OpMul -> 1
  | OpDiv -> 1
  | OpEq -> 4
  | OpNe -> 4
  | OpLess -> 3
  | OpGreater -> 3
  | OpLessEq -> 3
  | OpGreaterEq -> 3
  | OpAnd -> 5
  | OpOr -> 6
  | OpXor -> 7
  | OpSemicolon -> 10

let max_level = 10

let rec flatten_op_app expr =
  match expr.value with
  | TmOpApp { lhs; operator; rhs } ->
      let lhs_first, lhs_next = flatten_op_app lhs in
      let rhs_first, rhs_next = flatten_op_app rhs in
      (lhs_first, List.append lhs_next ((operator, rhs_first) :: rhs_next))
  | _ -> (expr, [])

let rec distribute_operator (expr : expr_node) =
  let rec distribute_level (lhs : expr_node) rhs level =
    if level = 0 then lhs
      (* there is no way for rhs to have any more elements *)
    else
      let rec split_by_level lhs prev_rhs
          (next_rhs : (operator * expr_node) list) level =
        match next_rhs with
        | [] -> distribute_level lhs (List.rev prev_rhs) (level - 1)
        | (operator, expr) :: rest ->
            if operator_level operator.type_ = level then
              let l = distribute_level lhs (List.rev prev_rhs) (level - 1) in
              let r = split_by_level expr [] rest level in
              let position = extend_span l.position r.position in
              { value = TmOpApp { lhs = l; operator; rhs = r }; position }
            else split_by_level lhs ((operator, expr) :: prev_rhs) rest level
      in
      split_by_level lhs [] rhs level
  in
  let value =
    match expr.value with
    | TmOpApp _ ->
        let lhs, rhs = flatten_op_app expr in
        let lhs = distribute_operator lhs in
        let rhs =
          List.map
            (fun (operator, expr) -> (operator, distribute_operator expr))
            rhs
        in
        (distribute_level lhs rhs max_level).value
    | TmApplication { name; arguments } ->
        TmApplication
          { name; arguments = List.map distribute_operator arguments }
    | TmIf { condition; if_true; if_false } ->
        let condition = distribute_operator condition in
        let if_true = distribute_operator if_true in
        let if_false = distribute_operator if_false in
        TmIf { condition; if_true; if_false }
    | TmLet { name; value; expression } ->
        TmLet
          {
            name;
            value = distribute_operator value;
            expression = distribute_operator expression;
          }
    | TmParenth expr -> (distribute_operator expr).value
    | other -> other
  in
  { value; position = expr.position }

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
            Error
              (UnknownOperator { str = other; position = type_name.position })
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
    let parse_nonrequired_type (position : position) : type_expr option parser =
     fun tokens ->
      let res, rest = parse_type position tokens in
      match res with
      | Ok expr -> return (Some expr) rest
      | Error (NoPossibleExpression _) -> return None tokens
      | Error err -> fail err rest
    in

    let rec parse_parameters parameters last_position =
      let* parameter = parse_nonrequired_type last_position in
      match parameter with
      | Some new_arg ->
          parse_parameters (new_arg :: parameters) new_arg.position
      | None -> return (List.rev parameters, last_position)
    in
    let is_bracket_open = function TkBracketOpen -> true | _ -> false in
    let is_bracket_close = function TkBracketClose -> true | _ -> false in

    let* open_bracket =
      expect_token ~pred:is_bracket_open
        ~on_empty:(error_no_possible_type position) ~on_unexpected:(fun _ ->
          error_no_possible_type position)
    in
    let* parameters, last_position =
      parse_parameters [] open_bracket.position
    in
    let* close_bracket =
      expect_token ~pred:is_bracket_close
        ~on_empty:(error_unexpected_end last_position)
        ~on_unexpected:error_in_expected
    in
    let* return_type = parse_type close_bracket.position in
    return
      {
        type_value = TyFunc (parameters, return_type);
        position = extend_span open_bracket.position return_type.position;
      }
  in
  parse_basic_type position <|> parse_parentheses position
  <|> parse_function_type position

let parse_func (position : position) : implementation parser =
  let parse_parameter (position : position) :
      (substring * type_expr * position) parser =
    let is_colon = function TkOperator ":" -> true | _ -> false in
    let is_parenth_open = function TkParenOpen -> true | _ -> false in
    let is_parenth_close = function TkParenClose -> true | _ -> false in

    let* open_parenth =
      expect_token ~pred:is_parenth_open
        ~on_empty:(error_no_possible_expression position)
        ~on_unexpected:(fun _ -> error_no_possible_expression position)
    in
    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end open_parenth.position)
        ~on_unexpected:error_ident_expected
    in

    let* colon =
      expect_token ~pred:is_colon
        ~on_empty:(error_unexpected_end name.position)
        ~on_unexpected:error_colon_expected
    in
    let* type_ = parse_type colon.position in

    let* close_parenth =
      expect_token ~pred:is_parenth_close
        ~on_empty:(error_unexpected_end type_.position)
        ~on_unexpected:error_in_expected
    in

    let position = extend_span open_parenth.position close_parenth.position in

    return (name, type_, position)
  in

  let rec parse_parameters (position : position)
      (parameters : (substring * type_expr) list) :
      (substring * type_expr) list parser =
    let* parameter =
     fun tokens ->
      let res, rest = parse_parameter position tokens in
      match res with
      | Ok param -> return (Some param) rest
      | Error (NoPossibleExpression _) -> return None tokens
      | Error err -> fail err rest
    in
    match parameter with
    | None -> return (List.rev parameters)
    | Some (name, type_, position) ->
        parse_parameters position ((name, type_) :: parameters)
  in

  let is_func = function TkFunc -> true | _ -> false in
  let is_set = function TkOperator "=" -> true | _ -> false in
  let is_colon = function TkOperator ":" -> true | _ -> false in
  let* func_tok =
    expect_token ~pred:is_func
      ~on_empty:(error_no_function position)
      ~on_unexpected:error_func_expected
  in
  let* name =
    expect_ident
      ~on_empty:(error_unexpected_end func_tok.position)
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
  let* set_tok =
    expect_token ~pred:is_set
      ~on_empty:(error_unexpected_end type_.position)
      ~on_unexpected:error_set_expected
  in
  let* expression = parse_expr false set_tok.position in
  let position = extend_span func_tok.position expression.position in
  return { position; name; parameters; type_; expression }

let rec parse_stmts position funcs =
 fun tokens ->
  let res, rest = parse_func position tokens in
  match res with
  | Ok res -> parse_stmts res.position (res :: funcs) rest
  | Error (NoFunction _) -> return funcs rest
  | Error err -> fail err rest
