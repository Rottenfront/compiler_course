open Lexer
open Utils

type literal = LitNumber of int | LitBool of bool

let print_literal literal =
  match literal with
  | LitNumber number -> string_of_int number
  | LitBool true -> "true"
  | LitBool false -> "false"

type builtin_operator_type =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEq
  | OpNe
  | OpLess
  | OpGreater
  | OpLessEq
  | OpGreaterEq
  | OpAnd
  | OpOr
  | OpXor

type operator_type = BuiltInOp of builtin_operator_type | UserOp of string
type operator = { type_ : operator_type; position : position }

let print_builtin_operator operator =
  match operator with
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq -> "=="
  | OpNe -> "!="
  | OpLess -> "<"
  | OpGreater -> ">"
  | OpLessEq -> "<="
  | OpGreaterEq -> ">="
  | OpAnd -> "&&"
  | OpOr -> "||"
  | OpXor -> "^"

let print_operator operator =
  match operator with
  | BuiltInOp op -> print_builtin_operator op
  | UserOp op -> op

type type_value =
  | TyNamed of string
  | TyTuple of type_expr list
  | TyFunc of type_expr list * type_expr

and type_expr = { type_value : type_value; position : position }

let rec print_type type_ =
  match type_ with
  | TyNamed type_ -> type_
  | TyTuple fields ->
      "("
      ^ (List.map (fun field -> print_type field.type_value) fields
        |> String.concat " * ")
      ^ ")"
  | TyFunc (params, return) ->
      "["
      ^ (List.map (fun param -> print_type param.type_value) params
        |> String.concat " ")
      ^ "] "
      ^ print_type return.type_value

let rec equal_type lhs rhs =
  match lhs.type_value with
  | TyNamed l_type -> (
      match rhs.type_value with TyNamed r_type -> l_type = r_type | _ -> false)
  | TyTuple fields -> (
      match rhs.type_value with
      | TyTuple r_fields ->
          List.combine fields r_fields
          |> List.for_all (fun (lhs, rhs) -> equal_type lhs rhs)
      | _ -> false)
  | TyFunc (params, res) -> (
      match rhs.type_value with
      | TyFunc (r_params, r_res) ->
          List.combine params r_params
          |> List.for_all (fun (lhs, rhs) -> equal_type lhs rhs)
          && equal_type res r_res
      | _ -> false)

type expr =
  | TmLiteral of literal
  | TmApplication of { name : substring; arguments : expr_node list }
  | TmOpApp of { operator : operator; lhs : expr_node; rhs : expr_node }
  | TmLet of { name : substring; value : expr_node; expression : expr_node }
  | TmIf of { condition : expr_node; if_true : expr_node; if_false : expr_node }
  | TmSequence of expr_node list
  | TmParenth of expr_node

and expr_node = { value : expr; position : position }

type parse_error =
  | ConditionExpected of char_position
  | TrueBranchExpected of char_position
  | FalseBranchExpected of char_position
  | TokenExpected of token_type * token
  | IdentTokenExpected of token
  | VariableNameExpected of token
  | VariableValueExpected of char_position
  | UnknownOperator of substring
  | ExpectedExpressionAfterOperator of position
  | UnknownType of substring
  | FunctionNameExpected of token
  | UnexpectedEnd of char_position
  | NoPossibleExpression of char_position
  | NoPossibleType of char_position
  | NoFunction of char_position

let print_error error =
  match error with
  | ConditionExpected pos ->
      Format.sprintf "Condition expected after %s" (print_char_position pos)
  | TrueBranchExpected pos ->
      Format.sprintf "True branch expected after %s" (print_char_position pos)
  | FalseBranchExpected pos ->
      Format.sprintf "False branch expected after %s" (print_char_position pos)
  | VariableValueExpected pos ->
      Format.sprintf "Variable value expected after %s"
        (print_char_position pos)
  | TokenExpected (expected, got) ->
      Format.sprintf "`%s` token expected, got: `%s` on %s"
        (print_token expected) (print_token got.type_)
        (print_position got.position)
  | IdentTokenExpected tok ->
      Format.sprintf "identifier token expected, got: `%s` on %s"
        (print_token tok.type_)
        (print_position tok.position)
  | VariableNameExpected tok ->
      Format.sprintf "Variable name expected, got: `%s` on %s"
        (print_token tok.type_)
        (print_position tok.position)
  | UnknownOperator { str; position } ->
      Format.sprintf "Unknown operator `%s` used on %s" str
        (print_position position)
  | ExpectedExpressionAfterOperator pos ->
      Format.sprintf "Expected expression after operator on %s"
        (print_position pos)
  | UnknownType { str; position } ->
      Format.sprintf "Unknown type `%s` used on %s" str
        (print_position position)
  | FunctionNameExpected tok ->
      Format.sprintf "`then` token expected, got: `%s` on %s"
        (print_token tok.type_)
        (print_position tok.position)
  | UnexpectedEnd pos ->
      Format.sprintf "Unexpected end after %s" (print_char_position pos)
  | NoPossibleExpression pos ->
      Format.sprintf "No possible expression after %s" (print_char_position pos)
  | NoPossibleType pos ->
      Format.sprintf "No possible type after %s" (print_char_position pos)
  | NoFunction pos ->
      Format.sprintf "No possible function after %s" (print_char_position pos)

type implementation = {
  position : position;
  name : substring;
  parameters : (substring * type_expr) list;
  type_ : type_expr;
  expression : expr_node;
}

let default_char_position : char_position = { line = 0; char = 0 }
let default_position : position = (default_char_position, default_char_position)

type 'a parser = token list -> ('a, parse_error) result * token list

let return (x : 'a) : 'a parser = fun tokens -> (Ok x, tokens)

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun tokens ->
  match p tokens with Ok v, rest -> f v rest | Error e, rest -> (Error e, rest)

let fail (err : parse_error) : 'a parser = fun rest -> (Error err, rest)
let ( let* ) = bind

let map p f =
 fun tokens ->
  match p tokens with Ok (v, rest) -> Ok (f v, rest) | Error e -> Error e

let or_else p q =
 fun tokens ->
  match p tokens with
  | Ok ok, cs -> (Ok ok, cs)
  | Error (NoPossibleExpression _), _ | Error (NoPossibleType _), _ -> q tokens
  | Error e, cs -> (Error e, cs)

let ( <|> ) = or_else
let run (p : 'a parser) (tokens : token list) = p tokens

let error_unexpected_end pos : 'a parser =
 fun rest -> (Error (UnexpectedEnd pos), rest)

let error_in_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkIn, tok)), rest)

let error_set_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkOperator "=", tok)), rest)

let error_arrow_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkOperator "->", tok)), rest)

let error_ident_expected tok : 'a parser =
 fun rest -> (Error (IdentTokenExpected tok), rest)

let error_variable_name_expected tok : 'a parser =
 fun rest -> (Error (VariableNameExpected tok), rest)

let error_condition_expected pos : 'a parser =
 fun rest -> (Error (ConditionExpected pos), rest)

let error_true_branch_expected pos : 'a parser =
 fun rest -> (Error (TrueBranchExpected pos), rest)

let error_false_branch_expected pos : 'a parser =
 fun rest -> (Error (FalseBranchExpected pos), rest)

let error_variable_value_expected pos : 'a parser =
 fun rest -> (Error (FalseBranchExpected pos), rest)

let error_else_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkElse, tok)), rest)

let error_define_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkDefine, tok)), rest)

let error_then_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkThen, tok)), rest)

let error_function_name_expected tok : 'a parser =
 fun rest -> (Error (FunctionNameExpected tok), rest)

let error_paren_close_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkParenClose, tok)), rest)

let error_bracket_close_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkBracketClose, tok)), rest)

let error_brace_close_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkBraceClose, tok)), rest)

let error_colon_expected tok : 'a parser =
 fun rest -> (Error (TokenExpected (TkOperator ":", tok)), rest)

let error_unknown_operator str : 'a parser =
 fun rest -> (Error (UnknownOperator str), rest)

let error_unknown_type str : 'a parser =
 fun rest -> (Error (UnknownType str), rest)

let error_no_possible_expression pos : 'a parser =
 fun rest -> (Error (NoPossibleExpression pos), rest)

let error_no_possible_type pos : 'a parser =
 fun rest -> (Error (NoPossibleType pos), rest)

let error_no_function pos : 'a parser =
 fun rest -> (Error (NoFunction pos), rest)

let error_expected_expression_after_operator pos : 'a parser =
 fun rest -> (Error (ExpectedExpressionAfterOperator pos), rest)

let extend_span ((left, _) : position) ((_, right) : position) : position =
  (left, right)

let expect_token ~pred ~on_empty ~on_unexpected : token parser =
 fun tokens ->
  match
    match tokens with
    | tok :: rest when pred tok.type_ -> (Ok tok, rest)
    | [] -> on_empty []
    | tok :: rest -> on_unexpected tok rest
  with
  | Error (NoPossibleExpression pos), _ ->
      (Error (NoPossibleExpression pos), tokens)
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleType pos), tokens)
  | other -> other

let expect_ident ~on_empty ~on_unexpected : substring parser =
 fun tokens ->
  match
    match tokens with
    | { type_ = TkIdent str; position } :: rest -> (Ok { str; position }, rest)
    | [] -> on_empty []
    | tok :: rest -> on_unexpected tok rest
  with
  | Error (NoPossibleExpression pos), _ ->
      (Error (NoPossibleExpression pos), tokens)
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleType pos), tokens)
  | other -> other

let expect_number ~on_empty ~on_unexpected : (int * position) parser =
 fun tokens ->
  match
    match tokens with
    | { type_ = TkNumber number; position } :: rest ->
        (Ok (number, position), rest)
    | [] -> on_empty []
    | tok :: rest -> on_unexpected tok rest
  with
  | Error (NoPossibleExpression pos), _ ->
      (Error (NoPossibleExpression pos), tokens)
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleType pos), tokens)
  | other -> other

let parse_nonrequired (parser : char_position -> 'a parser)
    (cursor_pos : char_position) : 'a option parser =
 fun tokens ->
  let res, rest = parser cursor_pos tokens in
  match res with
  | Ok expr -> return (Some expr) rest
  | Error (NoPossibleExpression _) -> return None tokens
  | Error (NoPossibleType _) -> return None tokens
  | Error err -> fail err rest

let parse_in_parenth (on_first_error : char_position -> token parser)
    (parser : char_position -> ('a * position) parser)
    (cursor_pos : char_position) : ('a * position) parser =
  let is_parenth_open = function TkParenOpen -> true | _ -> false in
  let is_parenth_close = function TkParenClose -> true | _ -> false in

  let* open_parenth =
    expect_token ~pred:is_parenth_open ~on_empty:(on_first_error cursor_pos)
      ~on_unexpected:(fun _ -> on_first_error cursor_pos)
  in
  let* inner, position = parser (snd open_parenth.position) in
  let* close_parenth =
    expect_token ~pred:is_parenth_close
      ~on_empty:(error_unexpected_end (snd position))
      ~on_unexpected:error_paren_close_expected
  in

  let position = extend_span open_parenth.position close_parenth.position in
  return (inner, position)

let parse_in_brackets (on_first_error : char_position -> token parser)
    (parser : char_position -> ('a * position) parser)
    (cursor_pos : char_position) : ('a * position) parser =
  let is_bracket_open = function TkBracketOpen -> true | _ -> false in
  let is_bracket_close = function TkBracketClose -> true | _ -> false in

  let* open_bracket =
    expect_token ~pred:is_bracket_open ~on_empty:(on_first_error cursor_pos)
      ~on_unexpected:(fun _ -> on_first_error cursor_pos)
  in
  let* inner, position = parser (snd open_bracket.position) in
  let* close_bracket =
    expect_token ~pred:is_bracket_close
      ~on_empty:(error_unexpected_end (snd position))
      ~on_unexpected:error_bracket_close_expected
  in

  let position = extend_span open_bracket.position close_bracket.position in
  return (inner, position)

let parse_in_braces (on_first_error : char_position -> token parser)
    (parser : char_position -> ('a * position) parser)
    (cursor_pos : char_position) : ('a * position) parser =
  let is_brace_open = function TkBraceOpen -> true | _ -> false in
  let is_brace_close = function TkBraceClose -> true | _ -> false in

  let* open_bracket =
    expect_token ~pred:is_brace_open ~on_empty:(on_first_error cursor_pos)
      ~on_unexpected:(fun _ -> on_first_error cursor_pos)
  in
  let* inner, position = parser (snd open_bracket.position) in
  let* close_bracket =
    expect_token ~pred:is_brace_close
      ~on_empty:(error_unexpected_end (snd position))
      ~on_unexpected:error_brace_close_expected
  in

  let position = extend_span open_bracket.position close_bracket.position in
  return (inner, position)
