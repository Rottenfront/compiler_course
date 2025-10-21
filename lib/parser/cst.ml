open Lexer
open Utils

type literal = LitNumber of int | LitBool of bool

let print_literal literal =
  match literal with
  | LitNumber number -> string_of_int number
  | LitBool true -> "true"
  | LitBool false -> "false"

type operator_type =
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
  | OpSemicolon

type operator = { type_ : operator_type; position : position }

let print_operator operator =
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
  | OpSemicolon -> ";"

type type_value =
  | TyInt
  | TyBool
  | TyUnit
  | TyFunc of type_expr list * type_expr

and type_expr = { type_value : type_value; position : position }

let rec print_type type_ =
  match type_ with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "()"
  | TyFunc (params, return) ->
      "["
      ^ (List.map (fun param -> print_type param.type_value) params
        |> String.concat " ")
      ^ "] "
      ^ print_type return.type_value

let rec equal_type lhs rhs =
  match lhs.type_value with
  | TyInt -> ( match rhs.type_value with TyInt -> true | _ -> false)
  | TyBool -> ( match rhs.type_value with TyBool -> true | _ -> false)
  | TyUnit -> ( match rhs.type_value with TyUnit -> true | _ -> false)
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
  | TmParenth of expr_node

and expr_node = { value : expr; position : position }

type parse_error =
  | ConditionExpressionExpected of position
  | ThenTokenExpected of token
  | ElseTokenExpected of token
  | InTokenExpected of token
  | SetTokenExpected of token
  | ArrowTokenExpected of token
  | IdentTokenExpected of token
  | ParenCloseExpected of token
  | ColonExpected of token
  | CannotUseConditionInFunction of position
  | CannotDefineVariableInFunction of position
  | UnknownOperator of substring
  | ExpectedExpressionAfterOperator of position
  | UnknownType of substring
  | FunctionNameExpected of token
  | UnexpectedEnd of position
  | NoPossibleExpression of position
  | NoFunction of position
  | NoPossibleType of position
  | FuncTokenExpected of token

let print_error error =
  match error with
  | ConditionExpressionExpected pos ->
      Format.sprintf "Condition expression expected on %s" (print_position pos)
  | ThenTokenExpected tok ->
      Format.sprintf "`then` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | ElseTokenExpected tok ->
      Format.sprintf "`else` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | InTokenExpected tok ->
      Format.sprintf "`in` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | SetTokenExpected tok ->
      Format.sprintf "`=` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | ArrowTokenExpected tok ->
      Format.sprintf "`->` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | IdentTokenExpected tok ->
      Format.sprintf "identifier token expected, got: `%s` on %s"
        (print_token tok)
        (print_position tok.position)
  | ParenCloseExpected tok ->
      Format.sprintf "`)` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | ColonExpected tok ->
      Format.sprintf "`:` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | FuncTokenExpected tok ->
      Format.sprintf "`func` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | CannotUseConditionInFunction pos ->
      Format.sprintf
        "Cannot use condition expression inside as an argument without \
         paretheses (%s)"
        (print_position pos)
  | CannotDefineVariableInFunction pos ->
      Format.sprintf
        "Cannot define variables inside as an argument without paretheses (%s)"
        (print_position pos)
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
      Format.sprintf "`then` token expected, got: `%s` on %s" (print_token tok)
        (print_position tok.position)
  | UnexpectedEnd pos ->
      Format.sprintf "Unexpected end on %s" (print_position pos)
  | NoPossibleExpression pos ->
      Format.sprintf "No possible expression on %s" (print_position pos)
  | NoPossibleType pos ->
      Format.sprintf "No possible type on %s" (print_position pos)
  | NoFunction pos ->
      Format.sprintf "No possible function on %s" (print_position pos)

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
 fun rest -> (Error (InTokenExpected tok), rest)

let error_set_expected tok : 'a parser =
 fun rest -> (Error (SetTokenExpected tok), rest)

let error_arrow_expected tok : 'a parser =
 fun rest -> (Error (ArrowTokenExpected tok), rest)

let error_ident_expected tok : 'a parser =
 fun rest -> (Error (IdentTokenExpected tok), rest)

let error_condition_expected pos : 'a parser =
 fun rest -> (Error (ConditionExpressionExpected pos), rest)

let error_else_expected tok : 'a parser =
 fun rest -> (Error (ElseTokenExpected tok), rest)

let error_func_expected tok : 'a parser =
 fun rest -> (Error (FuncTokenExpected tok), rest)

let error_then_expected tok : 'a parser =
 fun rest -> (Error (ThenTokenExpected tok), rest)

let error_function_name_expected tok : 'a parser =
 fun rest -> (Error (FunctionNameExpected tok), rest)

let error_paren_close_expected tok : 'a parser =
 fun rest -> (Error (ParenCloseExpected tok), rest)

let error_colon_expected tok : 'a parser =
 fun rest -> (Error (ColonExpected tok), rest)

let error_cannot_use_condition pos : 'a parser =
 fun rest -> (Error (CannotUseConditionInFunction pos), rest)

let error_cannot_define_variable pos : 'a parser =
 fun rest -> (Error (CannotDefineVariableInFunction pos), rest)

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
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleExpression pos), tokens)
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
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleExpression pos), tokens)
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
  | Error (NoPossibleType pos), _ -> (Error (NoPossibleExpression pos), tokens)
  | other -> other
