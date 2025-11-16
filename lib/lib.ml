module Utils = struct
  module StringMap = Map.Make (String)
  module StringSet = Set.Make (String)

  type position = { line : int; char : int }
  type span = position * position
  type substring = { str : string; position : span }

  let print_position { line; char } =
    Format.sprintf "%d:%d" (line + 1) (char + 1)

  let print_span ((left, right) : span) =
    print_position left ^ "-" ^ print_position right

  let default_position : position = { line = 0; char = 0 }
  let default_span : span = (default_position, default_position)
  let extend_span ((left, _) : span) ((_, right) : span) : span = (left, right)
end

module Config = struct
  type target_architecture = Arm64Darwin | Arm64Linux
  type source_language = Lama | Racket
end

module Lexer = struct
  open Utils

  type token_type =
    | TkIdent of string
    | TkOperator of string
    | TkNumber of int
    | TkParenOpen
    | TkParenClose
    | TkBracketOpen
    | TkBracketClose
    | TkBraceOpen
    | TkBraceClose
    | TkBegin
    | TkLet
    | TkIn
    | TkDefine
    | TkDefOp
    | TkType
    | TkIf
    | TkThen
    | TkElse
    | TkTrue
    | TkFalse

  type token = { type_ : token_type; position : span }

  let print_token token =
    match token with
    | TkIdent ident -> ident
    | TkOperator ident -> ident
    | TkNumber num -> string_of_int num
    | TkParenOpen -> "("
    | TkParenClose -> ")"
    | TkBracketOpen -> "["
    | TkBracketClose -> "]"
    | TkBraceOpen -> "{"
    | TkBraceClose -> "}"
    | TkBegin -> "begin"
    | TkLet -> "let"
    | TkIn -> "in"
    | TkIf -> "if"
    | TkThen -> "then"
    | TkElse -> "else"
    | TkTrue -> "true"
    | TkFalse -> "false"
    | TkDefine -> "define"
    | TkDefOp -> "defop"
    | TkType -> "type"

  let is_digit c = c >= '0' && c <= '9'

  let is_operator_symbol c =
    c = '-' || c = '+' || c = '!' || c = '%' || c = '^' || c = '&' || c = '*'
    || c = '/' || c = '=' || c = '<' || c = '>' || c = '|' || c = ':' || c = '.'
    || c = ';'

  let is_special_char c =
    c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}'

  let is_whitespace c = c = '\n' || c = '\t' || c = '\r' || c = ' '

  (* List to string *)
  let list_to_reversed_string x =
    let len = List.length x in
    String.init len (fun n -> List.nth x (len - n - 1))

  let rec lex_ident use_operator_symbols pos acc input =
    match input with
    | c :: rest
      when is_special_char c
           || (is_operator_symbol c && not use_operator_symbols)
           || is_whitespace c ->
        (list_to_reversed_string acc, pos, c :: rest)
    | c :: rest -> lex_ident use_operator_symbols (pos + 1) (c :: acc) rest
    | [] -> (list_to_reversed_string acc, pos, [])

  let rec lex_operator pos acc input =
    match input with
    | c :: rest when is_operator_symbol c ->
        lex_operator (pos + 1) (c :: acc) rest
    | rest -> (list_to_reversed_string acc, pos, rest)

  let rec lex_number pos acc input =
    match input with
    | c :: rest when is_digit c -> lex_number (pos + 1) (c :: acc) rest
    | rest -> (int_of_string (list_to_reversed_string acc), pos, rest)

  let rec lex_string line pos acc input =
    match input with
    | [] -> failwith "Undetermined string"
    | '\n' :: rest -> lex_string (line + 1) 0 ('\n' :: acc) rest
    | '\\' :: rest -> (
        match rest with
        | 'r' :: cs -> lex_string line (pos + 2) ('\r' :: acc) cs
        | 'n' :: cs -> lex_string line (pos + 2) ('\n' :: acc) cs
        | 't' :: cs -> lex_string line (pos + 2) ('\t' :: acc) cs
        | '"' :: cs -> lex_string line (pos + 2) ('"' :: acc) cs
        | '\\' :: cs -> lex_string line (pos + 2) ('\\' :: acc) cs
        | _ -> failwith "Unknown backslash symbol")
    | '"' :: rest -> (list_to_reversed_string acc, line, pos + 1, rest)
    | c :: rest -> lex_string line (pos + 1) (c :: acc) rest

  let one_char_position line char = ({ line; char }, { line; char = char + 1 })

  let one_line_position line char new_char =
    ({ line; char }, { line; char = new_char })

  let lexer (source_language : Config.source_language) (input : string) :
      token list =
    let rec aux line pos input =
      match input with
      | [] -> []
      | '\n' :: rest -> aux (line + 1) 0 rest
      | c :: rest when is_whitespace c -> aux line (pos + 1) rest
      | c :: rest when is_special_char c ->
          let position = one_char_position line pos in
          {
            type_ =
              (match c with
              | '(' -> TkParenOpen
              | ')' -> TkParenClose
              | '[' -> TkBracketOpen
              | ']' -> TkBracketClose
              | '{' -> TkBraceOpen
              | '}' -> TkBraceClose
              | _ -> failwith "unreachable");
            position;
          }
          :: aux line (pos + 1) rest
      | c :: rest when is_digit c ->
          let number, new_pos, cs = lex_number pos [ c ] rest in
          let position = one_line_position line pos new_pos in
          { type_ = TkNumber number; position } :: aux line new_pos cs
      | c :: rest when is_operator_symbol c && source_language = Config.Lama ->
          let operator, new_pos, cs = lex_operator pos [ c ] rest in
          let position = one_line_position line pos new_pos in
          { type_ = TkOperator operator; position } :: aux line new_pos cs
      | c :: rest ->
          let ident, new_pos, cs =
            lex_ident (source_language = Config.Racket) pos [ c ] rest
          in
          let position = one_line_position line pos new_pos in
          let type_ =
            match ident with
            | "let" -> TkLet
            | "if" -> TkIf
            | "define" -> TkDefine
            | "type" -> TkType
            | ident -> (
                match source_language with
                | Config.Lama -> (
                    match ident with
                    | "and" -> TkOperator "and"
                    | "or" -> TkOperator "or"
                    | "xor" -> TkOperator "xor"
                    | "in" -> TkIn
                    | "then" -> TkThen
                    | "else" -> TkElse
                    | "true" -> TkTrue
                    | "false" -> TkFalse
                    | "defop" -> TkDefOp
                    | ident -> TkIdent ident)
                | Config.Racket -> (
                    match ident with
                    | "begin" -> TkBegin
                    | "#t" -> TkTrue
                    | "#f" -> TkFalse
                    | ident -> TkIdent ident))
          in
          { type_; position } :: aux line new_pos cs
    in
    aux 0 0 (List.init (String.length input) (String.get input))
end

module ParserError = struct
  open Utils
  open Lexer

  type parse_error =
    | ConditionExpected of position
    | TrueBranchExpected of position
    | FalseBranchExpected of position
    | TokenExpected of token_type * token
    | IdentTokenExpected of token
    | VariableNameExpected of token
    | VariableValueExpected of position
    | UnknownOperator of substring
    | ExpectedExpressionAfterOperator of span
    | UnknownType of substring
    | FunctionNameExpected of token
    | UnexpectedEnd of position
    | ExpressionExpected of position
    | TypeExpected of position
    | StatementExpected of token
    | NoPossibleExpression of position
    | NoPossibleType of position
    | NoStatement of position
    | FunctionDataExpected of position

  let print_error error =
    match error with
    | ExpressionExpected pos ->
        Format.sprintf "Expresison expected after %s" (print_position pos)
    | FunctionDataExpected pos ->
        Format.sprintf "Function signature expected after %s"
          (print_position pos)
    | TypeExpected pos ->
        Format.sprintf "Type expected after %s" (print_position pos)
    | ConditionExpected pos ->
        Format.sprintf "Condition expected after %s" (print_position pos)
    | TrueBranchExpected pos ->
        Format.sprintf "True branch expected after %s" (print_position pos)
    | FalseBranchExpected pos ->
        Format.sprintf "False branch expected after %s" (print_position pos)
    | VariableValueExpected pos ->
        Format.sprintf "Variable value expected after %s" (print_position pos)
    | TokenExpected (expected, got) ->
        Format.sprintf "`%s` token expected, got: `%s` on %s"
          (print_token expected) (print_token got.type_)
          (print_span got.position)
    | IdentTokenExpected tok ->
        Format.sprintf "identifier token expected, got: `%s` on %s"
          (print_token tok.type_) (print_span tok.position)
    | VariableNameExpected tok ->
        Format.sprintf "Variable name expected, got: `%s` on %s"
          (print_token tok.type_) (print_span tok.position)
    | UnknownOperator { str; position } ->
        Format.sprintf "Unknown operator `%s` used on %s" str
          (print_span position)
    | ExpectedExpressionAfterOperator pos ->
        Format.sprintf "Expected expression after operator on %s"
          (print_span pos)
    | UnknownType { str; position } ->
        Format.sprintf "Unknown type `%s` used on %s" str (print_span position)
    | FunctionNameExpected tok ->
        Format.sprintf "`then` token expected, got: `%s` on %s"
          (print_token tok.type_) (print_span tok.position)
    | StatementExpected tok ->
        Format.sprintf "Statement expected, got: `%s` on %s"
          (print_token tok.type_) (print_span tok.position)
    | UnexpectedEnd pos ->
        Format.sprintf "Unexpected end after %s" (print_position pos)
    | NoPossibleExpression pos ->
        Format.sprintf "No possible expression after %s" (print_position pos)
    | NoPossibleType pos ->
        Format.sprintf "No possible type after %s" (print_position pos)
    | NoStatement pos ->
        Format.sprintf "No possible statement after %s" (print_position pos)
end

module ParserMonad = struct
  open ParserError
  open Lexer
  open Utils

  type 'a parser = token list -> ('a, parse_error) result * token list

  let return (x : 'a) : 'a parser = fun tokens -> (Ok x, tokens)

  let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
   fun tokens ->
    match p tokens with
    | Ok v, rest -> f v rest
    | Error e, rest -> (Error e, rest)

  let fail (err : parse_error) : 'a parser = fun rest -> (Error err, rest)
  let ( let* ) = bind

  let map p f =
   fun tokens ->
    match p tokens with Ok (v, rest) -> Ok (f v, rest) | Error e -> Error e

  let or_else p q =
   fun tokens ->
    match p tokens with
    | Ok ok, cs -> (Ok ok, cs)
    | Error (NoPossibleExpression _), _ | Error (NoPossibleType _), _ ->
        q tokens
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

  let error_no_statement pos : 'a parser =
   fun rest -> (Error (NoStatement pos), rest)

  let error_expression_expected pos : 'a parser =
   fun rest -> (Error (ExpressionExpected pos), rest)

  let error_function_data_expected pos : 'a parser =
   fun rest -> (Error (FunctionDataExpected pos), rest)

  let error_type_expected pos : 'a parser =
   fun rest -> (Error (TypeExpected pos), rest)

  let error_statement_expected tok : 'a parser =
   fun rest -> (Error (StatementExpected tok), rest)

  let error_expected_expression_after_operator pos : 'a parser =
   fun rest -> (Error (ExpectedExpressionAfterOperator pos), rest)

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

  let expect_operator ~on_empty ~on_unexpected : substring parser =
   fun tokens ->
    match
      match tokens with
      | { type_ = TkOperator str; position } :: rest ->
          (Ok { str; position }, rest)
      | [] -> on_empty []
      | tok :: rest -> on_unexpected tok rest
    with
    | Error (NoPossibleExpression pos), _ ->
        (Error (NoPossibleExpression pos), tokens)
    | Error (NoPossibleType pos), _ -> (Error (NoPossibleType pos), tokens)
    | other -> other

  let expect_number ~on_empty ~on_unexpected : (int * span) parser =
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

  let parse_required (parser : position -> 'a parser)
      (error_on_end : position -> 'a parser) (cursor_pos : position) : 'a parser
      =
   fun tokens ->
    let res, rest = parser cursor_pos tokens in
    match res with
    | Ok expr -> return expr rest
    | Error (NoPossibleExpression pos) -> error_on_end pos tokens
    | Error (NoPossibleType pos) -> error_on_end pos tokens
    | Error err -> fail err rest

  let parse_nonrequired (parser : position -> 'a parser) (cursor_pos : position)
      : 'a option parser =
   fun tokens ->
    let res, rest = parser cursor_pos tokens in
    match res with
    | Ok expr -> return (Some expr) rest
    | Error (NoPossibleExpression _) -> return None tokens
    | Error (NoPossibleType _) -> return None tokens
    | Error err -> fail err rest

  let parse_in_parenth (on_first_error : position -> token parser)
      (parser : position -> ('a * span) parser) (cursor_pos : position) :
      ('a * span) parser =
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

  let parse_in_brackets (on_first_error : position -> token parser)
      (parser : position -> ('a * span) parser) (cursor_pos : position) :
      ('a * span) parser =
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

  let parse_in_braces (on_first_error : position -> token parser)
      (parser : position -> ('a * span) parser) (cursor_pos : position) :
      ('a * span) parser =
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
end

module Parser = struct
  module Cst = struct
    open Lexer
    open Utils

    type literal = LitNumber of int | LitBool of bool

    type type_value =
      | TyNamed of string
      | TyTuple of type_expr list
      | TyFunc of type_expr list * type_expr

    and type_expr = { type_value : type_value; position : span }

    type expr =
      | TmLiteral of literal
      | TmApplication of { name : substring; arguments : expr_node list }
      | TmOpApp of { operator : substring; lhs : expr_node; rhs : expr_node }
      | TmUnaryOp of { operator : substring; argument : expr_node }
      | TmLet of { name : substring; value : expr_node; expression : expr_node }
      | TmIf of {
          condition : expr_node;
          if_true : expr_node;
          if_false : expr_node;
        }
      | TmSequence of expr_node list
      | TmParenth of expr_node

    and expr_node = { value : expr; position : span }

    type implementation = {
      position : span;
      name : substring;
      parameters : (substring * type_expr) list;
      type_ : type_expr;
      expression : expr_node;
    }

    type type_declaration = {
      position : span;
      name : substring;
      value : type_expr;
    }

    type statement = Function of implementation | TypeDecl of type_declaration

    let print_literal literal =
      match literal with
      | LitNumber number -> string_of_int number
      | LitBool true -> "true"
      | LitBool false -> "false"

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
          match rhs.type_value with
          | TyNamed r_type -> l_type = r_type
          | _ -> false)
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
  end

  module Lama = struct
    open Lexer
    open Utils
    open ParserMonad
    open Cst

    let rec parse_expr (in_function : bool) (cursor_pos : position) :
        expr_node parser =
      let parse_if (cursor_pos : position) : expr_node parser =
        let is_if = function TkIf -> true | _ -> false in
        let is_then = function TkThen -> true | _ -> false in
        let is_else = function TkElse -> true | _ -> false in

        let* if_tok =
          expect_token ~pred:is_if
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in

        let* condition =
          parse_required (parse_expr false) error_condition_expected
            (snd if_tok.position)
        in

        let* then_tok =
          expect_token ~pred:is_then
            ~on_empty:(error_unexpected_end (snd condition.position))
            ~on_unexpected:error_then_expected
        in

        let* if_true =
          parse_required (parse_expr false) error_true_branch_expected
            (snd then_tok.position)
        in

        let* else_tok =
          expect_token ~pred:is_else
            ~on_empty:(error_unexpected_end (snd if_true.position))
            ~on_unexpected:error_else_expected
        in

        let* if_false =
          parse_required (parse_expr false) error_false_branch_expected
            (snd else_tok.position)
        in
        let position = extend_span if_tok.position if_false.position in
        return { value = TmIf { condition; if_true; if_false }; position }
      in

      let parse_let (cursor_pos : position) : expr_node parser =
        let is_let = function TkLet -> true | _ -> false in
        let is_set = function TkOperator "=" -> true | _ -> false in
        let is_in = function TkIn -> true | _ -> false in

        let* let_tok =
          expect_token ~pred:is_let
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in

        let* name =
          expect_ident
            ~on_empty:(error_unexpected_end (snd let_tok.position))
            ~on_unexpected:error_ident_expected
        in

        let* set_tok =
          expect_token ~pred:is_set
            ~on_empty:(error_unexpected_end (snd name.position))
            ~on_unexpected:error_set_expected
        in

        let* value =
          parse_required (parse_expr false) error_expression_expected
            (snd set_tok.position)
        in

        let* in_tok =
          expect_token ~pred:is_in
            ~on_empty:(error_unexpected_end (snd value.position))
            ~on_unexpected:error_in_expected
        in
        let* expression =
          parse_required (parse_expr false) error_expression_expected
            (snd in_tok.position)
        in
        let position = extend_span let_tok.position expression.position in
        return { value = TmLet { name; value; expression }; position }
      in

      let parse_parenth (cursor_pos : position) : expr_node parser =
        let is_parenth_open = function TkParenOpen -> true | _ -> false in
        let is_parenth_close = function TkParenClose -> true | _ -> false in

        let* open_parenth =
          expect_token ~pred:is_parenth_open
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        let* inner =
          parse_required (parse_expr false) error_expression_expected
            (snd open_parenth.position)
        in
        let* close_parenth =
          expect_token ~pred:is_parenth_close
            ~on_empty:(error_unexpected_end (snd inner.position))
            ~on_unexpected:error_in_expected
        in

        let position =
          extend_span open_parenth.position close_parenth.position
        in
        return { value = TmParenth inner; position }
      in

      let parse_function (cursor_pos : position) : expr_node parser =
        let rec parse_arguments (name : substring) (arguments : expr_node list)
            (cursor_pos : position) =
          let* argument = parse_nonrequired (parse_expr true) cursor_pos in
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
          expect_ident ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        parse_arguments name [] (snd name.position)
      in

      let parse_ident (cursor_pos : position) : expr_node parser =
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

      let parse_number (cursor_pos : position) : expr_node parser =
        let* number, position =
          expect_number ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitNumber number); position }
      in

      let parse_true (cursor_pos : position) : expr_node parser =
        let is_true = function TkTrue -> true | _ -> false in
        let* token =
          expect_token ~pred:is_true
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitBool true); position = token.position }
      in

      let parse_false (cursor_pos : position) : expr_node parser =
        let is_false = function TkFalse -> true | _ -> false in
        let* token =
          expect_token ~pred:is_false
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitBool false); position = token.position }
      in

      let parse_literal (cursor_pos : position) : expr_node parser =
        parse_true cursor_pos <|> parse_false cursor_pos
        <|> parse_number cursor_pos
      in

      let parse_binary_operator (lhs : expr_node) : expr_node parser =
        let* operator =
          expect_operator
            ~on_empty:(error_no_possible_expression (snd lhs.position))
            ~on_unexpected:(fun _ ->
              error_no_possible_expression (snd lhs.position))
        in
        let* rhs =
          parse_required (parse_expr false) error_expression_expected
            (snd operator.position)
        in
        let position = extend_span lhs.position rhs.position in
        return { value = TmOpApp { operator; lhs; rhs }; position }
      in

      let parse_basic_expr (cursor_pos : position) : expr_node parser =
        parse_parenth cursor_pos <|> parse_literal cursor_pos
        <|> parse_let cursor_pos <|> parse_if cursor_pos
        <|> parse_function cursor_pos
      in

      let parse_argument_expr (cursor_pos : position) : expr_node parser =
        parse_parenth cursor_pos <|> parse_literal cursor_pos
        <|> parse_ident cursor_pos
      in

      let parse_unary_operator (cursor_pos : position) : expr_node parser =
        let* operator =
          parse_nonrequired
            (fun cursor_pos ->
              expect_operator
                ~on_empty:(error_no_possible_expression cursor_pos)
                ~on_unexpected:(fun _ ->
                  error_no_possible_expression cursor_pos))
            cursor_pos
        in
        match operator with
        | Some operator ->
            let* argument =
              parse_required parse_basic_expr error_expression_expected
                (snd operator.position)
            in
            let position = extend_span operator.position argument.position in
            return { value = TmUnaryOp { operator; argument }; position }
        | None -> parse_basic_expr cursor_pos
      in

      if in_function then parse_argument_expr cursor_pos
      else
        let* lhs = parse_unary_operator cursor_pos in
        parse_binary_operator lhs <|> return lhs

    let rec parse_type (cursor_pos : position) : type_expr parser =
      let parse_basic_type (cursor_pos : position) : type_expr parser =
        let* { str; position } =
          expect_ident ~on_empty:(error_no_possible_type cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_type cursor_pos)
        in
        return { type_value = TyNamed str; position }
      in

      let parse_parentheses (cursor_pos : position) : type_expr parser =
        let is_parenth_open = function TkParenOpen -> true | _ -> false in
        let is_parenth_close = function TkParenClose -> true | _ -> false in

        let* open_parenth =
          expect_token ~pred:is_parenth_open
            ~on_empty:(error_no_possible_type cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_type cursor_pos)
        in
        let* inner = parse_nonrequired parse_type (snd open_parenth.position) in
        let next_cursor_pos =
          snd
            (match inner with
            | Some type_ -> type_.position
            | None -> open_parenth.position)
        in
        let* close_parenth =
          expect_token ~pred:is_parenth_close
            ~on_empty:(error_unexpected_end next_cursor_pos)
            ~on_unexpected:error_paren_close_expected
        in
        let inner =
          match inner with
          | Some type_ -> type_
          | None ->
              let position =
                extend_span open_parenth.position close_parenth.position
              in
              { type_value = TyTuple []; position }
        in
        return inner
      in

      let parse_cellar_type (cursor_pos : position) : type_expr parser =
        parse_basic_type cursor_pos <|> parse_parentheses cursor_pos
      in

      let parse_function_type (position : span) : type_expr parser =
        let parse_nonrequired_type (position : span) : type_expr option parser =
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

    let parse_func (position : span) : implementation parser =
      let parse_parameter (position : span) :
          (substring * type_expr * span) parser =
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

        let position =
          extend_span open_parenth.position close_parenth.position
        in

        return (name, type_, position)
      in

      let rec parse_parameters (position : span)
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

      let is_define = function TkDefine -> true | _ -> false in
      let is_set = function TkOperator "=" -> true | _ -> false in
      let is_colon = function TkOperator ":" -> true | _ -> false in
      let* define_tok =
        expect_token ~pred:is_define
          ~on_empty:(error_no_function position)
          ~on_unexpected:error_define_expected
      in
      let* name =
        expect_ident
          ~on_empty:(error_unexpected_end define_tok.position)
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
      let expression = distribute_operator expression in
      let position = extend_span define_tok.position expression.position in
      return { position; name; parameters; type_; expression }

    let rec parse_stmts position funcs =
     fun tokens ->
      let res, rest = parse_func position tokens in
      match res with
      | Ok res -> parse_stmts res.position (res :: funcs) rest
      | Error (NoFunction _) -> return funcs rest
      | Error err -> fail err rest
  end

  module Racket = struct
    open Lexer
    open Utils
    open Cst

    let parse_in (on_first_error : position -> token parser)
        (parser : position -> ('a * span) parser) (cursor_pos : position) :
        ('a * span) parser =
      parse_in_parenth on_first_error parser cursor_pos
      <|> parse_in_brackets on_first_error parser cursor_pos
      <|> parse_in_braces on_first_error parser cursor_pos

    let rec parse_expr (cursor_pos : position) : expr_node parser =
      let parse_in_expr (parser : position -> (expr_node * span) parser)
          (cursor_pos : position) : expr_node parser =
        let* expr, position =
          parse_in error_no_possible_expression parser cursor_pos
        in
        return { value = expr.value; position }
      in

      let parse_if (cursor_pos : position) : (expr_node * span) parser =
        let is_if = function TkIf -> true | _ -> false in

        let* if_tok =
          expect_token ~pred:is_if
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in

        let* condition =
          parse_required parse_expr error_condition_expected
            (snd if_tok.position)
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

      let parse_let (position : position) : (expr_node * span) parser =
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
        let rec parse_assignments (acc : (substring * expr_node * span) list)
            (assignments_span : span) :
            ((substring * expr_node * span) list * span) parser =
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
        let rec construct_let (variables : (substring * expr_node * span) list)
            (expression : expr_node) (let_position : span) : expr_node =
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

      let parse_function (position : position) : (expr_node * span) parser =
        let rec parse_arguments (name : substring) (arguments : expr_node list)
            (cursor_pos : position) =
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

      let parse_sequence (position : position) : (expr_node * span) parser =
        let rec parse_arguments (arguments : expr_node list) (span : span) =
          let* argument = parse_nonrequired parse_expr cursor_pos in
          match argument with
          | Some new_arg ->
              parse_arguments (new_arg :: arguments)
                (extend_span span new_arg.position)
          | None ->
              return
                { value = TmSequence (List.rev arguments); position = span }
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

      let parse_ident (cursor_pos : position) : expr_node parser =
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

      let parse_number (cursor_pos : position) : expr_node parser =
        let* number, position =
          expect_number ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitNumber number); position }
      in

      let parse_true (cursor_pos : position) : expr_node parser =
        let is_true = function TkTrue -> true | _ -> false in
        let* token =
          expect_token ~pred:is_true
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitBool true); position = token.position }
      in

      let parse_false (cursor_pos : position) : expr_node parser =
        let is_false = function TkFalse -> true | _ -> false in
        let* token =
          expect_token ~pred:is_false
            ~on_empty:(error_no_possible_expression cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_expression cursor_pos)
        in
        return { value = TmLiteral (LitBool false); position = token.position }
      in

      let parse_basic_expr (cursor_pos : position) : expr_node parser =
        parse_true cursor_pos <|> parse_false cursor_pos
        <|> parse_number cursor_pos <|> parse_ident cursor_pos
      in

      parse_basic_expr cursor_pos
      <|> parse_in_expr
            (fun position ->
              parse_let position <|> parse_if position
              <|> parse_function position <|> parse_sequence position)
            cursor_pos

    let rec parse_type (cursor_pos : position) : type_expr parser =
      let parse_basic_type (cursor_pos : position) : type_expr parser =
        let* { str; position } =
          expect_ident ~on_empty:(error_no_possible_type cursor_pos)
            ~on_unexpected:(fun tok ->
              error_no_possible_type (snd tok.position))
        in
        return { type_value = TyNamed str; position }
      in
      let parse_parentheses (cursor_pos : position) : type_expr parser =
        let is_parenth_open = function TkParenOpen -> true | _ -> false in
        let is_parenth_close = function TkParenClose -> true | _ -> false in

        let* open_parenth =
          expect_token ~pred:is_parenth_open
            ~on_empty:(error_no_possible_type cursor_pos)
            ~on_unexpected:(fun _ -> error_no_possible_type cursor_pos)
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
        let position =
          extend_span open_parenth.position close_parenth.position
        in
        return { type_value = inner; position }
      in

      let parse_function_type (cursor_pos : position) : type_expr parser =
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

      let rec parse_tuple_type (prev : type_expr list) (cursor_pos : position) :
          type_expr parser =
        let parse_next_type (cursor_pos : position) : type_expr parser =
          let is_star = function TkOperator "*" -> true | _ -> false in
          let* star =
            expect_token ~pred:is_star
              ~on_empty:(error_no_possible_type cursor_pos)
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
                (fun (rhs : span) (lhs : type_expr) ->
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

    let parse_func (cursor_pos : position) : (implementation * span) parser =
      let parse_parameter (cursor_pos : position) :
          ((substring * type_expr) * span) parser =
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

      let rec parse_parameters (cursor_pos : position)
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

      let parse_function_data (cursor_pos : position) :
          ((substring * (substring * type_expr) list * type_expr) * span) parser
          =
        let is_colon = function TkIdent ":" -> true | _ -> false in
        let* name =
          expect_ident
            ~on_empty:(error_unexpected_end cursor_pos)
            ~on_unexpected:error_function_name_expected
        in
        let* parameters = parse_parameters (snd name.position) [] in
        let last_position =
          if List.is_empty parameters then name.position
          else
            (List.nth parameters (List.length parameters - 1) |> snd).position
        in
        let* colon_tok =
          expect_token ~pred:is_colon
            ~on_empty:(error_unexpected_end (snd last_position))
            ~on_unexpected:error_colon_expected
        in
        let* type_ =
          parse_required parse_type error_type_expected (snd colon_tok.position)
        in
        let position = extend_span name.position type_.position in
        return ((name, parameters, type_), position)
      in

      let is_define = function TkDefine -> true | _ -> false in

      let* define_tok =
        expect_token ~pred:is_define ~on_empty:(error_no_statement cursor_pos)
          ~on_unexpected:(fun _ -> error_no_statement cursor_pos)
      in
      let* (name, parameters, type_), position =
        parse_in error_function_data_expected parse_function_data
          (snd define_tok.position)
      in
      let* expression =
        parse_required parse_expr error_expression_expected (snd position)
      in
      (* let expression = distribute_operator expression in *)
      let position = extend_span define_tok.position expression.position in
      return ({ position; name; parameters; type_; expression }, position)

    let rec parse_func_statement (cursor_pos : position) =
      parse_nonrequired (parse_in error_no_statement parse_func) cursor_pos
  end

  let parse language =
    match language with
    | Config.Lama -> Lama.parse_stmts
    | Config.Racket -> Racket.parse_stmts
end

module Checker = struct
  open Parser.Cst
  open Utils

  type check_context = { functions : type_value StringMap.t }

  type check_error =
    | UnsupportedHighOrderFunctions of span
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
          (print_span pos)
    | TypeMismatch (type1, type2) ->
        Format.sprintf
          "Mismatched types, expected: %s (source: %s), got: %s (source: %s)"
          (print_type type1.type_value)
          (print_span type1.position)
          (print_type type2.type_value)
          (print_span type2.position)
    | DoubleDeclaration name ->
        Format.sprintf "Double declaration of function '%s' on %s" name.str
          (print_span name.position)
    | NonexistentFunction name ->
        Format.sprintf "No function with name '%s' found, usage: %s" name.str
          (print_span name.position)
    | NotAFunction (name, args_count) ->
        Format.sprintf
          "'%s' (%s) is not a function, but %d arguments were provided" name.str
          (print_span name.position) args_count
    | UnexpectedArgumentCount (name, expected, provided) ->
        Format.sprintf
          "Function '%s' (%s) has %d parameters, but %d arguments were provided"
          name.str (print_span name.position) expected provided

  let get_type_parameter_count type_ =
    match type_.type_value with
    | TyFunc (parameters, _) -> List.length parameters
    | _ -> failwith "unreachable"

  let get_result_type type_ =
    match type_ with TyFunc (_, res) -> res.type_value | _ -> type_

  let rec get_type (context : check_context) (expr : expr_node) :
      type_expr option =
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
              {
                type_value = StringMap.find name.str context.functions;
                position;
              }
          else None
      | TmOpApp { lhs = _; operator; rhs } -> (
          match operator.type_ with
          | OpSemicolon -> get_type context rhs
          | other ->
              Some
                (match other with
                | OpAdd | OpSub | OpMul | OpDiv ->
                    { type_value = TyInt; position }
                | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq
                | OpAnd | OpOr | OpXor ->
                    { type_value = TyBool; position }
                | OpSemicolon -> failwith "unreachable"))
      | TmIf { condition = _; if_true; if_false = _ } ->
          get_type context if_true
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
                ( [ { type_value = TyInt; position = default_span } ],
                  { type_value = TyUnit; position = default_span } ))
        |> add_builtin_declaration "print_bool"
             (TyFunc
                ( [ { type_value = TyBool; position = default_span } ],
                  { type_value = TyUnit; position = default_span } ));
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
end

module Compiler = struct
  module Uniquify = struct
    open Utils
    open Parser.Ast

    let rec uniquify_expr count (context : string StringMap.t) expr =
      let rec uniquify_list count context prev exprs =
        match exprs with
        | expr :: rest ->
            let expr, count = uniquify_expr count context expr in
            uniquify_list count context (expr :: prev) rest
        | [] -> (List.rev prev, count)
      in
      match expr with
      | TmApplication { name; arguments } ->
          let arguments, count = uniquify_list count context [] arguments in
          ( TmApplication { name = StringMap.find name context; arguments },
            count )
      | TmLiteral lit -> (TmLiteral lit, count)
      | TmOpApp { lhs; operator; rhs } ->
          let lhs, count = uniquify_expr count context lhs in
          let rhs, count = uniquify_expr count context rhs in
          (TmOpApp { lhs; operator; rhs }, count)
      | TmIf { condition; if_true; if_false } ->
          let condition, count = uniquify_expr count context condition in
          let if_true, count = uniquify_expr count context if_true in
          let if_false, count = uniquify_expr count context if_false in
          (TmIf { condition; if_true; if_false }, count)
      | TmLet { name; value; expression } ->
          let value, count = uniquify_expr count context value in
          let new_name = Format.sprintf "%s.%d" name count in
          let context = StringMap.add name new_name context in
          let count = count + 1 in
          let expression, count = uniquify_expr count context expression in
          (TmLet { name = new_name; value; expression }, count)
  end

  module Monadic = struct
    open Parser.Ast
    open Utils

    type atm = AtmVar of string | AtmInt of int | AtmBool of bool

    type math_op =
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
      | OpXor

    let print_math_op operator =
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
      | OpXor -> "^"

    type node =
      | AtmValue of atm
      | AtmFunction of string * atm list
      | AtmOp of math_op * atm * atm
      | AtmIf of atm * node * node
      | Let of string * node * node
      | Sequence of node * node

    type simplified = Node of node | Atm of atm

    let rec remove_complex_operands variables count expr =
      let simplify_argument variables count expr =
        match expr with
        | TmLiteral lit ->
            ( Atm
                (match lit with
                | LitBool value -> AtmBool value
                | LitNumber value -> AtmInt value),
              count )
        | TmApplication { name; _ } ->
            if StringMap.mem name variables then (Atm (AtmVar name), count)
            else
              let node, count = remove_complex_operands variables count expr in
              (Node node, count)
        | other ->
            let node, count = remove_complex_operands variables count other in
            (Node node, count)
      in
      let create_let expr result =
        match expr with
        | Atm expr, count -> result expr count
        | Node expr, count ->
            let arg_name = Format.sprintf "tmp.%d" count in
            let result, count = result (AtmVar arg_name) (count + 1) in
            (Let (arg_name, expr, result), count)
      in
      match expr with
      | TmLiteral lit ->
          ( (match lit with
            | LitBool value -> AtmValue (AtmBool value)
            | LitNumber value -> AtmValue (AtmInt value)),
            count )
      | TmApplication { name; arguments } ->
          if StringMap.mem name variables then (AtmValue (AtmVar name), count)
          else
            let rec simplify_function variables name args prev_args count =
              match args with
              | [] -> (AtmFunction (name, List.rev prev_args), count)
              | arg :: rest ->
                  create_let (simplify_argument variables count arg)
                    (fun atm count ->
                      simplify_function variables name rest (atm :: prev_args)
                        count)
            in
            simplify_function variables name arguments [] count
      | TmOpApp { operator; lhs; rhs } -> (
          match operator with
          | OpSemicolon ->
              let lhs, count = remove_complex_operands variables count lhs in
              let rhs, count = remove_complex_operands variables count rhs in
              (Sequence (lhs, rhs), count)
          | op ->
              create_let (simplify_argument variables count lhs)
                (fun lhs count ->
                  match op with
                  | OpAnd ->
                      let rhs, count =
                        remove_complex_operands variables count rhs
                      in
                      (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
                  | OpOr ->
                      let rhs, count =
                        remove_complex_operands variables count rhs
                      in
                      (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
                  | other ->
                      let op =
                        match other with
                        | OpAdd -> OpAdd
                        | OpSub -> OpSub
                        | OpMul -> OpMul
                        | OpDiv -> OpDiv
                        | OpEq -> OpEq
                        | OpNe -> OpNe
                        | OpLess -> OpLess
                        | OpGreater -> OpGreater
                        | OpLessEq -> OpLessEq
                        | OpGreaterEq -> OpGreaterEq
                        | OpXor -> OpXor
                        | _ -> failwith "unreachable"
                      in
                      create_let (simplify_argument variables count rhs)
                        (fun rhs count -> (AtmOp (op, lhs, rhs), count))))
      | TmLet { name; value; expression } ->
          let value, count = remove_complex_operands variables count value in
          let variables = StringMap.add name true variables in
          let expression, count =
            remove_complex_operands variables count expression
          in
          (Let (name, value, expression), count)
      | TmIf { condition; if_true; if_false } ->
          create_let (simplify_argument variables count condition)
            (fun cond count ->
              let lhs, count =
                remove_complex_operands variables count if_true
              in
              let rhs, count =
                remove_complex_operands variables count if_false
              in
              (AtmIf (cond, lhs, rhs), count))

    let print_atm atm =
      match atm with
      | AtmVar name -> name
      | AtmInt int -> string_of_int int
      | AtmBool bool -> string_of_bool bool

    let rec print_monadic expr =
      match expr with
      | AtmValue atm -> print_atm atm
      | AtmFunction (name, parameters) ->
          "(" ^ name
          ^ (List.map (fun param -> " " ^ print_atm param) parameters
            |> String.concat "")
          ^ ")"
      | AtmOp (op, lhs, rhs) ->
          "(" ^ print_math_op op ^ " " ^ print_atm lhs ^ " " ^ print_atm rhs
          ^ ")"
      | AtmIf (cond, lhs, rhs) ->
          "(if " ^ print_atm cond ^ " " ^ print_monadic lhs ^ " "
          ^ print_monadic rhs ^ ")"
      | Let (name, value, expression) ->
          "(let [" ^ name ^ " " ^ print_monadic value ^ "] "
          ^ print_monadic expression ^ ")"
      | Sequence (lhs, rhs) ->
          "(begin " ^ print_monadic lhs ^ " " ^ print_monadic rhs ^ ")"
  end

  module ExplicateControl = struct
    open Monadic

    type exp =
      | Atm of atm
      | Function of string * atm list
      | Operator of math_op * atm * atm

    type stmt =
      | Assign of string * exp
      | Return of exp
      | CMov of atm * int
      | Jmp of int
      | Label of int

    let rec explicate_control expr name count =
      let return_expr name expr =
        ( (match name with
          | None -> [ Return expr ]
          | Some name -> [ Assign (name, expr) ]),
          count )
      in
      match expr with
      | AtmValue atm ->
          let expr = Atm atm in
          return_expr name expr
      | AtmFunction (func, args) ->
          let expr = Function (func, args) in
          return_expr name expr
      | AtmOp (op, lhs, rhs) ->
          let expr = Operator (op, lhs, rhs) in
          return_expr name expr
      | AtmIf (cond, lhs, rhs) ->
          let lhs, count = explicate_control lhs name count in
          let rhs, count = explicate_control rhs name count in
          let cond = CMov (cond, count) in
          let inter_jmp = Jmp (count + 1) in
          let inter_label = Label count in
          let end_label = Label (count + 1) in
          ( List.concat
              [ [ cond ]; rhs; [ inter_jmp; inter_label ]; lhs; [ end_label ] ],
            count + 2 )
      | Let (new_name, value, expr) ->
          let value, count = explicate_control value (Some new_name) count in
          let expr, count = explicate_control expr name count in
          (List.append value expr, count)
      | Sequence (lhs, rhs) ->
          let lhs, count = explicate_control lhs None count in
          let rhs, count = explicate_control rhs name count in
          (List.append lhs rhs, count)

    let format_atm atm =
      match atm with
      | AtmVar name -> name
      | AtmInt num -> string_of_int num
      | AtmBool bool -> string_of_bool bool

    let format_exp expr =
      match expr with
      | Atm atm -> format_atm atm
      | Function (name, atms) ->
          Format.sprintf "%s(%s)" name
            (String.concat ", " (List.map format_atm atms))
      | Operator (op, lhs, rhs) ->
          Format.sprintf "%s %s %s" (format_atm lhs) (print_math_op op)
            (format_atm rhs)

    let format_statement stmt =
      match stmt with
      | Assign (name, expr) ->
          Format.sprintf "  %s <- %s" name (format_exp expr)
      | Return expr -> Format.sprintf "  return %s" (format_exp expr)
      | CMov (cond, idx) -> Format.sprintf "  if %s go %d" (format_atm cond) idx
      | Jmp idx -> Format.sprintf "  jmp %d" idx
      | Label idx -> Format.sprintf "label%d:" idx
  end

  module AssignHomes = struct
    open Monadic
    open ExplicateControl
    open Utils

    (* there should be parameters already *)
    let rec analyze_variable_use statements variables =
      let analyze_atm atm variables =
        match atm with
        | AtmVar var ->
            StringMap.update var (Option.map (fun x -> x + 1)) variables
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
              StringMap.update var
                (Option.map (fun x -> x - 1))
                context.variables
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
            |> List.fold_left
                 (fun context atm -> analyze_atm atm context)
                 context
        | Operator (_, lhs, rhs) ->
            [ lhs; rhs ]
            |> List.fold_left
                 (fun context atm -> analyze_atm atm context)
                 context
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
                  stack_table =
                    StringMap.add new_variable index context.stack_table;
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
  end

  module BasicInstructions = struct
    open AssignHomes
    open Monadic
    open Utils
    open ExplicateControl

    type basic_data = BasicReg of reg | BasicInt of int

    type instruction =
      | MovIns of reg * basic_data
      | OpIns of reg * math_op * basic_data * basic_data
      | FuncIns of reg * string * basic_data list
      | CMovIns of basic_data * int
      | JmpIns of int
      | LblIns of int

    let compile_statement statement homes =
      let atm_to_basic_data atm =
        match atm with
        | AtmVar var -> BasicReg (StringMap.find var homes)
        | AtmInt int -> BasicInt int
        | AtmBool true -> BasicInt 1
        | AtmBool false -> BasicInt 0
      in
      let compile_expression return expr =
        match expr with
        | Atm atm -> MovIns (return, atm_to_basic_data atm)
        | Function (name, args) ->
            FuncIns (return, name, List.map atm_to_basic_data args)
        | Operator (op, lhs, rhs) ->
            OpIns (return, op, atm_to_basic_data lhs, atm_to_basic_data rhs)
      in
      match statement with
      | Assign (var, expr) -> compile_expression (StringMap.find var homes) expr
      | Return expr -> compile_expression (Reg "x0") expr
      | CMov (cond, label) -> CMovIns (atm_to_basic_data cond, label)
      | Label label -> LblIns label
      | Jmp label -> JmpIns label
  end

  module AsmGenerator = struct
    module Arm64Darwin = struct
      open Monadic
      open BasicInstructions
      open AssignHomes
      open Utils

      let align16 n = (n + 15) / 16 * 16
      let add_line acc s = acc := !acc ^ s ^ "\n"
      let buffer_reg = "x9"
      let second_buffer_reg = "x10"
      let additional_buffer_reg = "x11"

      (* let temp_regs =
        [ "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25"; "x26"; "x27"; "x28" ] *)
      let temp_regs = []
      let input_regs = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ]
      let stack_position sp_shift index = sp_shift - (index * 8)

      let compile_instruction out function_name sp_shift instruction =
        match instruction with
        | MovIns (dest, data) -> (
            match dest with
            | Reg dest -> (
                match data with
                | BasicInt int ->
                    Format.sprintf "    mov %s, %d" dest int |> add_line out
                | BasicReg (Reg src) ->
                    Format.sprintf "    mov %s, %s" dest src |> add_line out
                | BasicReg (Stack index) ->
                    Format.sprintf "    ldr %s, [sp, #%d]" dest
                      (stack_position sp_shift index)
                    |> add_line out)
            | Stack dest ->
                let src =
                  match data with
                  | BasicInt int ->
                      Format.sprintf "    mov %s, %d" buffer_reg int
                      |> add_line out;
                      buffer_reg
                  | BasicReg (Reg src) -> src
                  | BasicReg (Stack src) ->
                      Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                        (stack_position sp_shift src)
                      |> add_line out;
                      buffer_reg
                in
                Format.sprintf "    str %s, [sp, #%d]" src
                  (stack_position sp_shift dest)
                |> add_line out)
        | OpIns (dest, op, lhs, rhs) -> (
            let lhs =
              match lhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            let rhs =
              match rhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" additional_buffer_reg int
                  |> add_line out;
                  additional_buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" additional_buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  additional_buffer_reg
            in
            let compile_operator =
             fun dest op ->
              match op with
              | OpAdd ->
                  add_line out
                    (Format.sprintf "    add %s, %s, %s" dest lhs rhs)
              | OpSub ->
                  add_line out
                    (Format.sprintf "    sub %s, %s, %s" dest lhs rhs)
              | OpMul ->
                  add_line out
                    (Format.sprintf "    mul %s, %s, %s" dest lhs rhs)
              | OpDiv ->
                  add_line out
                    (Format.sprintf "    sdiv %s, %s, %s" dest lhs rhs)
              | OpXor ->
                  add_line out
                    (Format.sprintf "    eor %s, %s, %s" dest lhs rhs)
              | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
                  add_line out (Format.sprintf "    cmp %s, %s" lhs rhs);
                  let cond =
                    match op with
                    | OpEq -> "eq"
                    | OpNe -> "ne"
                    | OpLess -> "lt"
                    | OpGreater -> "gt"
                    | OpLessEq -> "le"
                    | OpGreaterEq -> "ge"
                    | _ -> assert false
                  in
                  add_line out (Format.sprintf "    cset %s, %s" dest cond)
            in
            match dest with
            | Reg reg -> compile_operator reg op
            | Stack index ->
                compile_operator second_buffer_reg op;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" second_buffer_reg
                     (stack_position sp_shift index)))
        | CMovIns (cond, label) ->
            let cond =
              match cond with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            add_line out (Format.sprintf "    cbnz %s, label_%d" cond label)
        | JmpIns label -> add_line out (Format.sprintf "    b label_%d" label)
        | LblIns label -> add_line out (Format.sprintf "label_%d:" label)
        | FuncIns (dest, name, args) ->
            let load_arg =
             fun shift dest arg ->
              match arg with
              | BasicReg (Reg src) ->
                  Format.sprintf "    mov %s, %s" dest src |> add_line out
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" dest
                    (stack_position (sp_shift + shift) src)
                  |> add_line out
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" dest int |> add_line out
            in
            (* tail recursion *)
            List.take 8 args
            |> List.mapi (fun index arg ->
                load_arg 0 ("x" ^ string_of_int index) arg)
            |> List.fold_left (fun _ _ -> ()) ();
            if dest = Reg "x0" && name = function_name then
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> ( * ) 8 |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                List.mapi
                  (fun index _ ->
                    add_line out
                      (Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                         (index * 8));
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (stack_position (sp_shift + shift) (-1 - index))))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    b start_%s" function_name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    b start_%s" function_name)
            else (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    bl _%s" name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    bl _%s" name);
              match dest with
              | Stack index ->
                  add_line out
                    (Format.sprintf "    str x0, [sp, #%d]"
                       (stack_position sp_shift index))
              | Reg reg -> add_line out (Format.sprintf "    mov %s, x0" reg))

      let compile_function name params instructions (homes : reg StringMap.t) =
        let frame_size =
          StringMap.fold
            (fun _ home max_width ->
              match home with
              | Stack value -> max value max_width
              | _ -> max_width)
            homes 0
          |> ( * ) 8 |> align16
        in
        let out = ref "" in
        add_line out ".text";
        add_line out (".global _" ^ name);
        add_line out ("_" ^ name ^ ":");
        add_line out "    stp fp, lr, [sp, #-16]!";
        add_line out (Format.sprintf "    sub sp, sp, #%d" frame_size);
        add_line out ("start_" ^ name ^ ":");

        List.take 8 params
        |> List.fold_left
             (fun index _ ->
               add_line out
                 (Format.sprintf "    str x%d, [sp, #%d]" index
                    (stack_position frame_size (index + 1)));
               index + 1)
             0
        |> fun _ ->
        ();

        List.map
          (fun instruction ->
            compile_instruction out name frame_size instruction)
          instructions
        |> List.fold_left (fun _ _ -> ()) ();

        add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
        add_line out "    ldp fp, lr, [sp], #16";
        add_line out "    ret";
        !out

      type function_data = Generator.function_data

      let rec compile_functions (functions : function_data list)
          (declarations : string list) (cons : string) =
        match functions with
        | [] -> cons
        | { name; parameters; statements } :: rest ->
            let params_map =
              parameters
              |> List.map (fun param -> (param, 0))
              |> StringMap.of_list
            in
            let variables =
              AssignHomes.analyze_variable_use statements params_map
            in
            let stack_list =
              List.take 8 parameters
              |> List.fold_left_map
                   (fun index name -> (index + 1, (name, index)))
                   1
              |> snd
              |> List.append
                   (List.drop 8 parameters
                   |> List.fold_left_map
                        (fun index name -> (index - 1, (name, index)))
                        (-2)
                   |> snd)
              |> StringMap.of_list
            in
            let homes =
              StringMap.map (fun index -> AssignHomes.Stack index) stack_list
            in
            let homes =
              AssignHomes.assign_homes statements
                {
                  variables;
                  register_table =
                    temp_regs
                    |> List.map (fun reg -> (reg, None))
                    |> StringMap.of_list;
                  stack_table = stack_list;
                  free_stack = [];
                }
                homes
            in
            let instructions =
              List.map
                (fun stmt -> BasicInstructions.compile_statement stmt homes)
                statements
            in
            let cons =
              cons ^ "\n" ^ compile_function name parameters instructions homes
            in
            compile_functions rest declarations cons

      let compile_code functions =
        let declarations =
          functions
          |> List.map (fun (func : function_data) -> func.name)
          |> List.append Generator.builtin_functions
        in
        let builtin =
          "\n\
           .text\n\
           .global _print_bool\n\
           _print_bool:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    sub sp, sp, #16\n\
          \    cmp w0, #0\n\
          \    adrp x0, fmt_false@PAGE\n\
          \    add  x0, x0, fmt_false@PAGEOFF\n\
          \    b.eq 1f\n\
          \    adrp x0, fmt_true@PAGE\n\
          \    add  x0, x0, fmt_true@PAGEOFF\n\
           1:\n\
          \    bl _printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global _print_int\n\
           _print_int:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    mov x19, x0\n\
          \    ADRP X0, fmt_int@PAGE\n\
          \    ADD X0, X0, fmt_int@PAGEOFF\n\
          \    STR x19, [SP, #-16]!\n\
          \    BL  _printf\n\
          \    mov x0, #0\n\
          \    ADD SP, SP, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global _read\n\
           _read:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    adrp x0, fmt_read@PAGE\n\
          \    add x0, x0, fmt_read@PAGEOFF\n\
          \    adrp x11, num@PAGE\n\
          \    add x11, x11, num@PAGEOFF\n\
          \    str x11, [SP, #-16]!\n\
          \    bl _scanf\n\
          \    add sp, sp, #16\n\
          \    adrp x11, num@PAGE\n\
          \    add x11, x11, num@PAGEOFF\n\
          \    ldr x0, [x11]\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\n\
           .data\n\n\
           .balign 4\n\
           fmt_true:\n\
          \    .asciz \"true\\n\"\n\
           .balign 4\n\
           fmt_false:\n\
          \    .asciz \"false\\n\"\n\
           .balign 4\n\
           fmt_read:\n\
          \    .asciz \"%lld\"\n\
           .balign 4\n\
           fmt_int:\n\
          \    .asciz \"%lld\\n\"\n\
           .balign 4\n\
           num:    .quad 0\n\
          \      "
        in
        compile_functions functions declarations builtin
    end

    module Arm64Linux = struct
      open Monadic
      open BasicInstructions
      open AssignHomes
      open Utils

      let align16 n = (n + 15) / 16 * 16
      let add_line acc s = acc := !acc ^ s ^ "\n"
      let buffer_reg = "x9"
      let second_buffer_reg = "x10"
      let additional_buffer_reg = "x11"

      (* let temp_regs =
        [ "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25"; "x26"; "x27"; "x28" ] *)
      let temp_regs = []
      let input_regs = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ]
      let stack_position sp_shift index = sp_shift - (index * 8)

      let compile_instruction out function_name sp_shift instruction =
        match instruction with
        | MovIns (dest, data) -> (
            match dest with
            | Reg dest -> (
                match data with
                | BasicInt int ->
                    Format.sprintf "    mov %s, %d" dest int |> add_line out
                | BasicReg (Reg src) ->
                    Format.sprintf "    mov %s, %s" dest src |> add_line out
                | BasicReg (Stack index) ->
                    Format.sprintf "    ldr %s, [sp, #%d]" dest
                      (stack_position sp_shift index)
                    |> add_line out)
            | Stack dest ->
                let src =
                  match data with
                  | BasicInt int ->
                      Format.sprintf "    mov %s, %d" buffer_reg int
                      |> add_line out;
                      buffer_reg
                  | BasicReg (Reg src) -> src
                  | BasicReg (Stack src) ->
                      Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                        (stack_position sp_shift src)
                      |> add_line out;
                      buffer_reg
                in
                Format.sprintf "    str %s, [sp, #%d]" src
                  (stack_position sp_shift dest)
                |> add_line out)
        | OpIns (dest, op, lhs, rhs) -> (
            let lhs =
              match lhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            let rhs =
              match rhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" additional_buffer_reg int
                  |> add_line out;
                  additional_buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" additional_buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  additional_buffer_reg
            in
            let compile_operator =
             fun dest op ->
              match op with
              | OpAdd ->
                  add_line out
                    (Format.sprintf "    add %s, %s, %s" dest lhs rhs)
              | OpSub ->
                  add_line out
                    (Format.sprintf "    sub %s, %s, %s" dest lhs rhs)
              | OpMul ->
                  add_line out
                    (Format.sprintf "    mul %s, %s, %s" dest lhs rhs)
              | OpDiv ->
                  add_line out
                    (Format.sprintf "    sdiv %s, %s, %s" dest lhs rhs)
              | OpXor ->
                  add_line out
                    (Format.sprintf "    eor %s, %s, %s" dest lhs rhs)
              | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
                  add_line out (Format.sprintf "    cmp %s, %s" lhs rhs);
                  let cond =
                    match op with
                    | OpEq -> "eq"
                    | OpNe -> "ne"
                    | OpLess -> "lt"
                    | OpGreater -> "gt"
                    | OpLessEq -> "le"
                    | OpGreaterEq -> "ge"
                    | _ -> assert false
                  in
                  add_line out (Format.sprintf "    cset %s, %s" dest cond)
            in
            match dest with
            | Reg reg -> compile_operator reg op
            | Stack index ->
                compile_operator second_buffer_reg op;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" second_buffer_reg
                     (stack_position sp_shift index)))
        | CMovIns (cond, label) ->
            let cond =
              match cond with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            add_line out (Format.sprintf "    cbnz %s, label_%d" cond label)
        | JmpIns label -> add_line out (Format.sprintf "    b label_%d" label)
        | LblIns label -> add_line out (Format.sprintf "label_%d:" label)
        | FuncIns (dest, name, args) ->
            let load_arg =
             fun shift dest arg ->
              match arg with
              | BasicReg (Reg src) ->
                  Format.sprintf "    mov %s, %s" dest src |> add_line out
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" dest
                    (stack_position (sp_shift + shift) src)
                  |> add_line out
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" dest int |> add_line out
            in
            (* tail recursion *)
            List.take 8 args
            |> List.mapi (fun index arg ->
                load_arg 0 ("x" ^ string_of_int index) arg)
            |> List.fold_left (fun _ _ -> ()) ();
            if dest = Reg "x0" && name = function_name then (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> ( * ) 8 |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                List.mapi
                  (fun index _ ->
                    add_line out
                      (Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                         (index * 8));
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (stack_position (sp_shift + shift) (-1 - index))))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else ();
              add_line out (Format.sprintf "    b start_%s" function_name))
            else (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    bl %s" name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    bl %s" name);
              match dest with
              | Stack index ->
                  add_line out
                    (Format.sprintf "    str x0, [sp, #%d]"
                       (stack_position sp_shift index))
              | Reg reg -> add_line out (Format.sprintf "    mov %s, x0" reg))

      let compile_function name params instructions (homes : reg StringMap.t) =
        let frame_size =
          StringMap.fold
            (fun _ home max_width ->
              match home with
              | Stack value -> max value max_width
              | _ -> max_width)
            homes 0
          |> ( * ) 8 |> align16
        in
        let out = ref "" in
        add_line out ".text";
        add_line out (".global " ^ name);
        add_line out (name ^ ":");
        add_line out "    stp fp, lr, [sp, #-16]!";
        add_line out ("start_" ^ name ^ ":");
        add_line out (Format.sprintf "    sub sp, sp, #%d" frame_size);

        List.take 8 params
        |> List.fold_left
             (fun index _ ->
               add_line out
                 (Format.sprintf "    str x%d, [sp, #%d]" index
                    (stack_position frame_size (index + 1)));
               index + 1)
             0
        |> fun _ ->
        ();

        List.map
          (fun instruction ->
            compile_instruction out name frame_size instruction)
          instructions
        |> List.fold_left (fun _ _ -> ()) ();

        add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
        add_line out "    ldp fp, lr, [sp], #16";
        add_line out "    ret";
        !out

      type function_data = Generator.function_data

      let rec compile_functions (functions : function_data list)
          (declarations : string list) (cons : string) =
        match functions with
        | [] -> cons
        | { name; parameters; statements } :: rest ->
            let params_map =
              parameters
              |> List.map (fun param -> (param, 0))
              |> StringMap.of_list
            in
            let variables =
              AssignHomes.analyze_variable_use statements params_map
            in
            let stack_list =
              List.take 8 parameters
              |> List.fold_left_map
                   (fun index name -> (index + 1, (name, index)))
                   1
              |> snd
              |> List.append
                   (List.drop 8 parameters
                   |> List.fold_left_map
                        (fun index name -> (index - 1, (name, index)))
                        (-2)
                   |> snd)
              |> StringMap.of_list
            in
            let homes =
              StringMap.map (fun index -> AssignHomes.Stack index) stack_list
            in
            let homes =
              AssignHomes.assign_homes statements
                {
                  variables;
                  register_table =
                    temp_regs
                    |> List.map (fun reg -> (reg, None))
                    |> StringMap.of_list;
                  stack_table = stack_list;
                  free_stack = [];
                }
                homes
            in
            let instructions =
              List.map
                (fun stmt -> BasicInstructions.compile_statement stmt homes)
                statements
            in
            let cons =
              cons ^ "\n" ^ compile_function name parameters instructions homes
            in
            compile_functions rest declarations cons

      let compile_code functions =
        let declarations =
          functions
          |> List.map (fun (func : function_data) -> func.name)
          |> List.append Generator.builtin_functions
        in
        let builtin =
          "\n\
           .text\n\
           .global print_bool\n\
           print_bool:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    sub sp, sp, #16\n\
          \    cmp w0, #0\n\
          \    adr x0, fmt_false\n\
          \    b.eq 1f\n\
          \    adr x0, fmt_true\n\
           1:\n\
          \    bl printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global print_int\n\
           print_int:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    mov x19, x0\n\
          \    adr x0, fmt_int\n\
          \    str x19, [sp, #-16]!\n\
          \    bl  printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global read\n\
           read:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    adr x0, fmt_read\n\
          \    adr x11, num\n\
          \    str x11, [SP, #-16]!\n\
          \    bl scanf\n\
          \    add sp, sp, #16\n\
          \    adr x11, num\n\
          \    ldr x0, [x11]\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\n\
           .data\n\n\
           .balign 4\n\
           fmt_true:\n\
          \    .asciz \"true\\n\"\n\
           .balign 4\n\
           fmt_false:\n\
          \    .asciz \"false\\n\"\n\
           .balign 4\n\
           fmt_read:\n\
          \    .asciz \"%lld\"\n\
           .balign 4\n\
           fmt_int:\n\
          \    .asciz \"%lld\\n\"\n\
           .balign 4\n\
           num:    .quad 0\n\
          \      "
        in
        compile_functions functions declarations builtin
    end

    type function_data = {
      name : string;
      parameters : string list;
      statements : ExplicateControl.stmt list;
    }

    let builtin_functions = [ "read"; "print_int"; "print_bool" ]
  end

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
  open Parser.Ast

  let compile (verbose : bool) (target : Config.target_architecture)
      (functions : Parser.Ast.implementation list) =
    let declarations =
      functions
      |> List.map (fun (func : Parser.Ast.implementation) -> func.name)
      |> List.append AsmGenerator.builtin_functions
    in
    let compile_function verbose (func : Parser.Ast.implementation) :
        AsmGenerator.function_data =
      if verbose then (
        print_endline "function:";
        print_endline (Parser.Ast.print_ast func));
      let { name; parameters; expression } = func in
      let params_map =
        List.append declarations parameters
        |> List.map (fun name -> (name, name))
        |> StringMap.of_list
      in
      let expr, count = expression |> Uniquify.uniquify_expr 0 params_map in
      if verbose then (
        print_endline "uniquified expression:";
        print_endline (Parser.Ast.print_ast_expr expr));
      let params_map =
        parameters |> List.map (fun param -> (param, true)) |> StringMap.of_list
      in
      let expr, count = Monadic.remove_complex_operands params_map count expr in

      if verbose then (
        print_endline "monadized expression:";
        print_endline (Monadic.print_monadic expr));
      let statements, _ = ExplicateControl.explicate_control expr None count in
      { name; parameters; statements }
    in
    let functions = List.map (compile_function verbose) functions in
    match target with
    | Config.Arm64Darwin -> AsmGenerator.Arm64Darwin.compile_code functions
    | Config.Arm64Linux -> AsmGenerator.Arm64Linux.compile_code functions
end
