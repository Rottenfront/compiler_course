open Lexer

type substring = { str : string; position : position }
type literal = LitNumber of int | LitBool of bool

let print_literal literal =
  match literal with
  | LitNumber number -> Printf.sprintf "%d" number
  | LitBool true -> "true"
  | LitBool false -> "false"

type operator =
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

type type_expr =
  | TyInt of position
  | TyBool of position
  | TyUnit of position
  | TyFunc of position * type_expr list * type_expr

let rec equal_type lhs rhs =
  match lhs with
  | TyInt _ -> ( match rhs with TyInt _ -> true | _ -> false)
  | TyBool _ -> ( match rhs with TyBool _ -> true | _ -> false)
  | TyUnit _ -> ( match rhs with TyUnit _ -> true | _ -> false)
  | TyFunc (_, types, res) -> (
      match rhs with
      | TyFunc (_, r_types, r_res) ->
          List.combine types r_types
          |> List.for_all (fun (lhs, rhs) -> equal_type lhs rhs)
          && equal_type res r_res
      | _ -> false)

type expr =
  | TmLiteral of position * literal
  | TmApplication of position * application_info
  | TmOpApp of position * operator_info
  | TmLet of position * var_info * expr
  | TmIf of position * expr * expr * expr
  | TmParenth of expr

and application_info = { name : substring; arguments : expr list }
and var_info = { name : substring; value : expr }
and operator_info = { lhs : expr; operator : operator; rhs : expr }

type parse_error =
  | ConditionExpressionExpected of position
  | ThenTokenExpected of lex_token
  | ElseTokenExpected of lex_token
  | InTokenExpected of lex_token
  | SetTokenExpected of lex_token
  | ArrowTokenExpected of lex_token
  | IdentTokenExpected of lex_token
  | ParenCloseExpected of lex_token
  | DoubleColonExpected of lex_token
  | StatementExpected of lex_token
  | CannotUseConditionInFunction of position
  | CannotDefineVariableInFunction of position
  | UnknownOperator of substring
  | ExpectedExpressionAfterOperator of position
  | UnknownType of substring
  | FunctionNameExpected of lex_token
  | UnexpectedEnd of char_position
  | NoPossibleExpression
  | NoPossibleType

let print_error error =
  match error with
  | ConditionExpressionExpected pos ->
      Printf.sprintf "Condition expression expected on %s" (print_position pos)
  | ThenTokenExpected tok ->
      Printf.sprintf "`then` token expected, got: `%s`" (print_token tok)
  | ElseTokenExpected tok ->
      Printf.sprintf "`else` token expected, got: `%s`" (print_token tok)
  | InTokenExpected tok ->
      Printf.sprintf "`in` token expected, got: `%s`" (print_token tok)
  | SetTokenExpected tok ->
      Printf.sprintf "`=` token expected, got: `%s`" (print_token tok)
  | ArrowTokenExpected tok ->
      Printf.sprintf "`->` token expected, got: `%s`" (print_token tok)
  | IdentTokenExpected tok ->
      Printf.sprintf "identifier token expected, got: `%s`" (print_token tok)
  | ParenCloseExpected tok ->
      Printf.sprintf "`)` token expected, got: `%s`" (print_token tok)
  | DoubleColonExpected tok ->
      Printf.sprintf "`::` token expected, got: `%s`" (print_token tok)
  | StatementExpected tok ->
      Printf.sprintf "`impl` of `decl` expected, got: `%s`" (print_token tok)
  | CannotUseConditionInFunction pos ->
      Printf.sprintf
        "Cannot use condition expression inside as an argument without \
         paretheses (%s)"
        (print_position pos)
  | CannotDefineVariableInFunction pos ->
      Printf.sprintf
        "Cannot define variables inside as an argument without paretheses (%s)"
        (print_position pos)
  | UnknownOperator { str; position } ->
      Printf.sprintf "Unknown operator `%s` used on %s" str
        (print_position position)
  | ExpectedExpressionAfterOperator pos ->
      Printf.sprintf "Expected expression after operator on %s"
        (print_position pos)
  | UnknownType { str; position } ->
      Printf.sprintf "Unknown type `%s` used on %s" str
        (print_position position)
  | FunctionNameExpected tok ->
      Printf.sprintf "`then` token expected, got: `%s`" (print_token tok)
  | UnexpectedEnd { line; char } ->
      Printf.sprintf "Unexpected end on %d:%d" line char
  | NoPossibleExpression -> "No possible expression"
  | NoPossibleType -> "No possible type"

type declaration = { position : position; name : substring; type_ : type_expr }

type implementation = {
  position : position;
  name : substring;
  parameters : substring list;
  expression : expr;
}

let default_char_pos : char_position = { line = 0; char = 0 }
let default_position : position = (default_char_pos, default_char_pos)

type 'a parser = lex_token list -> ('a, parse_error) result * lex_token list

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
  match p tokens with Ok ok, cs -> (Ok ok, cs) | Error _, _ -> q tokens

let ( <|> ) = or_else
let run (p : 'a parser) (tokens : lex_token list) = p tokens

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

let error_then_expected tok : 'a parser =
 fun rest -> (Error (ThenTokenExpected tok), rest)

let error_function_name_expected tok : 'a parser =
 fun rest -> (Error (FunctionNameExpected tok), rest)

let error_paren_close_expected tok : 'a parser =
 fun rest -> (Error (ParenCloseExpected tok), rest)

let error_double_colon_expected tok : 'a parser =
 fun rest -> (Error (DoubleColonExpected tok), rest)

let error_statement_expected tok : 'a parser =
 fun rest -> (Error (StatementExpected tok), rest)

let error_cannot_use_condition pos : 'a parser =
 fun rest -> (Error (CannotUseConditionInFunction pos), rest)

let error_cannot_define_variable pos : 'a parser =
 fun rest -> (Error (CannotDefineVariableInFunction pos), rest)

let error_unknown_operator str : 'a parser =
 fun rest -> (Error (UnknownOperator str), rest)

let error_unknown_type str : 'a parser =
 fun rest -> (Error (UnknownType str), rest)

let error_no_possible_expression : 'a parser =
 fun rest -> (Error NoPossibleExpression, rest)

let error_no_possible_type : 'a parser = fun rest -> (Error NoPossibleType, rest)

let error_expected_expression_after_operator pos : 'a parser =
 fun rest -> (Error (ExpectedExpressionAfterOperator pos), rest)

let extend_span ((left, _) : position) ((_, right) : position) : position =
  (left, right)

let rec get_position expr =
  match expr with
  | TmLiteral (position, _) -> position
  | TmApplication (position, _) -> position
  | TmLet (position, _, _) -> position
  | TmOpApp (position, _) -> position
  | TmIf (position, _, _, _) -> position
  | TmParenth expr -> get_position expr

let type_position type_ =
  match type_ with
  | TyBool pos -> pos
  | TyInt pos -> pos
  | TyUnit pos -> pos
  | TyFunc (pos, _, _) -> pos

(* Generic token expectation:
   pred: token -> bool
   on_empty: parse_error  (what to return if token list is empty)
   on_unexpected: token -> parse_error (how to convert a wrong token into an error)
*)
let expect_token ~pred ~on_empty ~on_unexpected : lex_token parser =
 fun tokens ->
  match tokens with
  | tok :: rest when pred tok -> (Ok tok, rest)
  | [] -> on_empty []
  | tok :: rest -> on_unexpected tok rest

(* Expect an identifier token and return (pos, string) like your old code did *)
let expect_ident ~on_empty ~on_unexpected : substring parser =
 fun tokens ->
  match tokens with
  | TkIdent (position, str) :: rest -> (Ok { str; position }, rest)
  | [] -> on_empty []
  | tok :: rest -> on_unexpected tok rest

let rec parse_expr tokens in_function =
  let parse_if (position : position) : expr parser =
    let is_then = function TkThen _ -> true | _ -> false in
    let is_else = function TkElse _ -> true | _ -> false in

    let* cond = parse_required_expr ~position in

    let* then_tok =
      expect_token ~pred:is_then
        ~on_empty:(error_unexpected_end (snd position))
        ~on_unexpected:error_then_expected
    in

    let* lhs = parse_required_expr ~position:(token_position then_tok) in

    let position_after_lhs = get_position lhs in

    let* else_tok =
      expect_token ~pred:is_else
        ~on_empty:(get_position lhs |> snd |> error_unexpected_end)
        ~on_unexpected:error_else_expected
    in

    match else_tok with
    | TkElse tok_pos ->
        let position_after_in = extend_span position_after_lhs tok_pos in
        let* rhs = parse_required_expr ~position:position_after_in in
        let let_span = get_position rhs |> extend_span position_after_in in
        return (TmIf (let_span, cond, lhs, rhs))
    | tok -> error_else_expected tok
  in

  let parse_let (position : position) : expr parser =
    let is_set = function TkSet _ -> true | _ -> false in
    let is_in = function TkIn _ -> true | _ -> false in

    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end (snd position))
        ~on_unexpected:error_ident_expected
    in

    let* _set =
      expect_token ~pred:is_set
        ~on_empty:(error_unexpected_end (snd name.position))
        ~on_unexpected:error_set_expected
    in

    let* value = parse_required_expr ~position in

    let position_after_value = get_position value in

    let* in_tok =
      expect_token ~pred:is_in
        ~on_empty:(get_position value |> snd |> error_unexpected_end)
        ~on_unexpected:error_in_expected
    in

    match in_tok with
    | TkIn tok_pos ->
        let position_after_in = extend_span position_after_value tok_pos in
        let* expr = parse_required_expr ~position:position_after_in in
        let let_span = get_position expr |> extend_span position in
        return (TmLet (let_span, { name; value }, expr))
    | tok -> error_in_expected tok
  in

  let parse_parenth (position : position) : expr parser =
    let is_parenth_close = function TkParenClose _ -> true | _ -> false in
    let* inner = parse_required_expr ~position in
    let* close_parenth =
      expect_token ~pred:is_parenth_close
        ~on_empty:(get_position inner |> snd |> error_unexpected_end)
        ~on_unexpected:error_in_expected
    in

    match close_parenth with
    | TkParenClose _ -> return (TmParenth inner)
    | tok -> error_in_expected tok
  in

  let parse_operator (lhs : (expr, parse_error) result) : expr parser =
    let parse_right_side (lhs : expr) operator position =
      let* rhs =
       fun tokens ->
        let res, rest = parse_expr tokens false in
        match res with
        | Ok expr -> (Ok expr, rest)
        | Error NoPossibleExpression ->
            (Error (ExpectedExpressionAfterOperator position), rest)
        | Error err -> (Error err, rest)
      in
      return
        (TmOpApp
           ( extend_span (get_position lhs) (get_position rhs),
             { lhs; operator; rhs } ))
    in
    match lhs with
    | Error err -> fail err
    | Ok lhs -> (
        fun tokens ->
          match tokens with
          | TkOperator (pos, operator) :: rest -> (
              let operator =
                match operator with
                | "+" -> Ok (OpAdd, pos)
                | "-" -> Ok (OpSub, pos)
                | "*" -> Ok (OpMul, pos)
                | "/" -> Ok (OpDiv, pos)
                | "==" -> Ok (OpEq, pos)
                | "!=" -> Ok (OpNe, pos)
                | "<" -> Ok (OpLess, pos)
                | ">" -> Ok (OpGreater, pos)
                | "<=" -> Ok (OpLessEq, pos)
                | ">=" -> Ok (OpGreaterEq, pos)
                | "||" -> Ok (OpOr, pos)
                | "&&" -> Ok (OpAnd, pos)
                | "^" -> Ok (OpXor, pos)
                | ";" -> Ok (OpSemicolon, pos)
                | other ->
                    Error (UnknownOperator { str = other; position = pos })
              in
              match operator with
              | Ok (operator, position) ->
                  run (parse_right_side lhs operator position) rest
              | Error err -> fail err tokens)
          | rest -> return lhs rest)
  in

  let rec parse_function (name : substring) (arguments : expr list)
      (position : position) : expr parser =
    let* argument = parse_nonrequired_expr in
    match argument with
    | Some new_arg ->
        parse_function name (new_arg :: arguments) (get_position new_arg)
    | None ->
        return
          (TmApplication
             (extend_span name.position position, { name; arguments }))
  in

  let lhs, rest =
    match tokens with
    | TkIf pos :: rest ->
        if in_function then error_cannot_use_condition pos rest
        else run (parse_if pos) rest
    | TkLet pos :: rest ->
        if in_function then error_cannot_define_variable pos rest
        else run (parse_let pos) rest
    | TkParenOpen pos :: rest -> run (parse_parenth pos) rest
    | TkIdent (pos, ident) :: rest ->
        if in_function then
          ( Ok
              (TmApplication
                 ( pos,
                   { name = { str = ident; position = pos }; arguments = [] } )),
            rest )
        else run (parse_function { str = ident; position = pos } [] pos) rest
    | TkNumber (pos, value) :: rest ->
        (Ok (TmLiteral (pos, LitNumber value)), rest)
    | TkTrue pos :: rest -> (Ok (TmLiteral (pos, LitBool true)), rest)
    | TkFalse pos :: rest -> (Ok (TmLiteral (pos, LitBool false)), rest)
    | rest -> (Error NoPossibleExpression, rest)
  in
  if in_function then (lhs, rest) else run (parse_operator lhs) rest

and parse_required_expr ~position : expr parser =
 fun tokens ->
  let res, rest = parse_expr tokens false in
  match res with
  | Ok expr -> return expr rest
  | Error NoPossibleExpression -> fail (UnexpectedEnd (snd position)) rest
  | Error err -> fail err rest

and parse_nonrequired_expr : expr option parser =
 fun tokens ->
  let res, rest = parse_expr tokens false in
  match res with
  | Ok expr -> return (Some expr) rest
  | Error NoPossibleExpression -> return None tokens
  | Error err -> fail err rest

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
  match expr with
  | TmOpApp (_, { lhs; operator; rhs }) ->
      let lhs_first, lhs_next = flatten_op_app lhs in
      let rhs_first, rhs_next = flatten_op_app rhs in
      (lhs_first, List.append lhs_next ((operator, rhs_first) :: rhs_next))
  | other -> (other, [])

let rec distribute_operator expr =
  let rec distribute_level lhs rhs level =
    if level = 0 then lhs
      (* there is no way for rhs to have any more elements *)
    else
      let rec split_by_level lhs prev_rhs next_rhs level =
        match next_rhs with
        | [] -> distribute_level lhs (List.rev prev_rhs) (level - 1)
        | (operator, expr) :: rest ->
            if operator_level operator = level then
              let l = distribute_level lhs (List.rev prev_rhs) (level - 1) in
              let r = split_by_level expr [] rest level in
              let position = extend_span (get_position l) (get_position r) in
              TmOpApp (position, { lhs = l; operator; rhs = r })
            else split_by_level lhs ((operator, expr) :: prev_rhs) rest level
      in
      split_by_level lhs [] rhs level
  in
  match expr with
  | TmOpApp (pos, op_app) ->
      let lhs, rhs = flatten_op_app (TmOpApp (pos, op_app)) in
      let lhs' = distribute_operator lhs in
      let rhs' =
        List.map
          (fun (operator, expr) -> (operator, distribute_operator expr))
          rhs
      in
      distribute_level lhs' rhs' max_level
  | TmApplication (pos, { name; arguments }) ->
      TmApplication
        (pos, { name; arguments = List.map distribute_operator arguments })
  | TmIf (pos, cond, lhs, rhs) ->
      TmIf
        ( pos,
          distribute_operator cond,
          distribute_operator lhs,
          distribute_operator rhs )
  | TmLet (pos, { name; value }, expr) ->
      TmLet
        ( pos,
          { name; value = distribute_operator value },
          distribute_operator expr )
  | TmParenth expr -> distribute_operator expr
  | other -> other

let rec parse_type (position : position) (prev : type_expr option) :
    type_expr parser =
  let parse_after_type prev =
   fun tokens ->
    match tokens with
    | TkArrow position :: rest -> parse_type position prev rest
    | rest -> (
        match prev with
        | Some ty -> return ty rest
        | None -> error_no_possible_type rest)
  in
  let parse_parentheses (position : position) : type_expr parser =
    let parse_nonrequired_type (position : position) : type_expr option parser =
     fun tokens ->
      let res, rest = parse_type position None tokens in
      match res with
      | Ok expr -> return (Some expr) rest
      | Error NoPossibleType -> return None tokens
      | Error err -> fail err rest
    in
    let* _open =
      expect_token
        ~pred:(function TkParenOpen _ -> true | _ -> false)
        ~on_unexpected:(fun _ -> error_no_possible_type)
        ~on_empty:(error_unexpected_end (snd position))
    in
    let* expr = parse_nonrequired_type (token_position _open) in
    let* close =
      expect_token
        ~pred:(function TkParenClose _ -> true | _ -> false)
        ~on_unexpected:error_paren_close_expected
        ~on_empty:
          ((match expr with
           | Some expr -> type_position expr
           | _ -> token_position _open)
          |> snd |> error_unexpected_end)
    in
    return
      (match expr with
      | Some typ -> typ
      | None ->
          TyUnit (extend_span (token_position _open) (token_position close)))
  in
  let parse_basic_type (position : position) =
    let* { str = name; position = pos } =
      expect_ident
        ~on_unexpected:(fun _ -> error_no_possible_type)
        ~on_empty:(position |> snd |> error_unexpected_end)
    in
    match name with
    | "int" -> return (TyInt pos)
    | "bool" -> return (TyBool pos)
    | _ -> error_unknown_type { str = name; position = pos }
  in
  fun tokens ->
    match
      or_else (parse_basic_type position) (parse_parentheses position) tokens
    with
    | Ok ty, rest ->
        let next =
          match prev with
          | None -> ty
          | Some (TyFunc (pos, args, res)) ->
              TyFunc
                ( type_position ty |> extend_span pos,
                  List.append args [ res ],
                  ty )
          | Some prev ->
              TyFunc
                ( extend_span (type_position prev) (type_position ty),
                  [ prev ],
                  ty )
        in
        parse_after_type (Some next) rest
    | Error err, rest -> fail err rest

let parse_decl position =
  let* name =
    expect_ident
      ~on_empty:(snd position |> error_unexpected_end)
      ~on_unexpected:error_function_name_expected
  in
  let* colon =
    expect_token
      ~pred:(function TkDoubleColon _ -> true | _ -> false)
      ~on_empty:(snd position |> error_unexpected_end)
      ~on_unexpected:error_double_colon_expected
  in
  let* type_ = parse_type (token_position colon) None in
  return { position = extend_span position (type_position type_); name; type_ }

let parse_impl position =
  let rec parse_names (parameters : substring list) (position : position) :
      substring list parser =
   fun tokens ->
    match tokens with
    | TkIdent (pos, name) :: rest ->
        parse_names ({ str = name; position = pos } :: parameters) pos rest
    | [] -> fail (UnexpectedEnd (snd position)) []
    | TkSet _ :: rest -> return (List.rev parameters) rest
    | tok :: rest -> fail (SetTokenExpected tok) rest
  in
  let* { str = name; position = name_pos } =
    expect_ident
      ~on_empty:(error_unexpected_end (snd position))
      ~on_unexpected:error_function_name_expected
  in
  let* parameters = parse_names [] name_pos in
  let* expression = fun tokens -> parse_expr tokens false in
  let expression' = distribute_operator expression in
  return
    {
      position = extend_span position (get_position expression');
      name = { str = name; position };
      parameters;
      expression = expression';
    }

let rec parse_stmts prev_decls prev_impls =
 fun tokens ->
  match tokens with
  | [] -> return (prev_decls, prev_impls) []
  | TkImpl position :: rest -> (
      let next, cs = parse_impl position rest in
      match next with
      | Ok next -> parse_stmts prev_decls (next :: prev_impls) cs
      | Error err -> fail err cs)
  | TkDecl position :: rest -> (
      let next, cs = parse_decl position rest in
      match next with
      | Ok next -> parse_stmts (next :: prev_decls) prev_impls cs
      | Error err -> fail err cs)
  | tok :: rest -> error_statement_expected tok rest
