open Lexer

type substring = { str : string; position : position }
type literal = LitNumber of int | LitBool of bool

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
  | UnexpectedTokenInLet of lex_token
  | ConditionExpressionExpected of position
  | ThenTokenExpected of lex_token
  | ElseTokenExpected of lex_token
  | CannotUseConditionInFunction of position
  | CannotDefineVariableInFunction of position
  | UnknownOperator of substring
  | ExpectedExpressionAfterOperator of position
  | UnknownType of substring
  | UnexpectedEnd of char_position
  | NoPossibleExpression of lex_token

type declaration = { position : position; name : substring; type_ : type_expr }

type implementation = {
  position : position;
  name : substring;
  parameters : substring list;
  expression : expr;
}

type statement = StmtDecl of declaration | StmtImpl of implementation

let default_char_pos : char_position = { line = 0; char = 0 }
let default_position : position = (default_char_pos, default_char_pos)

type 'a parser = lex_token list -> ('a, parse_error) result * lex_token list

let return (x : 'a) : 'a parser = fun tokens -> (Ok x, tokens)

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun tokens ->
  match p tokens with Ok v, rest -> f v rest | Error e, rest -> (Error e, rest)

let fail (err : parse_error) : 'a parser = fun rest -> (Error err, rest)
let ( let* ) = bind
let ( let* ) = bind

let map p f =
 fun tokens ->
  match p tokens with Ok (v, rest) -> Ok (f v, rest) | Error e -> Error e

let or_else p q tokens =
  match p tokens with Ok _ as ok -> ok | Error _ -> q tokens

let ( <|> ) = or_else
let run (p : 'a parser) (tokens : lex_token list) = p tokens

let error_unexpected_end pos : 'a parser =
 fun rest -> (Error (UnexpectedEnd pos), rest)

let error_unexpected_token_let tok : 'a parser =
 fun rest -> (Error (UnexpectedTokenInLet tok), rest)

let error_condition_expected pos : 'a parser =
 fun rest -> (Error (ConditionExpressionExpected pos), rest)

let error_else_expected tok : 'a parser =
 fun rest -> (Error (ElseTokenExpected tok), rest)

let error_then_expected tok : 'a parser =
 fun rest -> (Error (ThenTokenExpected tok), rest)

let error_cannot_use_condition pos : 'a parser =
 fun rest -> (Error (CannotUseConditionInFunction pos), rest)

let error_cannot_define_variable pos : 'a parser =
 fun rest -> (Error (CannotDefineVariableInFunction pos), rest)

let error_unknown_operator str : 'a parser =
 fun rest -> (Error (UnknownOperator str), rest)

let error_unknown_type str : 'a parser =
 fun rest -> (Error (UnknownType str), rest)

let error_no_possible_expression tok : 'a parser =
 fun rest -> (Error (NoPossibleExpression tok), rest)

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

let rec parse_expr position tokens in_function =
  let parse_required_expr ~position : expr parser =
   fun tokens ->
    let res, rest = parse_expr position tokens false in
    match res with
    | Ok expr -> (Ok expr, rest)
    | Error (NoPossibleExpression _) ->
        (Error (UnexpectedEnd (snd position)), rest)
    | Error err -> (Error err, rest)
  in

  let parse_let (position : position) : expr parser =
    (* small predicate helpers *)
    let is_set = function TkSet _ -> true | _ -> false in
    let is_in = function TkIn _ -> true | _ -> false in

    (* adapter for parse_expr that maps NoPossibleExpression -> UnexpectedEnd(snd pos) *)
    let parse_expr_at_pos pos = parse_required_expr ~position:pos in

    (* the parser pipeline: *)
    let* name =
      expect_ident
        ~on_empty:(error_unexpected_end (snd position))
        ~on_unexpected:error_unexpected_token_let
    in

    let* _set =
      expect_token ~pred:is_set
        ~on_empty:(error_unexpected_end (snd name.position))
        ~on_unexpected:error_unexpected_token_let
    in

    (* parse the value expression; if parse_expr says NoPossibleExpression we translate it
     into UnexpectedEnd (snd position) — same behaviour as your original code *)
    let* value = parse_expr_at_pos position in

    (* update local span with the value's span *)
    let position_after_value = get_position value in

    (* expect `in` token and get its position *)
    let* in_tok =
      expect_token ~pred:is_in
        ~on_empty:(get_position value |> snd |> error_unexpected_end)
        ~on_unexpected:error_unexpected_token_let
    in

    match in_tok with
    | TkIn tok_pos ->
        let position_after_in = extend_span position_after_value tok_pos in
        let* expr = parse_expr_at_pos position_after_in in
        let let_span = get_position expr |> extend_span position_after_in in
        return (TmLet (let_span, { name; value }, expr))
    | tok -> error_unexpected_token_let tok
  in
  run (parse_let position) tokens

let rec parse_expr tokens is_in_application =
  let rec parse_application name position arguments tokens =
    match parse_expr tokens true with
    | Ok expr, rest ->
        parse_application name
          (extend_span position (get_position expr))
          (expr :: arguments) rest
    | Error (NoPossibleExpression _), rest ->
        ( Ok (TmApplication (position, { name; arguments = List.rev arguments })),
          rest )
    | Error err, rest -> (Error err, rest)
  in
  let parse_let tokens position =
    match tokens with
    | TkIdent (pos, ident) :: rest -> (
        let name = { str = ident; position = pos } in
        match rest with
        | TkSet _ :: rest' -> (
            match parse_expr rest' false with
            | Ok expr, cs -> (
                let value, position, cs =
                  (expr, extend_span position (get_position expr), cs)
                in
                match cs with
                | TkIn tok_pos :: cs' -> (
                    let position = extend_span position tok_pos in
                    match parse_expr cs' false with
                    | Ok expr, cs'' ->
                        ( Ok
                            (TmLet
                               ( get_position expr |> extend_span position,
                                 { name; value },
                                 expr )),
                          cs'' )
                    | Error (NoPossibleExpression _), rest ->
                        (Error (UnexpectedEnd (snd position)), rest)
                    | Error err, rest -> (Error err, rest))
                | [] -> (Error (UnexpectedEnd (snd position)), [])
                | tok :: rest -> (Error (UnexpectedTokenInLet tok), rest))
            | Error (NoPossibleExpression _), rest ->
                (Error (UnexpectedEnd (snd position)), rest)
            | Error err, rest -> (Error err, rest))
        | [] -> (Error (UnexpectedEnd (snd position)), [])
        | tok :: rest -> (Error (UnexpectedTokenInLet tok), rest))
    | [] -> (Error (UnexpectedEnd (snd position)), [])
    | tok :: rest -> (Error (UnexpectedTokenInLet tok), rest)
  in
  let parse_if tokens position =
    let cond, position, rest =
      match parse_expr tokens false with
      | Some cond, rest -> (cond, extend_span position (get_position cond), rest)
      | None, _ ->
          failwith
            (Printf.sprintf "Condition expression expected after if on %s"
               (print_position position))
    in
    let position, rest' =
      match rest with
      | TkThen pos :: rest' -> (extend_span position pos, rest')
      | [] ->
          failwith
            (Printf.sprintf "Unexpected end after condition expression (%s)"
               (print_position position))
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token after condition on %s"
               (print_position (token_position tok)))
    in
    let if_true, position, cs =
      match parse_expr rest' false with
      | Some expr, cs -> (expr, extend_span position (get_position expr), cs)
      | _ ->
          failwith
            (Printf.sprintf "Unexpected end in true branch (%s)"
               (print_position position))
    in
    let position, cs' =
      match cs with
      | TkElse pos :: cs' -> (extend_span position pos, cs')
      | [] ->
          failwith
            (Printf.sprintf "Unexpected end after true branch expression (%s)"
               (print_position position))
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token after true branch on %s"
               (print_position (token_position tok)))
    in
    match parse_expr cs' false with
    | Some if_false, cs'' ->
        ( Some
            (TmIf
               ( extend_span position (get_position if_false),
                 cond,
                 if_true,
                 if_false )),
          cs'' )
    | None, _ ->
        failwith
          (Printf.sprintf "Unexpected end in false branch after %s"
             (print_position position))
  in
  let expr, rest =
    match tokens with
    | TkIdent (pos, ident) :: rest ->
        if is_in_application then
          ( Some
              (TmApplication
                 ( pos,
                   { name = { str = ident; position = pos }; arguments = [] } )),
            rest )
        else parse_application { str = ident; position = pos } pos [] rest
    | TkNumber (pos, number) :: rest ->
        (Some (TmLiteral (pos, LitNumber number)), rest)
    | TkTrue pos :: rest -> (Some (TmLiteral (pos, LitBool true)), rest)
    | TkFalse pos :: rest -> (Some (TmLiteral (pos, LitBool false)), rest)
    | TkParenOpen pos :: rest -> (
        match parse_expr rest false with
        | expr, TkParenClose _ :: cs -> (expr, cs)
        | _, _ ->
            failwith
              (Printf.sprintf "Unclosed parenthesis on %s" (print_position pos))
        )
    | TkIf pos :: rest ->
        if is_in_application then
          failwith
            (Printf.sprintf
               "Cannot use branching inside of function application (%s)"
               (print_position pos))
        else parse_if rest pos
    | TkLet pos :: rest ->
        if is_in_application then
          failwith
            (Printf.sprintf
               "Cannot define new variables inside of function application (%s)"
               (print_position pos))
        else parse_let rest pos
    | rest -> (None, rest)
  in
  if is_in_application then (expr, rest)
  else
    match expr with
    | Some lhs -> (
        let parse_rhs lhs rest operator position =
          match parse_expr rest false with
          | Some rhs, cs ->
              ( Some
                  (TmOpApp
                     ( extend_span (get_position lhs) (get_position rhs),
                       { lhs; rhs; operator } )),
                cs )
          | None, _ ->
              failwith
                (Printf.sprintf "No expression after operator on: %s"
                   (print_position position))
        in
        match rest with
        | TkAnd pos :: cs -> parse_rhs lhs cs OpAnd pos
        | TkOr pos :: cs -> parse_rhs lhs cs OpOr pos
        | TkSet pos :: cs -> parse_rhs lhs cs OpEq pos
        | TkOperator (pos, op_token) :: cs ->
            let operator =
              match op_token with
              | "+" -> OpAdd
              | "-" -> OpSub
              | "*" -> OpMul
              | "/" -> OpDiv
              | "!=" -> OpNe
              | "<" -> OpLess
              | ">" -> OpGreater
              | "<=" -> OpLessEq
              | ">=" -> OpGreaterEq
              | "||" -> OpOr
              | "&&" -> OpAnd
              | "^" -> OpXor
              | ";" -> OpSemicolon
              | _ -> failwith (Printf.sprintf "Unknown operator: %s" op_token)
            in
            parse_rhs lhs cs operator pos
        | _ -> (Some lhs, rest))
    | None -> (None, rest)

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

let parse_type tokens =
  let rec parse_types types pos =
    match types with
    | [] -> TyUnit pos
    | type' :: [] -> type'
    | type' :: rest ->
        let next, res =
          match parse_types rest pos with
          | TyFunc (_, next, res) -> (next, res)
          | other -> ([], other)
        in
        TyFunc (pos, type' :: next, res)
  in
  let rec aux tokens is_between =
    if is_between then
      match tokens with
      | TkArrow _ :: rest -> aux rest false
      | rest -> ([], rest)
    else
      match tokens with
      | TkIdent (pos, name) :: rest ->
          let next_types, cs = aux rest true in
          ( (match name with
            | "int" -> TyInt pos
            | "bool" -> TyBool pos
            | _ ->
                failwith
                  (Printf.sprintf "Unknown type `%s` on %s" name
                     (print_position pos)))
            :: next_types,
            cs )
      | TkParenOpen pos1 :: rest ->
          let types, cs = aux rest true in
          let pos2, cs' =
            match cs with
            | TkParenClose pos2 :: cs' -> (pos2, cs')
            | [] -> failwith "unexpected end in type declaration"
            | tok :: _ ->
                failwith
                  (Printf.sprintf "Unexpected token on %s"
                     (print_position (token_position tok)))
          in
          let next_types, cs'' = aux cs' true in
          (parse_types types (extend_span pos1 pos2) :: next_types, cs'')
      | [] -> failwith "unexpected end in type declaration"
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token on %s"
               (print_position (token_position tok)))
  in
  let types, rest = aux tokens false in
  let heading_position =
    match types with
    | head :: _ -> type_position head
    | _ -> failwith "unreachable"
  in
  let position =
    types |> List.map type_position
    |> List.fold_left extend_span heading_position
  in
  (parse_types types position, rest)

let parse_decl tokens =
  let name, cs =
    match tokens with
    | TkIdent (position, name) :: cs -> ({ str = name; position }, cs)
    | [] -> failwith "Unexpected end in function declaration"
    | tok :: _ ->
        failwith
          (Printf.sprintf "Unexpected token on %s"
             (print_position (token_position tok)))
  in
  let cs' =
    match cs with
    | TkDoubleColon _ :: cs' -> cs'
    | [] -> failwith "Unexpected end in function declaration"
    | tok :: _ ->
        failwith
          (Printf.sprintf "Unexpected token on %s"
             (print_position (token_position tok)))
  in
  let type_, cs'' = parse_type cs' in
  ({ position = type_position type_; name; type_ }, cs'')

let parse_impl tokens =
  let rec parse_names tokens =
    match tokens with
    | TkIdent (position, name) :: rest ->
        let next, cs = parse_names rest in
        ({ str = name; position } :: next, cs)
    | [] -> failwith "Unexpected end in function declaration"
    | TkSet _ :: rest -> ([], rest)
    | tok :: _ ->
        failwith
          (Printf.sprintf "Unexpected token in function implementation on %s"
             (print_position (token_position tok)))
  in
  let name, cs =
    match tokens with
    | TkIdent (position, name) :: cs -> ({ str = name; position }, cs)
    | [] -> failwith "Unexpected end in function declaration"
    | tok :: _ ->
        failwith
          (Printf.sprintf "Unexpected token on %s"
             (print_position (token_position tok)))
  in
  let parameters, cs' = parse_names cs in
  let expression, cs'' =
    match parse_expr cs' false with
    | Some expr, cs'' -> (expr, cs'')
    | None, _ ->
        failwith "Expected valid expression within function declaration"
  in
  let expression' = distribute_operator expression in
  ( {
      position = extend_span name.position (get_position expression);
      name;
      parameters;
      expression = expression';
    },
    cs'' )

let parse_stmt tokens =
  match tokens with
  | TkDecl pos :: rest ->
      let { position; name; type_ }, cs = parse_decl rest in
      (Some (StmtDecl { position = extend_span pos position; name; type_ }), cs)
  | TkImpl pos :: rest ->
      let { position; name; parameters; expression }, cs = parse_impl rest in
      ( Some
          (StmtImpl
             {
               position = extend_span pos position;
               name;
               parameters;
               expression;
             }),
        cs )
  | [] -> (None, tokens)
  | tok :: _ ->
      failwith
        (Printf.sprintf "Unexpected token in statement on %s"
           (print_position (token_position tok)))

let rec collect_declarations statements =
  match statements with
  | StmtDecl decl :: rest ->
      let next_decls, impls = collect_declarations rest in
      (decl :: next_decls, impls)
  | StmtImpl impl :: rest ->
      let decls, next_impls = collect_declarations rest in
      (decls, impl :: next_impls)
  | [] -> ([], [])
