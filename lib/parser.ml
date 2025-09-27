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

type type_expr = TyInt of position | TyBool of position | TyUnit of position

type expr =
  | TmLiteral of position * literal
  | TmApplication of position * application_info
  | TmOpApp of position * operator_info
  | TmLet of position * var_info * expr
  | TmIf of position * expr * expr * expr

and application_info = { name : substring; arguments : expr list }
and var_info = { name : substring; value : expr }
and operator_info = { lhs : expr; operator : operator; rhs : expr }

type declaration = { name : substring; types : type_expr list }

type implementation = {
  name : substring;
  parameters : substring list;
  expression : expr;
}

type statement =
  | StmtDecl of position * declaration
  | StmtImpl of position * implementation

let default_char_pos : char_position = { line = 0; char = 0 }
let default_position : position = (default_char_pos, default_char_pos)

let extend_span ((left, _) : position) ((_, right) : position) : position =
  (left, right)

let get_position expr =
  match expr with
  | TmLiteral (position, _) -> position
  | TmApplication (position, _) -> position
  | TmLet (position, _, _) -> position
  | TmOpApp (position, _) -> position
  | TmIf (position, _, _, _) -> position

let type_position type_ =
  match type_ with TyBool pos -> pos | TyInt pos -> pos | TyUnit pos -> pos

let rec parse_expr tokens is_in_application =
  let rec parse_application name position arguments tokens =
    match parse_expr tokens true with
    | Some expr, rest ->
        parse_application name
          (extend_span position (get_position expr))
          (expr :: arguments) rest
    | None, rest ->
        ( Some
            (TmApplication (position, { name; arguments = List.rev arguments })),
          rest )
  in
  let parse_let tokens position =
    let name, rest =
      match tokens with
      | TkIdent (pos, ident) :: rest -> ({ str = ident; position = pos }, rest)
      | [] ->
          failwith
            (Printf.sprintf "Unexpected end in variable definition (%s)"
               (print_position position))
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token in variable definition on %s"
               (print_position (token_position tok)))
    in
    let rest' =
      match rest with
      | TkSet _ :: rest' -> rest'
      | [] ->
          failwith
            (Printf.sprintf "Unexpected end in variable definition (%s)"
               (print_position position))
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token in variable definition on %s"
               (print_position (token_position tok)))
    in
    let value, position, cs =
      match parse_expr rest' false with
      | Some expr, cs -> (expr, extend_span position (get_position expr), cs)
      | _ ->
          failwith
            (Printf.sprintf "Unexpected end in variable definition (%s)"
               (print_position position))
    in
    let position, cs' =
      match cs with
      | TkIn tok_pos :: cs' -> (extend_span position tok_pos, cs')
      | _ ->
          failwith
            (Printf.sprintf "Unexpected end in variable definition (%s)"
               (print_position position))
    in
    match parse_expr cs' false with
    | Some expr, cs'' ->
        ( Some
            (TmLet
               (extend_span position (get_position expr), { name; value }, expr)),
          cs'' )
    | None, _ ->
        failwith
          (Printf.sprintf "Unexpected end in variable definition (%s)"
             (print_position position))
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
              | _ -> failwith (Printf.sprintf "Unknown operator: %s" op_token)
            in
            parse_rhs lhs cs operator pos
        | _ -> (Some lhs, rest))
    | None -> (None, rest)

let parse_type tokens =
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
            | "unit" -> TyUnit pos
            | _ ->
                failwith
                  (Printf.sprintf "Unknown type `%s` on %s" name
                     (print_position pos)))
            :: next_types,
            cs )
      | [] -> failwith "Unexpected end in type declaration"
      | tok :: _ ->
          failwith
            (Printf.sprintf "Unexpected token on %s"
               (print_position (token_position tok)))
  in
  aux tokens false

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
  let types, cs'' = parse_type cs' in
  ( { name; types },
    types |> List.map type_position |> List.fold_left extend_span name.position,
    cs'' )

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
  ( { name; parameters; expression },
    extend_span name.position (get_position expression),
    cs'' )

let parse_stmt tokens =
  match tokens with
  | TkDecl position :: rest ->
      let stmt, pos, cs = parse_decl rest in
      (Some (StmtDecl (extend_span position pos, stmt)), cs)
  | TkImpl position :: rest ->
      let stmt, pos, cs = parse_impl rest in
      (Some (StmtImpl (extend_span position pos, stmt)), cs)
  | [] -> (None, tokens)
  | tok :: _ ->
      failwith
        (Printf.sprintf "Unexpected token in statement on %s"
           (print_position (token_position tok)))
