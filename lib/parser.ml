open Lexer

type text_position = { line : int; char : int }
type text_span = text_position * text_position
type node_info = { position : text_span }
type substring = { str : string; position : text_span }

type racket_node =
  | RkValueNumber of node_info * int
  | RkVar of node_info * string
  | RkApplication of node_info * application_info
  (* | RkList of info * racket_node list *)
  | RkLet of node_info * var_info list * racket_node

and application_info = { name : substring; arguments : racket_node list }
and var_info = { name : substring; value : racket_node }

let default_position = { line = 0; char = 0 }
let default_info = { position = (default_position, default_position) }

let span_from_position (pos : position) : text_span =
  let start, close = pos.index in
  ({ line = pos.line; char = start }, { line = pos.line; char = close })

let rec parse_racket_expr tokens =
  let parse_parenth tokens =
    match tokens with
    | RkKwLet _ :: rest ->
        let parse_assignment tokens =
          match tokens with
          | RkIdent (pos, str) :: rest ->
              let node, cs = parse_racket_expr rest in
              ( {
                  name = { str; position = span_from_position pos };
                  value = node;
                },
                cs )
          | _ -> failwith "Unexpected token within assignment"
        in
        let rec parse_parenthised_assignments tokens =
          match tokens with
          | RkParenOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | RkParenClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | RkBraceOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | RkBraceClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | RkBracketOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | RkBracketClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | _ -> ([], tokens)
        in
        let parse_let_assignments tokens =
          match tokens with
          | RkParenOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | RkParenClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | RkBraceOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | RkBraceClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | RkBracketOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | RkBracketClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | _ -> failwith "Unexpected token after let"
        in
        let assignments, cs = parse_let_assignments rest in
        let node, cs' = parse_racket_expr cs in
        (RkLet (default_info, assignments, node), cs')
    | RkIdent (pos, str) :: rest ->
        let rec parse_arguments tokens =
          match tokens with
          | RkParenClose _ :: _ -> ([], tokens)
          | RkBraceClose _ :: _ -> ([], tokens)
          | RkBracketClose _ :: _ -> ([], tokens)
          | _ ->
              let node, cs = parse_racket_expr tokens in
              let next, cs' = parse_arguments cs in
              (node :: next, cs')
        in
        let arguments, cs = parse_arguments rest in
        ( RkApplication
            ( default_info,
              { name = { str; position = span_from_position pos }; arguments }
            ),
          cs )
    | _ -> failwith "Unexpected token in parentheses"
  in
  match tokens with
  | [] -> failwith "No input to parse_expr"
  | RkNumber (pos, value) :: rest ->
      (RkValueNumber ({ position = span_from_position pos }, value), rest)
  | RkIdent (pos, ident) :: rest ->
      (RkVar ({ position = span_from_position pos }, ident), rest)
  | RkBraceOpen pos :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = span_from_position pos in
      let (_, close), rest =
        match cs with
        | RkBraceClose close_pos :: rest -> (span_from_position close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened brace at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed brace"
      in
      let info = { position = (start, close) } in
      ( (match node with
        | RkValueNumber (_, value) -> RkValueNumber (info, value)
        | RkVar (_, value) -> RkVar (info, value)
        | RkApplication (_, value) -> RkApplication (info, value)
        | RkLet (_, vars, expr) -> RkLet (info, vars, expr)),
        rest )
  | RkBracketOpen pos :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = span_from_position pos in
      let (_, close), rest =
        match cs with
        | RkBracketClose close_pos :: rest ->
            (span_from_position close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened bracket at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed bracket"
      in
      let info = { position = (start, close) } in
      ( (match node with
        | RkValueNumber (_, value) -> RkValueNumber (info, value)
        | RkVar (_, value) -> RkVar (info, value)
        | RkApplication (_, value) -> RkApplication (info, value)
        | RkLet (_, vars, expr) -> RkLet (info, vars, expr)),
        rest )
  | RkParenOpen pos :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = span_from_position pos in
      let (_, close), rest =
        match cs with
        | RkParenClose close_pos :: rest -> (span_from_position close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened parenthesis at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed parenthesis"
      in
      let info = { position = (start, close) } in
      ( (match node with
        | RkValueNumber (_, value) -> RkValueNumber (info, value)
        | RkVar (_, value) -> RkVar (info, value)
        | RkApplication (_, value) -> RkApplication (info, value)
        | RkLet (_, vars, expr) -> RkLet (info, vars, expr)),
        rest )
  | _ -> failwith "Unexpected token"

let racket_parser input =
  let expr, _ = parse_racket_expr (racket_lexer input) in
  expr
