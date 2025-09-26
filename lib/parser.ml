open Lexer

type node_info = { position : position }
type substring = { str : string; position : position }
type literal = LitInt of int | LitBool of bool

type node =
  | TmLiteral of node_info * literal
  | TmVar of node_info * string
  | TmApp of node_info * substring * node list
  | TmIf of node_info * node * node * node
  | TmLet of node_info * (substring * node) list * node

let default_position = { line = 0; char = 0 }
let default_info = { position = (default_position, default_position) }

let set_info info node =
  match node with
  | TmLiteral (_, value) -> TmLiteral (info, value)
  | TmVar (_, value) -> TmVar (info, value)
  | TmApp (_, name, args) -> TmApp (info, name, args)
  | TmIf (_, cond, left, right) -> TmIf (info, cond, left, right)
  | TmLet (_, vars, expr) -> TmLet (info, vars, expr)

let rec parse_expr tokens =
  let parse_parenth tokens =
    match tokens with
    | TkLet _ :: rest ->
        let parse_assignment tokens =
          match tokens with
          | TkIdent (position, str) :: rest ->
              let node, cs = parse_expr rest in
              (({ str; position }, node), cs)
          | _ -> failwith "Unexpected token within assignment"
        in
        let rec parse_parenthised_assignments tokens =
          match tokens with
          | TkParenOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | TkParenClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | TkBraceOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | TkBraceClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | TkBracketOpen _ :: rest -> (
              let assignment, cs = parse_assignment rest in
              match cs with
              | TkBracketClose _ :: cs' ->
                  let arr, cs'' = parse_parenthised_assignments cs' in
                  (assignment :: arr, cs'')
              | _ -> failwith "Unexpected token within parenthised assignments")
          | _ -> ([], tokens)
        in
        let parse_let_assignments tokens =
          match tokens with
          | TkParenOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | TkParenClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | TkBraceOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | TkBraceClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | TkBracketOpen _ :: rest -> (
              let assignments, cs = parse_parenthised_assignments rest in
              match cs with
              | TkBracketClose _ :: cs' -> (assignments, cs')
              | _ -> failwith "Unexpected token after assignments")
          | _ -> failwith "Unexpected token after let"
        in
        let assignments, cs = parse_let_assignments rest in
        let node, cs' = parse_expr cs in
        (TmLet (default_info, assignments, node), cs')
    | TkIdent (position, str) :: rest ->
        let rec parse_arguments tokens =
          match tokens with
          | TkParenClose _ :: _ -> ([], tokens)
          | TkBraceClose _ :: _ -> ([], tokens)
          | TkBracketClose _ :: _ -> ([], tokens)
          | _ ->
              let node, cs = parse_expr tokens in
              let next, cs' = parse_arguments cs in
              (node :: next, cs')
        in
        let arguments, cs = parse_arguments rest in
        (TmApp (default_info, { str; position }, arguments), cs)
    | _ -> failwith "Unexpected token in parentheses"
  in
  match tokens with
  | [] -> failwith "No input to parse_expr"
  | TkNumber (position, value) :: rest ->
      (TmLiteral ({ position }, LitInt value), rest)
  | TkTrue position :: rest -> (TmLiteral ({ position }, LitBool true), rest)
  | TkFalse position :: rest -> (TmLiteral ({ position }, LitBool false), rest)
  | TkIdent (position, ident) :: rest -> (TmVar ({ position }, ident), rest)
  | TkBraceOpen position :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = position in
      let (_, close), rest =
        match cs with
        | TkBraceClose close_pos :: rest -> (close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened brace at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed brace"
      in
      let info = { position = (start, close) } in
      (set_info info node, rest)
  | TkBracketOpen pos :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = pos in
      let (_, close), rest =
        match cs with
        | TkBracketClose close_pos :: rest -> (close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened bracket at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed bracket"
      in
      let info = { position = (start, close) } in
      (set_info info node, rest)
  | TkParenOpen pos :: rest ->
      let node, cs = parse_parenth rest in
      let start, _ = pos in
      let (_, close), rest =
        match cs with
        | TkParenClose close_pos :: rest -> (close_pos, rest)
        | [] ->
            failwith
              (Printf.sprintf "Opened parenthesis at %d:%d" (start.line + 1)
                 (start.char + 1))
        | _ -> failwith "Unclosed parenthesis"
      in
      let info = { position = (start, close) } in
      (set_info info node, rest)
  | _ -> failwith "Unexpected token"
