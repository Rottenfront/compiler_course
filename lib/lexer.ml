type char_position = { line : int; char : int }
type position = char_position * char_position

let print_position (left, right) =
  Printf.sprintf "%d:%d - %d:%d" left.line left.char right.line (right.char - 1)

type lex_token =
  | TkIdent of position * string
  | TkOperator of position * string
  | TkNumber of position * int
  | TkParenOpen of position
  | TkParenClose of position
  | TkBracketOpen of position
  | TkBracketClose of position
  | TkBraceOpen of position
  | TkBraceClose of position
  | TkLet of position
  | TkIn of position
  | TkDecl of position
  | TkImpl of position
  | TkIf of position
  | TkThen of position
  | TkElse of position
  | TkAnd of position
  | TkOr of position
  | TkArrow of position
  | TkDoubleColon of position
  | TkSet of position
  | TkTrue of position
  | TkFalse of position

let print_token token =
  match token with
  | TkIdent (_, ident) -> ident
  | TkOperator (_, ident) -> ident
  | TkNumber (_, num) -> string_of_int num
  | TkParenOpen _ -> "("
  | TkParenClose _ -> ")"
  | TkBracketOpen _ -> "["
  | TkBracketClose _ -> "]"
  | TkBraceOpen _ -> "{"
  | TkBraceClose _ -> "}"
  | TkLet _ -> "let"
  | TkIn _ -> "in"
  | TkDecl _ -> "decl"
  | TkImpl _ -> "impl"
  | TkIf _ -> "if"
  | TkThen _ -> "then"
  | TkElse _ -> "else"
  | TkAnd _ -> "and"
  | TkOr _ -> "or"
  | TkArrow _ -> "->"
  | TkDoubleColon _ -> "::"
  | TkSet _ -> "="
  | TkTrue _ -> "true"
  | TkFalse _ -> "false"

let token_position token =
  match token with
  | TkIdent (pos, _) -> pos
  | TkOperator (pos, _) -> pos
  | TkNumber (pos, _) -> pos
  | TkParenOpen pos -> pos
  | TkParenClose pos -> pos
  | TkBracketOpen pos -> pos
  | TkBracketClose pos -> pos
  | TkBraceOpen pos -> pos
  | TkBraceClose pos -> pos
  | TkLet pos -> pos
  | TkIn pos -> pos
  | TkDecl pos -> pos
  | TkImpl pos -> pos
  | TkIf pos -> pos
  | TkThen pos -> pos
  | TkElse pos -> pos
  | TkAnd pos -> pos
  | TkOr pos -> pos
  | TkArrow pos -> pos
  | TkDoubleColon pos -> pos
  | TkSet pos -> pos
  | TkTrue pos -> pos
  | TkFalse pos -> pos

let is_digit c = c >= '0' && c <= '9'

let is_operator_symbol c =
  c = '-' || c = '+' || c = '!' || c = '%' || c = '^' || c = '&' || c = '*'
  || c = '/' || c = '=' || c = '<' || c = '>' || c = '|' || c = ':'

let is_special_char c =
  c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}'

let is_whitespace c = c = '\n' || c = '\t' || c = '\r' || c = ' '

(* List to string *)
let list_to_reversed_string x =
  let len = List.length x in
  String.init len (fun n -> List.nth x (len - n - 1))

let rec lex_ident pos acc input =
  match input with
  | c :: rest when is_special_char c || is_operator_symbol c || is_whitespace c
    ->
      (list_to_reversed_string acc, pos, c :: rest)
  | c :: rest -> lex_ident (pos + 1) (c :: acc) rest
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

let lexer (input : string) : lex_token list =
  let rec aux line pos input =
    match input with
    | [] -> []
    | '\n' :: rest -> aux (line + 1) 0 rest
    | c :: rest when is_whitespace c -> aux line (pos + 1) rest
    | c :: rest when is_special_char c ->
        let position = ({ line; char = pos }, { line; char = pos + 1 }) in
        (match c with
        | '(' -> TkParenOpen position
        | ')' -> TkParenClose position
        | '[' -> TkBracketOpen position
        | ']' -> TkBracketClose position
        | '{' -> TkBraceOpen position
        | '}' -> TkBraceClose position
        | _ -> failwith "unreachable")
        :: aux line (pos + 1) rest
    | c :: rest when is_digit c ->
        let number, new_pos, cs = lex_number pos [ c ] rest in
        TkNumber (({ line; char = pos }, { line; char = new_pos }), number)
        :: aux line new_pos cs
    | c :: rest when is_operator_symbol c ->
        let operator, new_pos, cs = lex_operator pos [ c ] rest in
        let position = ({ line; char = pos }, { line; char = new_pos }) in
        (match operator with
        | "->" -> TkArrow position
        | "=" -> TkSet position
        | "::" -> TkDoubleColon position
        | other -> TkOperator (position, other))
        :: aux line new_pos cs
    | c :: rest ->
        let ident, new_pos, cs = lex_ident pos [ c ] rest in
        let position = ({ line; char = pos }, { line; char = new_pos }) in
        (match ident with
        | "let" -> TkLet position
        | "in" -> TkIn position
        | "decl" -> TkDecl position
        | "impl" -> TkImpl position
        | "if" -> TkIf position
        | "then" -> TkThen position
        | "else" -> TkElse position
        | "and" -> TkAnd position
        | "or" -> TkOr position
        | "xor" -> TkOr position
        | "true" -> TkTrue position
        | "false" -> TkFalse position
        | id -> TkIdent (position, id))
        :: aux line new_pos cs
  in
  aux 0 0 (List.init (String.length input) (String.get input))
