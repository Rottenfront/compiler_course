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
  | TkIf
  | TkThen
  | TkElse
  | TkTrue
  | TkFalse

type token = { type_ : token_type; position : position }

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

let lexer source_language (input : string) : token list =
  let rec aux line pos input =
    match input with
    | [] -> []
    | '\n' :: rest -> aux (line + 1) 0 rest
    | c :: rest when is_whitespace c -> aux line (pos + 1) rest
    | c :: rest when is_special_char c ->
        let position = ({ line; char = pos }, { line; char = pos + 1 }) in
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
        let position = ({ line; char = pos }, { line; char = new_pos }) in
        { type_ = TkNumber number; position } :: aux line new_pos cs
    | c :: rest when is_operator_symbol c && source_language = Config.Lama ->
        let operator, new_pos, cs = lex_operator pos [ c ] rest in
        let position = ({ line; char = pos }, { line; char = new_pos }) in
        { type_ = TkOperator operator; position } :: aux line new_pos cs
    | c :: rest ->
        let ident, new_pos, cs =
          lex_ident (source_language = Config.Racket) pos [ c ] rest
        in
        let position = ({ line; char = pos }, { line; char = new_pos }) in
        let type_ =
          match ident with
          | "let" -> TkLet
          | "if" -> TkIf
          | "define" -> TkDefine
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
