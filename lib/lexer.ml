type position = { line : int; index : int * int }

type racket_token =
  | RkIdent of position * string
  | RkNumber of position * int
  (* | RkQuote of position *)
  | RkParenOpen of position
  | RkParenClose of position
  (* | RkString of position * string *)
  | RkBracketOpen of position
  | RkBracketClose of position
  | RkBraceOpen of position
  | RkBraceClose of position
  (* | RkKwDefine of position *)
  | RkKwLet of position
  | RkKwIf of position
(* | RkKwLambda of position *)

let is_digit c = c >= '0' && c <= '9'

let is_special_char c =
  c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}' || c = '\''
  || c = '"'

let is_whitespace c = c = '\n' || c = '\t' || c = '\r' || c = ' '

(* List to string *)
let list_to_rev_string x =
  let len = List.length x in
  String.init len (fun n -> List.nth x (len - n - 1))

let rec lex_rk_ident pos acc input =
  match input with
  | c :: rest when is_special_char c || is_whitespace c ->
      (list_to_rev_string acc, pos, c :: rest)
  | c :: rest -> lex_rk_ident (pos + 1) (c :: acc) rest
  | [] -> (list_to_rev_string acc, pos, [])

let rec lex_rk_number pos acc input =
  match input with
  | c :: rest when is_digit c -> lex_rk_number (pos + 1) (c :: acc) rest
  | rest -> (int_of_string (list_to_rev_string acc), pos, rest)

let rec lex_rk_string line pos acc input =
  match input with
  | [] -> failwith "Undetermined string"
  | '\n' :: rest -> lex_rk_string (line + 1) 0 ('\n' :: acc) rest
  | '\\' :: rest -> (
      match rest with
      | 'r' :: cs -> lex_rk_string line (pos + 2) ('\r' :: acc) cs
      | 'n' :: cs -> lex_rk_string line (pos + 2) ('\n' :: acc) cs
      | 't' :: cs -> lex_rk_string line (pos + 2) ('\t' :: acc) cs
      | '"' :: cs -> lex_rk_string line (pos + 2) ('"' :: acc) cs
      | '\\' :: cs -> lex_rk_string line (pos + 2) ('\\' :: acc) cs
      | _ -> failwith "Unknown backslash symbol")
  | '"' :: rest -> (list_to_rev_string acc, line, pos + 1, rest)
  | c :: rest -> lex_rk_string line (pos + 1) (c :: acc) rest

let racket_lexer (input : string) : racket_token list =
  let rec aux line pos input =
    match input with
    | [] -> []
    | '\n' :: rest -> aux (line + 1) 0 rest
    | c :: rest when is_whitespace c -> aux line (pos + 1) rest
    | '(' :: rest ->
        RkParenOpen { line; index = (pos, pos + 1) } :: aux line (pos + 1) rest
    | ')' :: rest ->
        RkParenClose { line; index = (pos, pos + 1) } :: aux line (pos + 1) rest
    | '[' :: rest ->
        RkBracketOpen { line; index = (pos, pos + 1) }
        :: aux line (pos + 1) rest
    | ']' :: rest ->
        RkBracketClose { line; index = (pos, pos + 1) }
        :: aux line (pos + 1) rest
    | '{' :: rest ->
        RkBraceOpen { line; index = (pos, pos + 1) } :: aux line (pos + 1) rest
    | '}' :: rest ->
        RkBraceClose { line; index = (pos, pos + 1) } :: aux line (pos + 1) rest
    (* | '\'' :: rest -> *)
    (*     RkQuote { line; index = (pos, pos + 1) } :: aux line (pos + 1) rest *)
    | c :: rest when is_digit c ->
        let number, new_pos, cs = lex_rk_number pos [ c ] rest in
        RkNumber ({ line; index = (pos, new_pos) }, number)
        :: aux line new_pos cs
    (* | '"' :: rest -> *)
    (*     let str, cs = lex_rk_string [] rest in *)
    (* (* length of string + 2 because it doesn't include quotes *)
    (*    into the string value *) *)
    (*     let length = String.length str + 2 in *)
    (*     RkString ({ line; index = (pos, pos + length) }, str) *)
    (*     :: aux line (pos + length) cs *)
    | c :: rest ->
        let ident, new_pos, cs = lex_rk_ident pos [ c ] rest in
        (match ident with
        | "let" -> RkKwLet { line; index = (pos, new_pos) }
        | "if" -> RkKwIf { line; index = (pos, new_pos) }
        | id -> RkIdent ({ line; index = (pos, new_pos) }, id))
        :: aux line new_pos cs
  in
  aux 0 0 (List.init (String.length input) (String.get input))
