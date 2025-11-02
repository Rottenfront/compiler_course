module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type char_position = { line : int; char : int }
type position = char_position * char_position
type substring = { str : string; position : position }

let print_char_position { line; char } =
  Format.sprintf "%d:%d" (line + 1) (char + 1)

let print_position (left, right) =
  print_char_position left ^ " - " ^ print_char_position right
