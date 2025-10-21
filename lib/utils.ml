module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type char_position = { line : int; char : int }
type position = char_position * char_position
type substring = { str : string; position : position }

let print_position (left, right) =
  Format.sprintf "%d:%d - %d:%d" (left.line + 1) (left.char + 1)
    (right.line + 1) (right.char + 1)
