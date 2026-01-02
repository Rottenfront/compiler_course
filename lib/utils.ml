module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type position = { line : int; char : int }
type span = position * position
type substring = { str : string; position : span }

let print_position { line; char } = Format.sprintf "%d:%d" (line + 1) (char + 1)

let print_span ((left, right) : span) =
  print_position left ^ "-" ^ print_position right

let default_position : position = { line = 0; char = 0 }
let default_span : span = (default_position, default_position)
let extend_span ((left, _) : span) ((_, right) : span) : span = (left, right)
