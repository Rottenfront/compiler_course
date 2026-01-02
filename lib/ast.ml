type ty = TyNamed of string | TyUnit | TyTuple of ty list

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Call of string * expr list
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Tuple of expr list
  | AccessTuple of int * expr

type param = string * ty

type def =
  | Define of (string * param list * ty) * expr
  | TypeDef of string * ty

type program = def list

let rec enable_tuples expr =
  match expr with
  | Call ("vector", args) -> Tuple (List.map enable_tuples args)
  | Call ("get", args) -> (
      match args with
      | [ Int index; body ] -> AccessTuple (index, body)
      | _ -> failwith "Unexpected get arguments")
  | Call (func, args) -> Call (func, List.map enable_tuples args)
  | If (cond, tru, fls) ->
      If (enable_tuples cond, enable_tuples tru, enable_tuples fls)
  | Let (definitions, body) ->
      Let
        ( List.map (fun (name, value) -> (name, enable_tuples value)) definitions,
          enable_tuples body )
  | Tuple args -> Tuple (List.map enable_tuples args)
  | AccessTuple (index, body) -> AccessTuple (index, enable_tuples body)
  | other -> other
