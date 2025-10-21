open Monadic

type exp =
  | Atm of atm
  | Function of string * atm list
  | Operator of math_op * atm * atm

type stmt =
  | Assign of string * exp
  | Return of exp
  | CMov of atm * int
  | Jmp of int
  | Label of int

let rec explicate_control expr name count =
  let return_expr name expr =
    ( (match name with
      | None -> [ Return expr ]
      | Some name -> [ Assign (name, expr) ]),
      count )
  in
  match expr with
  | AtmValue atm ->
      let expr = Atm atm in
      return_expr name expr
  | AtmFunction (func, args) ->
      let expr = Function (func, args) in
      return_expr name expr
  | AtmOp (op, lhs, rhs) ->
      let expr = Operator (op, lhs, rhs) in
      return_expr name expr
  | AtmIf (cond, lhs, rhs) ->
      let lhs, count = explicate_control lhs name count in
      let rhs, count = explicate_control rhs name count in
      let cond = CMov (cond, count) in
      let inter_jmp = Jmp (count + 1) in
      let inter_label = Label count in
      let end_label = Label (count + 1) in
      ( List.concat
          [ [ cond ]; rhs; [ inter_jmp; inter_label ]; lhs; [ end_label ] ],
        count + 2 )
  | Let (new_name, value, expr) ->
      let value, count = explicate_control value (Some new_name) count in
      let expr, count = explicate_control expr name count in
      (List.append value expr, count)
  | Sequence (lhs, rhs) ->
      let lhs, count = explicate_control lhs None count in
      let rhs, count = explicate_control rhs name count in
      (List.append lhs rhs, count)

let format_atm atm =
  match atm with
  | AtmVar name -> name
  | AtmInt num -> string_of_int num
  | AtmBool bool -> string_of_bool bool

let format_exp expr =
  match expr with
  | Atm atm -> format_atm atm
  | Function (name, atms) ->
      Format.sprintf "%s(%s)" name
        (String.concat ", " (List.map format_atm atms))
  | Operator (op, lhs, rhs) ->
      Format.sprintf "%s %s %s" (format_atm lhs) (print_math_op op)
        (format_atm rhs)

let format_statement stmt =
  match stmt with
  | Assign (name, expr) -> Format.sprintf "  %s <- %s" name (format_exp expr)
  | Return expr -> Format.sprintf "  return %s" (format_exp expr)
  | CMov (cond, idx) -> Format.sprintf "  if %s go %d" (format_atm cond) idx
  | Jmp idx -> Format.sprintf "  jmp %d" idx
  | Label idx -> Format.sprintf "label%d:" idx
