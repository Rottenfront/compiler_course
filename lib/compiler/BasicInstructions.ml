open AssignHomes
open Monadic
open Utils
open ExplicateControl

type basic_data = BasicReg of reg | BasicInt of int

type instruction =
  | MovIns of reg * basic_data
  | OpIns of reg * math_op * basic_data * basic_data
  | FuncIns of reg * string * basic_data list
  | CMovIns of basic_data * int
  | JmpIns of int
  | LblIns of int

let compile_statement statement homes =
  let atm_to_basic_data atm =
    match atm with
    | AtmVar var -> BasicReg (StringMap.find var homes)
    | AtmInt int -> BasicInt int
    | AtmBool true -> BasicInt 1
    | AtmBool false -> BasicInt 0
  in
  let compile_expression return expr =
    match expr with
    | Atm atm -> MovIns (return, atm_to_basic_data atm)
    | Function (name, args) ->
        FuncIns (return, name, List.map atm_to_basic_data args)
    | Operator (op, lhs, rhs) ->
        OpIns (return, op, atm_to_basic_data lhs, atm_to_basic_data rhs)
  in
  match statement with
  | Assign (var, expr) -> compile_expression (StringMap.find var homes) expr
  | Return expr -> compile_expression (Reg "x0") expr
  | CMov (cond, label) -> CMovIns (atm_to_basic_data cond, label)
  | Label label -> LblIns label
  | Jmp label -> JmpIns label
