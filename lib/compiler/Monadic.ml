open Parser.Ast
open Utils

type atm = AtmVar of string | AtmInt of int | AtmBool of bool

type math_op =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEq
  | OpNe
  | OpLess
  | OpGreater
  | OpLessEq
  | OpGreaterEq
  | OpXor

let print_math_op operator =
  match operator with
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq -> "=="
  | OpNe -> "!="
  | OpLess -> "<"
  | OpGreater -> ">"
  | OpLessEq -> "<="
  | OpGreaterEq -> ">="
  | OpXor -> "^"

type node =
  | AtmValue of atm
  | AtmFunction of string * atm list
  | AtmOp of math_op * atm * atm
  | AtmIf of atm * node * node
  | Let of string * node * node
  | Sequence of node * node

type simplified = Node of node | Atm of atm

let rec remove_complex_operands variables count expr =
  let simplify_argument variables count expr =
    match expr with
    | TmLiteral lit ->
        ( Atm
            (match lit with
            | LitBool value -> AtmBool value
            | LitNumber value -> AtmInt value),
          count )
    | TmApplication { name; _ } ->
        if StringMap.mem name variables then (Atm (AtmVar name), count)
        else
          let node, count = remove_complex_operands variables count expr in
          (Node node, count)
    | other ->
        let node, count = remove_complex_operands variables count other in
        (Node node, count)
  in
  let create_let expr result =
    match expr with
    | Atm expr, count -> result expr count
    | Node expr, count ->
        let arg_name = Format.sprintf "tmp.%d" count in
        let result, count = result (AtmVar arg_name) (count + 1) in
        (Let (arg_name, expr, result), count)
  in
  match expr with
  | TmLiteral lit ->
      ( (match lit with
        | LitBool value -> AtmValue (AtmBool value)
        | LitNumber value -> AtmValue (AtmInt value)),
        count )
  | TmApplication { name; arguments } ->
      if StringMap.mem name variables then (AtmValue (AtmVar name), count)
      else
        let rec simplify_function variables name args prev_args count =
          match args with
          | [] -> (AtmFunction (name, List.rev prev_args), count)
          | arg :: rest ->
              create_let (simplify_argument variables count arg)
                (fun atm count ->
                  simplify_function variables name rest (atm :: prev_args) count)
        in
        simplify_function variables name arguments [] count
  | TmOpApp { operator; lhs; rhs } -> (
      match operator with
      | OpSemicolon ->
          let lhs, count = remove_complex_operands variables count lhs in
          let rhs, count = remove_complex_operands variables count rhs in
          (Sequence (lhs, rhs), count)
      | op ->
          create_let (simplify_argument variables count lhs) (fun lhs count ->
              match op with
              | OpAnd ->
                  let rhs, count =
                    remove_complex_operands variables count rhs
                  in
                  (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
              | OpOr ->
                  let rhs, count =
                    remove_complex_operands variables count rhs
                  in
                  (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
              | other ->
                  let op =
                    match other with
                    | OpAdd -> OpAdd
                    | OpSub -> OpSub
                    | OpMul -> OpMul
                    | OpDiv -> OpDiv
                    | OpEq -> OpEq
                    | OpNe -> OpNe
                    | OpLess -> OpLess
                    | OpGreater -> OpGreater
                    | OpLessEq -> OpLessEq
                    | OpGreaterEq -> OpGreaterEq
                    | OpXor -> OpXor
                    | _ -> failwith "unreachable"
                  in
                  create_let (simplify_argument variables count rhs)
                    (fun rhs count -> (AtmOp (op, lhs, rhs), count))))
  | TmLet { name; value; expression } ->
      let value, count = remove_complex_operands variables count value in
      let variables = StringMap.add name true variables in
      let expression, count =
        remove_complex_operands variables count expression
      in
      (Let (name, value, expression), count)
  | TmIf { condition; if_true; if_false } ->
      create_let (simplify_argument variables count condition)
        (fun cond count ->
          let lhs, count = remove_complex_operands variables count if_true in
          let rhs, count = remove_complex_operands variables count if_false in
          (AtmIf (cond, lhs, rhs), count))
