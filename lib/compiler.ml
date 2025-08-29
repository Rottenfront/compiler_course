open Parser
open Utils

type racket_clear_node =
  | RkCVar of string
  | RkCValueNumber of int
  | RkCApplication of string * racket_clear_node list
  | RkCLet of string * racket_clear_node * racket_clear_node

let uniquify input =
  let rec uniquify_exp count (context : string StringMap.t) (expr : racket_node)
      =
    match expr with
    | RkVar (_, name) -> (RkCVar (StringMap.find name context), count)
    | RkValueNumber (_, value) -> (RkCValueNumber value, count)
    | RkApplication (_, { name = { str = name; position = _ }; arguments }) ->
        let rec uniquify_all count context exprs =
          match exprs with
          | [] -> ([], count)
          | expr :: rest ->
              let expr, count' = uniquify_exp count context expr in
              let exprs, count'' = uniquify_all count' context rest in
              (expr :: exprs, count'')
        in
        let arguments', count' = uniquify_all count context arguments in
        (RkCApplication (name, arguments'), count')
    | RkLet (info, vars, expr) -> (
        match vars with
        | [] -> uniquify_exp count context expr
        | { name = { str = name; position = _ }; value } :: rest ->
            let value', count' = uniquify_exp count context value in
            let name', count'' =
              (Printf.sprintf "%s.%d" name count', count' + 1)
            in
            let context' = StringMap.add name name' context in
            let expr', count''' =
              uniquify_exp count'' context' (RkLet (info, rest, expr))
            in
            (RkCLet (name', value', expr'), count'''))
  in
  let uniquified, _ = uniquify_exp 1 StringMap.empty input in
  uniquified

let rec format_uniquified node =
  match node with
  | RkCVar name -> name
  | RkCValueNumber number -> string_of_int number
  | RkCApplication (name, args) ->
      let args' = List.map (fun arg -> " " ^ format_uniquified arg) args in
      "(" ^ name ^ String.concat "" args' ^ ")"
  | RkCLet (name, value, expr) ->
      let value' = format_uniquified value in
      let expr' = format_uniquified expr in
      "(let ([" ^ name ^ " " ^ value' ^ "]) " ^ expr' ^ ")"

module MonadicRacket = struct
  type atm = Int of int | Var of string

  type node =
    | Atm of atm
    | Read
    | UnMinus of atm
    | BinMinus of atm * atm
    | Plus of atm * atm
    | Let of string * node * node

  let remove_complex_operands input =
    let rec remove_complex_operands_inner count (expr : racket_clear_node) :
        node * int =
      match expr with
      | RkCApplication (name, args) -> (
          match name with
          | "-" -> (
              match args with
              | [ value ] -> (
                  match value with
                  | RkCVar name -> (UnMinus (Var name), count)
                  | RkCValueNumber value -> (UnMinus (Int value), count)
                  | other ->
                      let expr', count' =
                        remove_complex_operands_inner count other
                      in
                      let new_name = Printf.sprintf "tmp.%d" count' in
                      (Let (new_name, expr', UnMinus (Var new_name)), count' + 1)
                  )
              | [ lhs; rhs ] -> (
                  match lhs with
                  | RkCVar l_name -> (
                      match rhs with
                      | RkCVar r_name ->
                          (BinMinus (Var l_name, Var r_name), count)
                      | RkCValueNumber r_val ->
                          (BinMinus (Var l_name, Int r_val), count)
                      | _ ->
                          let rhs', count' =
                            remove_complex_operands_inner count rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count' in
                          ( Let (r_name, rhs', BinMinus (Var l_name, Var r_name)),
                            count' + 1 ))
                  | RkCValueNumber l_val -> (
                      match rhs with
                      | RkCVar r_name ->
                          (BinMinus (Int l_val, Var r_name), count)
                      | RkCValueNumber r_val ->
                          (BinMinus (Int l_val, Int r_val), count)
                      | _ ->
                          let rhs', count' =
                            remove_complex_operands_inner count rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count' in
                          ( Let (r_name, rhs', BinMinus (Int l_val, Var r_name)),
                            count' + 1 ))
                  | _ -> (
                      let lhs', count' =
                        remove_complex_operands_inner count lhs
                      in
                      let l_name, count'' =
                        (Printf.sprintf "tmp.%d" count', count' + 1)
                      in
                      match rhs with
                      | RkCVar r_name ->
                          ( Let (l_name, lhs', BinMinus (Var l_name, Var r_name)),
                            count'' )
                      | RkCValueNumber r_val ->
                          ( Let (l_name, lhs', BinMinus (Var l_name, Int r_val)),
                            count'' )
                      | _ ->
                          let rhs', count''' =
                            remove_complex_operands_inner count'' rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count''' in
                          ( Let
                              ( l_name,
                                lhs',
                                Let
                                  ( r_name,
                                    rhs',
                                    BinMinus (Var l_name, Var r_name) ) ),
                            count''' + 1 )))
              | _ -> failwith "unreachable")
          | "+" -> (
              match args with
              | [ lhs; rhs ] -> (
                  match lhs with
                  | RkCVar l_name -> (
                      match rhs with
                      | RkCVar r_name -> (Plus (Var l_name, Var r_name), count)
                      | RkCValueNumber r_val ->
                          (Plus (Var l_name, Int r_val), count)
                      | _ ->
                          let rhs', count' =
                            remove_complex_operands_inner count rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count' in
                          ( Let (r_name, rhs', Plus (Var l_name, Var r_name)),
                            count' + 1 ))
                  | RkCValueNumber l_val -> (
                      match rhs with
                      | RkCVar r_name -> (Plus (Int l_val, Var r_name), count)
                      | RkCValueNumber r_val ->
                          (Plus (Int l_val, Int r_val), count)
                      | _ ->
                          let rhs', count' =
                            remove_complex_operands_inner count rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count' in
                          ( Let (r_name, rhs', Plus (Int l_val, Var r_name)),
                            count' + 1 ))
                  | _ -> (
                      let lhs', count' =
                        remove_complex_operands_inner count lhs
                      in
                      let l_name, count'' =
                        (Printf.sprintf "tmp.%d" count', count' + 1)
                      in
                      match rhs with
                      | RkCVar r_name ->
                          ( Let (l_name, lhs', Plus (Var l_name, Var r_name)),
                            count'' )
                      | RkCValueNumber r_val ->
                          ( Let (l_name, lhs', Plus (Var l_name, Int r_val)),
                            count'' )
                      | _ ->
                          let rhs', count''' =
                            remove_complex_operands_inner count'' rhs
                          in
                          let r_name = Printf.sprintf "tmp.%d" count''' in
                          ( Let
                              ( l_name,
                                lhs',
                                Let (r_name, rhs', Plus (Var l_name, Var r_name))
                              ),
                            count''' + 1 )))
              | _ -> failwith "unreachable")
          | "read" -> (Read, count)
          | _ -> failwith "unreachable")
      | RkCLet (name, value, expr) ->
          let value', count' = remove_complex_operands_inner count value in
          let expr', count'' = remove_complex_operands_inner count' expr in
          (Let (name, value', expr'), count'')
      | RkCValueNumber value -> (Atm (Int value), count)
      | RkCVar name -> (Atm (Var name), count)
    in
    match remove_complex_operands_inner 0 input with res, _ -> res

  let rec format node =
    let format_atm value =
      match value with Var name -> name | Int number -> string_of_int number
    in
    match node with
    | Atm value -> format_atm value
    | Read -> "(read)"
    | UnMinus expr -> "(- " ^ format_atm expr ^ ")"
    | BinMinus (lhs, rhs) -> "(- " ^ format_atm lhs ^ " " ^ format_atm rhs ^ ")"
    | Plus (lhs, rhs) -> "(+ " ^ format_atm lhs ^ " " ^ format_atm rhs ^ ")"
    | Let (name, value, expr) ->
        let value' = format value in
        let expr' = format expr in
        "(let ([" ^ name ^ " " ^ value' ^ "]) " ^ expr' ^ ")"
end

module ExplicateControl = struct
  type atm = MonadicRacket.atm

  type exp =
    | Atm of atm
    | Read
    | UnMinus of atm
    | BinMinus of atm * atm
    | Plus of atm * atm

  type stmt = Return of exp | Assign of string * exp

  let order expr =
    let rec order_inner (name : string option) expr =
      match expr with
      | MonadicRacket.Atm value -> (
          match name with
          | Some name -> [ Assign (name, Atm value) ]
          | None -> [ Return (Atm value) ])
      | MonadicRacket.BinMinus (lhs, rhs) -> (
          match name with
          | Some name -> [ Assign (name, BinMinus (lhs, rhs)) ]
          | None -> [ Return (BinMinus (lhs, rhs)) ])
      | MonadicRacket.UnMinus value -> (
          match name with
          | Some name -> [ Assign (name, UnMinus value) ]
          | None -> [ Return (UnMinus value) ])
      | MonadicRacket.Plus (lhs, rhs) -> (
          match name with
          | Some name -> [ Assign (name, Plus (lhs, rhs)) ]
          | None -> [ Return (Plus (lhs, rhs)) ])
      | MonadicRacket.Read -> (
          match name with
          | Some name -> [ Assign (name, Read) ]
          | None -> [ Return Read ])
      | MonadicRacket.Let (new_name, value, expr) ->
          let assignment = order_inner (Some new_name) value in
          let expr_result = order_inner name expr in
          List.append assignment expr_result
    in
    order_inner None expr

  let format statement =
    let format_expr expr =
      let format_atm atm =
        match atm with
        | MonadicRacket.Int number -> string_of_int number
        | MonadicRacket.Var name -> name
      in
      match expr with
      | Atm value -> format_atm value
      | Read -> "(read)"
      | UnMinus value -> "-" ^ format_atm value
      | BinMinus (lhs, rhs) -> format_atm lhs ^ " - " ^ format_atm rhs
      | Plus (lhs, rhs) -> format_atm lhs ^ " + " ^ format_atm rhs
    in
    match statement with
    | Return expr -> "return " ^ format_expr expr
    | Assign (name, expr) -> name ^ " = " ^ format_expr expr
end
