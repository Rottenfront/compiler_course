(* open Parser
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
    let rec order_inner (name : string option) expr names =
      match expr with
      | MonadicRacket.Let (new_name, value, expr) ->
          let assignment, names' = order_inner (Some new_name) value names in
          let expr_result, names'' = order_inner name expr names' in
          (List.append assignment expr_result, names'')
      | other -> (
          let result_expr =
            match other with
            | MonadicRacket.Atm value -> Atm value
            | MonadicRacket.BinMinus (lhs, rhs) -> BinMinus (lhs, rhs)
            | MonadicRacket.UnMinus value -> UnMinus value
            | MonadicRacket.Plus (lhs, rhs) -> Plus (lhs, rhs)
            | _ -> Read
          in
          match name with
          | Some name ->
              ( [ Assign (name, result_expr) ],
                if StringMap.mem name names then names
                else StringMap.add name (StringMap.cardinal names) names )
          | None -> ([ Return result_expr ], names))
    in
    order_inner None expr StringMap.empty

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

module AsmGenerator = struct
  type reg = X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28
  type atm = Reg of reg | Num of int

  type stmt =
    | Mov of reg * atm
    | Add of reg * atm * atm
    | Sub of reg * atm * atm
    | Store of int * reg
    | Load of int * reg
    | Read of reg
    | Result of reg

  let compile statements names =
    let compile_statement statement names =
      let compile_expr expression names =
        match expression with
        | ExplicateControl.Atm atm -> (
            match atm with
            | MonadicRacket.Int number -> [ Mov (X19, Num number) ]
            | MonadicRacket.Var name ->
                [ Load (StringMap.find name names, X19) ])
        | ExplicateControl.Read -> [ Read X19 ]
        | ExplicateControl.UnMinus atm ->
            [
              (match atm with
              | MonadicRacket.Int number -> Mov (X19, Num number)
              | MonadicRacket.Var name -> Load (StringMap.find name names, X19));
              Mov (X20, Num 0);
              Sub (X19, Reg X20, Reg X19);
            ]
        | ExplicateControl.BinMinus (lhs, rhs) ->
            [
              (match lhs with
              | MonadicRacket.Int number -> Mov (X20, Num number)
              | MonadicRacket.Var name -> Load (StringMap.find name names, X20));
              (match rhs with
              | MonadicRacket.Int number -> Mov (X21, Num number)
              | MonadicRacket.Var name -> Load (StringMap.find name names, X21));
              Sub (X19, Reg X20, Reg X21);
            ]
        | ExplicateControl.Plus (lhs, rhs) ->
            [
              (match lhs with
              | MonadicRacket.Int number -> Mov (X20, Num number)
              | MonadicRacket.Var name -> Load (StringMap.find name names, X20));
              (match rhs with
              | MonadicRacket.Int number -> Mov (X21, Num number)
              | MonadicRacket.Var name -> Load (StringMap.find name names, X21));
              Add (X19, Reg X20, Reg X21);
            ]
      in
      match statement with
      | ExplicateControl.Assign (name, expression) ->
          let prelude = compile_expr expression names in
          List.append prelude [ Store (StringMap.find name names, X19) ]
      | ExplicateControl.Return expression ->
          let prelude = compile_expr expression names in
          List.append prelude [ Result X19 ]
    in
    List.concat
      (List.map (fun statement -> compile_statement statement names) statements)

  let print_reg register =
    match register with
    | X19 -> "x19"
    | X20 -> "x20"
    | X21 -> "x21"
    | X22 -> "x22"
    | X23 -> "x23"
    | X24 -> "x24"
    | X25 -> "x25"
    | X26 -> "x26"
    | X27 -> "x27"
    | X28 -> "x28"

  let print_atm atomic =
    match atomic with
    | Num number -> Printf.sprintf "#%d" number
    | Reg register -> print_reg register

  let format_statement stmt =
    match stmt with
    | Mov (register, atomic) ->
        Printf.sprintf "    mov %s, %s\n" (print_reg register)
          (print_atm atomic)
    | Add (dest, lhs, rhs) ->
        Printf.sprintf "    add %s, %s, %s\n" (print_reg dest) (print_atm lhs)
          (print_atm rhs)
    | Sub (dest, lhs, rhs) ->
        Printf.sprintf "    sub %s, %s, %s\n" (print_reg dest) (print_atm lhs)
          (print_atm rhs)
    | Store (index, src) ->
        Printf.sprintf "    str %s, [sp, #%d]!\n    sub sp, sp, #%d\n"
          (print_reg src) (index * 8) (index * 8)
    | Load (index, dest) ->
        Printf.sprintf "    ldr %s, [sp, #%d]!\n    sub sp, sp, #%d\n"
          (print_reg dest) (index * 8) (index * 8)
    | Read dest ->
        Printf.sprintf
          "\n\
          \    adrp x0, template@PAGE\n\
          \    add x0, x0, template@PAGEOFF\n\
          \    adrp x11, num@PAGE\n\
          \    add x11, x11, num@PAGEOFF\n\
          \    str x11, [SP, #-16]!\n\
          \    bl _scanf\n\
          \    add sp, sp, #16\n\
          \    adrp x10, num@PAGE\n\
          \    add x10, x10, num@PAGEOFF\n\
          \    ldr %s, [x10]\n"
          (print_reg dest)
    | Result src ->
        Printf.sprintf
          "\n\
          \    ADRP X0, message@PAGE\n\
          \    ADD X0, X0, message@PAGEOFF\n\
          \    STR %s, [SP, #-16]!\n\
          \    BL  _printf\n\
          \    ADD SP, SP, #16\n"
          (print_reg src)

  let compile_format statements names =
    let logic =
      String.concat "" (List.map format_statement (compile statements names))
    in
    let prelude =
      ".global _main\n.align 4\n\n_main:\n    stp x29, x30, [SP, #-16]!\n"
    in
    let shift = StringMap.cardinal names * 8 in
    let aligned_shift = if shift mod 16 == 8 then shift + 8 else shift in
    let memory_allocation =
      Printf.sprintf "    sub sp, sp, #%d\n" aligned_shift
    in
    let memory_deallocation =
      Printf.sprintf "    add sp, sp, #%d\n" aligned_shift
    in
    let epilogue =
      "\n\
      \    mov X0, #0\n\
      \    LDP X29, X30, [SP], #16\n\
      \    ret\n\n\
       .data\n\
       .balign 4\n\
       message:    .asciz \"Result: %lld\\n\"\n\
       .balign 4\n\
       num:    .quad 0\n\
       .balign 4\n\
       template:   .asciz \"%lld\"\n"
    in
    String.concat "\n"
      [ prelude; memory_allocation; logic; memory_deallocation; epilogue ]
end
*)
