open Utils

type literal = Parser.literal

let print_literal = Parser.print_literal
let print_operator = Parser.print_operator

module Uniquify = struct
  type term =
    | TmLit of literal
    | TmApp of string * term list
    | TmOp of Parser.operator * term * term
    | TmLet of string * term * term
    | TmIf of term * term * term

  let rec uniquify_expr count (context : string StringMap.t) =
    let rec uniquify_list count context prev exprs =
      match exprs with
      | expr :: rest ->
          let expr, count = uniquify_expr count context expr in
          uniquify_list count context (expr :: prev) rest
      | [] -> (List.rev prev, count)
    in
    fun expr ->
      match expr with
      | Parser.TmApplication (_, { name = { str; position = _ }; arguments }) ->
          let arguments, count = uniquify_list count context [] arguments in
          (TmApp (StringMap.find str context, arguments), count)
      | Parser.TmLiteral (_, lit) -> (TmLit lit, count)
      | Parser.TmOpApp (_, { lhs; operator; rhs }) ->
          let lhs, count = uniquify_expr count context lhs in
          let rhs, count = uniquify_expr count context rhs in
          (TmOp (operator, lhs, rhs), count)
      | Parser.TmIf (_, cond, lhs, rhs) ->
          let cond, count = uniquify_expr count context cond in
          let lhs, count = uniquify_expr count context lhs in
          let rhs, count = uniquify_expr count context rhs in
          (TmIf (cond, lhs, rhs), count)
      | Parser.TmParenth expr -> uniquify_expr count context expr
      | Parser.TmLet (_, { name = { str; position = _ }; value }, expr) ->
          let value, count = uniquify_expr count context value in
          let name = Printf.sprintf "%s.%d" str count in
          let context = StringMap.add str name context in
          let count = count + 1 in
          let expr, count = uniquify_expr count context expr in
          (TmLet (name, value, expr), count)

  let uniquify expr =
    let expr, _ = uniquify_expr 0 StringMap.empty expr in
    expr

  let rec format_uniquified expr in_application =
    match expr with
    | TmLit lit -> print_literal lit
    | TmApp (name, arguments) ->
        let rec print_arguments prev arguments =
          match arguments with
          | expr :: rest ->
              print_arguments
                (Printf.sprintf "%s %s" prev (format_uniquified expr true))
                rest
          | [] -> prev
        in
        if List.is_empty arguments then name
        else
          let arguments = print_arguments "" arguments in
          if in_application then Printf.sprintf "(%s %s)" name arguments
          else Printf.sprintf "%s %s" name arguments
    | TmOp (operator, lhs, rhs) ->
        Printf.sprintf "(%s %s %s)"
          (format_uniquified lhs false)
          (print_operator operator)
          (format_uniquified rhs false)
    | TmLet (name, value, expr) ->
        let value = format_uniquified value false in
        let expr = format_uniquified expr false in
        if in_application then
          Printf.sprintf "(let %s = %s in %s)" name value expr
        else Printf.sprintf "let %s = %s in %s" name value expr
    | TmIf (cond, lhs, rhs) ->
        let cond = format_uniquified cond false in
        let lhs = format_uniquified lhs false in
        let rhs = format_uniquified rhs false in
        if in_application then
          Printf.sprintf "(if %s then %s else %s)" cond lhs rhs
        else Printf.sprintf "if %s then %s else %s" cond lhs rhs
end

module Monadic = struct
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
      | Uniquify.TmLit lit ->
          ( Atm
              (match lit with
              | Parser.LitBool value -> AtmBool value
              | Parser.LitNumber value -> AtmInt value),
            count )
      | Uniquify.TmApp (name, _) ->
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
          let arg_name = Printf.sprintf "tmp.%d" count in
          let result, count = result (AtmVar arg_name) (count + 1) in
          (Let (arg_name, expr, result), count)
    in
    match expr with
    | Uniquify.TmLit lit ->
        ( (match lit with
          | Parser.LitBool value -> AtmValue (AtmBool value)
          | Parser.LitNumber value -> AtmValue (AtmInt value)),
          count )
    | Uniquify.TmApp (name, args) ->
        if StringMap.mem name variables then (AtmValue (AtmVar name), count)
        else
          let rec simplify_function variables name args prev_args count =
            match args with
            | [] -> (AtmFunction (name, List.rev prev_args), count)
            | arg :: rest ->
                create_let (simplify_argument variables count arg)
                  (fun atm count ->
                    simplify_function variables name rest (atm :: prev_args)
                      count)
          in
          simplify_function variables name args [] count
    | Uniquify.TmOp (op, lhs, rhs) -> (
        match op with
        | OpSemicolon ->
            let lhs, count = remove_complex_operands variables count lhs in
            let rhs, count = remove_complex_operands variables count rhs in
            (Sequence (lhs, rhs), count)
        | op ->
            create_let (simplify_argument variables count lhs) (fun lhs count ->
                match op with
                | Parser.OpAnd ->
                    let rhs, count =
                      remove_complex_operands variables count rhs
                    in
                    (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
                | Parser.OpOr ->
                    let rhs, count =
                      remove_complex_operands variables count rhs
                    in
                    (AtmIf (lhs, rhs, AtmValue (AtmBool false)), count)
                | other ->
                    let op =
                      match other with
                      | Parser.OpAdd -> OpAdd
                      | Parser.OpSub -> OpSub
                      | Parser.OpMul -> OpMul
                      | Parser.OpDiv -> OpDiv
                      | Parser.OpEq -> OpEq
                      | Parser.OpNe -> OpNe
                      | Parser.OpLess -> OpLess
                      | Parser.OpGreater -> OpGreater
                      | Parser.OpLessEq -> OpLessEq
                      | Parser.OpGreaterEq -> OpGreaterEq
                      | Parser.OpXor -> OpXor
                      | _ -> failwith "unreachable"
                    in
                    create_let (simplify_argument variables count rhs)
                      (fun rhs count -> (AtmOp (op, lhs, rhs), count))))
    | Uniquify.TmLet (name, value, expr) ->
        let value, count = remove_complex_operands variables count value in
        let variables = StringMap.add name true variables in
        let expr, count = remove_complex_operands variables count expr in
        (Let (name, value, expr), count)
    | Uniquify.TmIf (cond, lhs, rhs) ->
        create_let (simplify_argument variables count cond) (fun cond count ->
            let lhs, count = remove_complex_operands variables count lhs in
            let rhs, count = remove_complex_operands variables count rhs in
            (AtmIf (cond, lhs, rhs), count))
end

module ExplicateControl = struct
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
        Printf.sprintf "%s(%s)" name
          (String.concat ", " (List.map format_atm atms))
    | Operator (op, lhs, rhs) ->
        Printf.sprintf "%s %s %s" (format_atm lhs) (print_math_op op)
          (format_atm rhs)

  let format_statement stmt =
    match stmt with
    | Assign (name, expr) -> Printf.sprintf "  %s <- %s" name (format_exp expr)
    | Return expr -> Printf.sprintf "  return %s" (format_exp expr)
    | CMov (cond, idx) -> Printf.sprintf "  if %s go %d" (format_atm cond) idx
    | Jmp idx -> Printf.sprintf "  jmp %d" idx
    | Label idx -> Printf.sprintf "label%d:" idx
end

module AsmGenerator = struct
  type reg =
    | X0
    | X1
    | X2
    | X3
    | X4
    | X5
    | X6
    | X7
    | X8
    | X9
    | X10
    | X11
    | X12
    | X13
    | X14
    | X15
    | X16
    | X17
    | X18
    | X19
    | X20
    | X21
    | X22
    | X23
    | X24
    | X25
    | X26
    | X27
    | X28
    | X29
    | X30
    | SP of int

  type cmd =
    | STP of reg * reg * int
    | STR of reg * int
    | LDP of reg * reg * int
    | LDR of reg * int
    | SUBSP of int
    | ADDSP of int
    | ADD of reg * reg * reg
    | SUB of reg * reg * reg
end

(*
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
