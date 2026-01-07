module Compiler = struct
  module Uniquify = struct
    open Utils
    open Ast

    type uniquified_expr =
      | TmInt of int
      | TmBool of bool
      | TmVar of string
      | TmCall of string * uniquified_expr list
      | TmIf of uniquified_expr * uniquified_expr * uniquified_expr
      | TmLet of string * uniquified_expr * uniquified_expr
      | TmSeq of uniquified_expr list
      | TmCreateTuple of uniquified_expr list
      | TmAccessTuple of int * uniquified_expr

    let rec uniquify_expr count (context : string StringMap.t) expr =
      let rec uniquify_list count context prev exprs =
        match exprs with
        | expr :: rest ->
            let expr, count = uniquify_expr count context expr in
            uniquify_list count context (expr :: prev) rest
        | [] -> (List.rev prev, count)
      in
      match expr with
      | Int int -> (TmInt int, count)
      | Bool bool -> (TmBool bool, count)
      | Var name -> (TmVar (StringMap.find name context), count)
      | Call (name, arguments) ->
          let arguments, count = uniquify_list count context [] arguments in
          (TmCall (name, arguments), count)
      | If (cond, tru, fls) ->
          let cond, count = uniquify_expr count context cond in
          let tru, count = uniquify_expr count context tru in
          let fls, count = uniquify_expr count context fls in
          (TmIf (cond, tru, fls), count)
      | Let ([], expr) -> uniquify_expr count context expr
      | Let ((name, value) :: rest, expr) ->
          let value, count = uniquify_expr count context value in
          let new_name = Format.sprintf "%s.%d" name count in
          let context = StringMap.add name new_name context in
          let count = count + 1 in
          let expression, count =
            uniquify_expr count context (Let (rest, expr))
          in
          (TmLet (new_name, value, expression), count)
      | Tuple values ->
          let values, count = uniquify_list count context [] values in
          (TmCreateTuple values, count)
      | AccessTuple (index, value) ->
          let value, count = uniquify_expr count context value in
          (TmAccessTuple (index, value), count)
  end

  module Monadic = struct
    open Uniquify
    open Utils

    type atm = AtmVar of string | AtmInt of int | AtmBool of bool
    type builtin_op = OpAnd | OpOr

    let parse_builtin_op func =
      match func with
      | "&&" | "and" -> Some OpAnd
      | "||" | "or" -> Some OpOr
      | _ -> None

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

    let parse_math_op func =
      match func with
      | "+" -> Some OpAdd
      | "-" -> Some OpSub
      | "*" -> Some OpMul
      | "/" -> Some OpDiv
      | "==" | "eq?" -> Some OpEq
      | "!=" | "ne?" -> Some OpNe
      | "<" | "lt?" -> Some OpLess
      | ">" | "gt?" -> Some OpGreater
      | "<=" | "le?" -> Some OpLessEq
      | ">=" | "ge?" -> Some OpGreaterEq
      | "^" | "xor" -> Some OpXor
      | _ -> None

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
      | AtmCall of string * atm list
      | AtmOp of math_op * atm * atm
      | AtmIf of atm * node * node
      | AtmCreateTuple of atm list
        (* field tag and variable name (if it is a complex operand, it should
           be incapsulated into Let)*)
      | AtmAccessTuple of int * string
      | Let of string * node * node
      | Sequence of node * node

    type simplified = Node of node | Atm of atm

    let rec remove_complex_operands count expr =
      let rec simplify_list count exprs result =
        match exprs with
        | [] -> (List.rev result, count)
        | expr :: exprs ->
            let expr, count = remove_complex_operands count expr in
            simplify_list count exprs (expr :: result)
      in
      let rec simplify_sequence exprs =
        match exprs with
        | [] -> failwith "Empty sequence"
        | [ first ] -> first
        | first :: rest -> Sequence (first, simplify_sequence rest)
      in
      let rec simplify_call count name exprs prev_exprs =
        match exprs with
        | [] -> (AtmCall (name, List.rev prev_exprs), count)
        | expr :: rest -> (
            match expr with
            | AtmValue value ->
                simplify_call count name rest (value :: prev_exprs)
            | complex_expr ->
                let variable_name = Format.sprintf "tmp.%d" count in
                let count = count + 1 in
                let inner, count =
                  simplify_call count name rest
                    (AtmVar variable_name :: prev_exprs)
                in
                (Let (variable_name, complex_expr, inner), count))
      in
      let rec simplify_tuple count exprs prev_exprs =
        match exprs with
        | [] -> (AtmCreateTuple (List.rev prev_exprs), count)
        | expr :: rest -> (
            match expr with
            | AtmValue value -> simplify_tuple count rest (value :: prev_exprs)
            | complex_expr ->
                let variable_name = Format.sprintf "tmp.%d" count in
                let count = count + 1 in
                let inner, count =
                  simplify_tuple count rest (AtmVar variable_name :: prev_exprs)
                in
                (Let (variable_name, complex_expr, inner), count))
      in
      match expr with
      | TmInt value -> (AtmValue (AtmInt value), count)
      | TmBool value -> (AtmValue (AtmBool value), count)
      | TmVar value -> (AtmValue (AtmVar value), count)
      | TmCall (name, exprs) ->
          let exprs, count = simplify_list count exprs [] in
          simplify_call count name exprs []
      | TmIf (cond, tru, fls) -> (
          let cond, count = remove_complex_operands count cond in
          let tru, count = remove_complex_operands count tru in
          let fls, count = remove_complex_operands count fls in
          match cond with
          | AtmValue atm -> (AtmIf (atm, tru, fls), count)
          | complex_expr ->
              let variable_name = Format.sprintf "tmp.%d" count in
              let count = count + 1 in
              ( Let
                  ( variable_name,
                    complex_expr,
                    AtmIf (AtmVar variable_name, tru, fls) ),
                count ))
      | TmCreateTuple exprs ->
          let exprs, count = simplify_list count exprs [] in
          simplify_tuple count exprs []
      | TmAccessTuple (tag, expr) -> (
          let expr, count = remove_complex_operands count expr in
          match expr with
          | AtmValue atm -> (
              match atm with
              | AtmVar var -> (AtmAccessTuple (tag, var), count)
              | _ -> failwith "Type error: cannot use AccessTuple on literals")
          | complex_expr ->
              let variable_name = Format.sprintf "tmp.%d" count in
              let count = count + 1 in
              ( Let
                  ( variable_name,
                    complex_expr,
                    AtmAccessTuple (tag, variable_name) ),
                count ))
      | TmLet (name, value, expr) ->
          let value, count = remove_complex_operands count value in
          let expr, count = remove_complex_operands count expr in
          (Let (name, value, expr), count)
      | TmSeq exprs ->
          let exprs, count = simplify_list count exprs [] in
          (simplify_sequence exprs, count)

    let print_atm atm =
      match atm with
      | AtmVar name -> name
      | AtmInt int -> string_of_int int
      | AtmBool bool -> string_of_bool bool

    let rec print_monadic expr =
      match expr with
      | AtmValue atm -> print_atm atm
      | AtmCall (name, parameters) ->
          "(" ^ name
          ^ (List.map (fun param -> " " ^ print_atm param) parameters
            |> String.concat "")
          ^ ")"
      | AtmOp (op, lhs, rhs) ->
          "(" ^ print_math_op op ^ " " ^ print_atm lhs ^ " " ^ print_atm rhs
          ^ ")"
      | AtmIf (cond, lhs, rhs) ->
          "(if " ^ print_atm cond ^ " " ^ print_monadic lhs ^ " "
          ^ print_monadic rhs ^ ")"
      | Let (name, value, expression) ->
          "(let [" ^ name ^ " " ^ print_monadic value ^ "] "
          ^ print_monadic expression ^ ")"
      | Sequence (lhs, rhs) ->
          "(begin " ^ print_monadic lhs ^ " " ^ print_monadic rhs ^ ")"
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
      | AtmCall (func, args) ->
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
      | Assign (name, expr) ->
          Format.sprintf "  %s <- %s" name (format_exp expr)
      | Return expr -> Format.sprintf "  return %s" (format_exp expr)
      | CMov (cond, idx) -> Format.sprintf "  if %s go %d" (format_atm cond) idx
      | Jmp idx -> Format.sprintf "  jmp %d" idx
      | Label idx -> Format.sprintf "label%d:" idx
  end

  module AssignHomes = struct
    open Monadic
    open ExplicateControl
    open Utils

    (* there should be parameters already *)
    let rec analyze_variable_use statements variables =
      let analyze_atm atm variables =
        match atm with
        | AtmVar var ->
            StringMap.update var (Option.map (fun x -> x + 1)) variables
        | _ -> variables
      in
      let analyze_expression expr variables =
        match expr with
        | Atm atm -> analyze_atm atm variables
        | Function (name, atms) ->
            AtmVar name :: atms
            |> List.fold_left (fun acc atm -> analyze_atm atm acc) variables
        | Operator (_, lhs, rhs) ->
            [ lhs; rhs ]
            |> List.fold_left (fun acc atm -> analyze_atm atm acc) variables
      in
      match statements with
      | Assign (new_variable, expr) :: rest ->
          variables
          |> StringMap.add new_variable 1
          |> analyze_expression expr |> analyze_variable_use rest
      | Return expr :: rest ->
          variables |> analyze_expression expr |> analyze_variable_use rest
      | CMov (atm, _) :: rest ->
          variables |> analyze_atm atm |> analyze_variable_use rest
      | _ :: rest -> analyze_variable_use rest variables
      | [] -> variables

    type reg = Reg of string | Stack of int

    type assign_homes_context = {
      variables : int StringMap.t;
      register_table : string option StringMap.t;
      stack_table : int StringMap.t;
      free_stack : int list;
    }

    (* first 8 or 6 (based on architecture) arguments should be pushed into stack
       rest should be with indices -2, -3, -4, etc.*)
    let rec assign_homes statements context (homes : reg StringMap.t) =
      let analyze_atm atm context =
        match atm with
        | AtmVar var ->
            let variables =
              StringMap.update var
                (Option.map (fun x -> x - 1))
                context.variables
            in
            if StringMap.find var variables == 0 then
              if
                StringMap.exists
                  (fun _ variable -> variable == Some var)
                  context.register_table
              then
                {
                  variables;
                  register_table =
                    StringMap.map
                      (fun variable ->
                        if variable == Some var then None else variable)
                      context.register_table;
                  stack_table = context.stack_table;
                  free_stack = context.free_stack;
                }
              else
                let stack_position = StringMap.find var context.stack_table in
                {
                  variables;
                  register_table = context.register_table;
                  stack_table = StringMap.remove var context.stack_table;
                  free_stack = stack_position :: context.free_stack;
                }
            else { context with variables }
        | _ -> context
      in
      let analyze_expression expr context =
        match expr with
        | Atm atm -> analyze_atm atm context
        | Function (name, atms) ->
            (if StringMap.mem name context.variables then AtmVar name :: atms
             else atms)
            |> List.fold_left
                 (fun context atm -> analyze_atm atm context)
                 context
        | Operator (_, lhs, rhs) ->
            [ lhs; rhs ]
            |> List.fold_left
                 (fun context atm -> analyze_atm atm context)
                 context
      in
      match statements with
      | [] -> homes
      | Assign (new_variable, expr) :: rest ->
          let context = analyze_expression expr context in
          let context, homes =
            if
              StringMap.for_all
                (fun _ variable -> variable != None)
                context.register_table
            then
              let index =
                if List.is_empty context.free_stack then
                  StringMap.fold
                    (fun _ index max_index -> max max_index (index + 1))
                    context.stack_table 1
                else List.hd context.free_stack
              in
              ( {
                  context with
                  stack_table =
                    StringMap.add new_variable index context.stack_table;
                },
                StringMap.add new_variable (Stack index) homes )
            else
              let reg =
                StringMap.bindings context.register_table
                |> List.filter (fun (_, var) -> var = None)
                |> List.map fst |> List.hd
              in
              ( {
                  context with
                  register_table =
                    StringMap.update reg
                      (fun _ -> Some (Some new_variable))
                      context.register_table;
                },
                StringMap.add new_variable (Reg reg) homes )
          in
          assign_homes rest context homes
      | Return expr :: rest ->
          let context = analyze_expression expr context in
          assign_homes rest context homes
      | CMov (atm, _) :: rest ->
          let context = analyze_atm atm context in
          assign_homes rest context homes
      | _ :: rest -> assign_homes rest context homes
  end

  module BasicInstructions = struct
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
  end

  module AsmGenerator = struct
    module Arm64Darwin = struct
      open Monadic
      open BasicInstructions
      open AssignHomes
      open Utils

      let align16 n = (n + 15) / 16 * 16
      let add_line acc s = acc := !acc ^ s ^ "\n"
      let buffer_reg = "x9"
      let second_buffer_reg = "x10"
      let additional_buffer_reg = "x11"

      (* let temp_regs =
        [ "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25"; "x26"; "x27"; "x28" ] *)
      let temp_regs = []
      let input_regs = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ]
      let stack_position sp_shift index = sp_shift - (index * 8)

      let compile_instruction out function_name sp_shift instruction =
        match instruction with
        | MovIns (dest, data) -> (
            match dest with
            | Reg dest -> (
                match data with
                | BasicInt int ->
                    Format.sprintf "    mov %s, %d" dest int |> add_line out
                | BasicReg (Reg src) ->
                    Format.sprintf "    mov %s, %s" dest src |> add_line out
                | BasicReg (Stack index) ->
                    Format.sprintf "    ldr %s, [sp, #%d]" dest
                      (stack_position sp_shift index)
                    |> add_line out)
            | Stack dest ->
                let src =
                  match data with
                  | BasicInt int ->
                      Format.sprintf "    mov %s, %d" buffer_reg int
                      |> add_line out;
                      buffer_reg
                  | BasicReg (Reg src) -> src
                  | BasicReg (Stack src) ->
                      Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                        (stack_position sp_shift src)
                      |> add_line out;
                      buffer_reg
                in
                Format.sprintf "    str %s, [sp, #%d]" src
                  (stack_position sp_shift dest)
                |> add_line out)
        | OpIns (dest, op, lhs, rhs) -> (
            let lhs =
              match lhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            let rhs =
              match rhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" additional_buffer_reg int
                  |> add_line out;
                  additional_buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" additional_buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  additional_buffer_reg
            in
            let compile_operator =
             fun dest op ->
              match op with
              | OpAdd ->
                  add_line out
                    (Format.sprintf "    add %s, %s, %s" dest lhs rhs)
              | OpSub ->
                  add_line out
                    (Format.sprintf "    sub %s, %s, %s" dest lhs rhs)
              | OpMul ->
                  add_line out
                    (Format.sprintf "    mul %s, %s, %s" dest lhs rhs)
              | OpDiv ->
                  add_line out
                    (Format.sprintf "    sdiv %s, %s, %s" dest lhs rhs)
              | OpXor ->
                  add_line out
                    (Format.sprintf "    eor %s, %s, %s" dest lhs rhs)
              | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
                  add_line out (Format.sprintf "    cmp %s, %s" lhs rhs);
                  let cond =
                    match op with
                    | OpEq -> "eq"
                    | OpNe -> "ne"
                    | OpLess -> "lt"
                    | OpGreater -> "gt"
                    | OpLessEq -> "le"
                    | OpGreaterEq -> "ge"
                    | _ -> assert false
                  in
                  add_line out (Format.sprintf "    cset %s, %s" dest cond)
            in
            match dest with
            | Reg reg -> compile_operator reg op
            | Stack index ->
                compile_operator second_buffer_reg op;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" second_buffer_reg
                     (stack_position sp_shift index)))
        | CMovIns (cond, label) ->
            let cond =
              match cond with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            add_line out (Format.sprintf "    cbnz %s, label_%d" cond label)
        | JmpIns label -> add_line out (Format.sprintf "    b label_%d" label)
        | LblIns label -> add_line out (Format.sprintf "label_%d:" label)
        | FuncIns (dest, name, args) ->
            let load_arg =
             fun shift dest arg ->
              match arg with
              | BasicReg (Reg src) ->
                  Format.sprintf "    mov %s, %s" dest src |> add_line out
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" dest
                    (stack_position (sp_shift + shift) src)
                  |> add_line out
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" dest int |> add_line out
            in
            (* tail recursion *)
            List.take 8 args
            |> List.mapi (fun index arg ->
                load_arg 0 ("x" ^ string_of_int index) arg)
            |> List.fold_left (fun _ _ -> ()) ();
            if dest = Reg "x0" && name = function_name then
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> ( * ) 8 |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                List.mapi
                  (fun index _ ->
                    add_line out
                      (Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                         (index * 8));
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (stack_position (sp_shift + shift) (-1 - index))))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    b start_%s" function_name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    b start_%s" function_name)
            else (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    bl _%s" name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    bl _%s" name);
              match dest with
              | Stack index ->
                  add_line out
                    (Format.sprintf "    str x0, [sp, #%d]"
                       (stack_position sp_shift index))
              | Reg reg -> add_line out (Format.sprintf "    mov %s, x0" reg))

      let compile_function name params instructions (homes : reg StringMap.t) =
        let frame_size =
          StringMap.fold
            (fun _ home max_width ->
              match home with
              | Stack value -> max value max_width
              | _ -> max_width)
            homes 0
          |> ( * ) 8 |> align16
        in
        let out = ref "" in
        add_line out ".text";
        add_line out (".global _" ^ name);
        add_line out ("_" ^ name ^ ":");
        add_line out "    stp fp, lr, [sp, #-16]!";
        add_line out (Format.sprintf "    sub sp, sp, #%d" frame_size);
        add_line out ("start_" ^ name ^ ":");

        List.take 8 params
        |> List.fold_left
             (fun index _ ->
               add_line out
                 (Format.sprintf "    str x%d, [sp, #%d]" index
                    (stack_position frame_size (index + 1)));
               index + 1)
             0
        |> fun _ ->
        ();

        List.map
          (fun instruction ->
            compile_instruction out name frame_size instruction)
          instructions
        |> List.fold_left (fun _ _ -> ()) ();

        add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
        add_line out "    ldp fp, lr, [sp], #16";
        add_line out "    ret";
        !out

      type function_data = Generator.function_data

      let rec compile_functions (functions : function_data list)
          (declarations : string list) (cons : string) =
        match functions with
        | [] -> cons
        | { name; parameters; statements } :: rest ->
            let params_map =
              parameters
              |> List.map (fun param -> (param, 0))
              |> StringMap.of_list
            in
            let variables =
              AssignHomes.analyze_variable_use statements params_map
            in
            let stack_list =
              List.take 8 parameters
              |> List.fold_left_map
                   (fun index name -> (index + 1, (name, index)))
                   1
              |> snd
              |> List.append
                   (List.drop 8 parameters
                   |> List.fold_left_map
                        (fun index name -> (index - 1, (name, index)))
                        (-2)
                   |> snd)
              |> StringMap.of_list
            in
            let homes =
              StringMap.map (fun index -> AssignHomes.Stack index) stack_list
            in
            let homes =
              AssignHomes.assign_homes statements
                {
                  variables;
                  register_table =
                    temp_regs
                    |> List.map (fun reg -> (reg, None))
                    |> StringMap.of_list;
                  stack_table = stack_list;
                  free_stack = [];
                }
                homes
            in
            let instructions =
              List.map
                (fun stmt -> BasicInstructions.compile_statement stmt homes)
                statements
            in
            let cons =
              cons ^ "\n" ^ compile_function name parameters instructions homes
            in
            compile_functions rest declarations cons

      let compile_code functions =
        let declarations =
          functions
          |> List.map (fun (func : function_data) -> func.name)
          |> List.append Generator.builtin_functions
        in
        let builtin =
          "\n\
           .text\n\
           .global _print_bool\n\
           _print_bool:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    sub sp, sp, #16\n\
          \    cmp w0, #0\n\
          \    adrp x0, fmt_false@PAGE\n\
          \    add  x0, x0, fmt_false@PAGEOFF\n\
          \    b.eq 1f\n\
          \    adrp x0, fmt_true@PAGE\n\
          \    add  x0, x0, fmt_true@PAGEOFF\n\
           1:\n\
          \    bl _printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global _print_int\n\
           _print_int:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    mov x19, x0\n\
          \    ADRP X0, fmt_int@PAGE\n\
          \    ADD X0, X0, fmt_int@PAGEOFF\n\
          \    STR x19, [SP, #-16]!\n\
          \    BL  _printf\n\
          \    mov x0, #0\n\
          \    ADD SP, SP, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global _read\n\
           _read:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    adrp x0, fmt_read@PAGE\n\
          \    add x0, x0, fmt_read@PAGEOFF\n\
          \    adrp x11, num@PAGE\n\
          \    add x11, x11, num@PAGEOFF\n\
          \    str x11, [SP, #-16]!\n\
          \    bl _scanf\n\
          \    add sp, sp, #16\n\
          \    adrp x11, num@PAGE\n\
          \    add x11, x11, num@PAGEOFF\n\
          \    ldr x0, [x11]\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\n\
           .data\n\n\
           .balign 4\n\
           fmt_true:\n\
          \    .asciz \"true\\n\"\n\
           .balign 4\n\
           fmt_false:\n\
          \    .asciz \"false\\n\"\n\
           .balign 4\n\
           fmt_read:\n\
          \    .asciz \"%lld\"\n\
           .balign 4\n\
           fmt_int:\n\
          \    .asciz \"%lld\\n\"\n\
           .balign 4\n\
           num:    .quad 0\n\
          \      "
        in
        compile_functions functions declarations builtin
    end

    module Arm64Linux = struct
      open Monadic
      open BasicInstructions
      open AssignHomes
      open Utils

      let align16 n = (n + 15) / 16 * 16
      let add_line acc s = acc := !acc ^ s ^ "\n"
      let buffer_reg = "x9"
      let second_buffer_reg = "x10"
      let additional_buffer_reg = "x11"

      (* let temp_regs =
        [ "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25"; "x26"; "x27"; "x28" ] *)
      let temp_regs = []
      let input_regs = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ]
      let stack_position sp_shift index = sp_shift - (index * 8)

      let compile_instruction out function_name sp_shift instruction =
        match instruction with
        | MovIns (dest, data) -> (
            match dest with
            | Reg dest -> (
                match data with
                | BasicInt int ->
                    Format.sprintf "    mov %s, %d" dest int |> add_line out
                | BasicReg (Reg src) ->
                    Format.sprintf "    mov %s, %s" dest src |> add_line out
                | BasicReg (Stack index) ->
                    Format.sprintf "    ldr %s, [sp, #%d]" dest
                      (stack_position sp_shift index)
                    |> add_line out)
            | Stack dest ->
                let src =
                  match data with
                  | BasicInt int ->
                      Format.sprintf "    mov %s, %d" buffer_reg int
                      |> add_line out;
                      buffer_reg
                  | BasicReg (Reg src) -> src
                  | BasicReg (Stack src) ->
                      Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                        (stack_position sp_shift src)
                      |> add_line out;
                      buffer_reg
                in
                Format.sprintf "    str %s, [sp, #%d]" src
                  (stack_position sp_shift dest)
                |> add_line out)
        | OpIns (dest, op, lhs, rhs) -> (
            let lhs =
              match lhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            let rhs =
              match rhs with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" additional_buffer_reg int
                  |> add_line out;
                  additional_buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" additional_buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  additional_buffer_reg
            in
            let compile_operator =
             fun dest op ->
              match op with
              | OpAdd ->
                  add_line out
                    (Format.sprintf "    add %s, %s, %s" dest lhs rhs)
              | OpSub ->
                  add_line out
                    (Format.sprintf "    sub %s, %s, %s" dest lhs rhs)
              | OpMul ->
                  add_line out
                    (Format.sprintf "    mul %s, %s, %s" dest lhs rhs)
              | OpDiv ->
                  add_line out
                    (Format.sprintf "    sdiv %s, %s, %s" dest lhs rhs)
              | OpXor ->
                  add_line out
                    (Format.sprintf "    eor %s, %s, %s" dest lhs rhs)
              | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
                  add_line out (Format.sprintf "    cmp %s, %s" lhs rhs);
                  let cond =
                    match op with
                    | OpEq -> "eq"
                    | OpNe -> "ne"
                    | OpLess -> "lt"
                    | OpGreater -> "gt"
                    | OpLessEq -> "le"
                    | OpGreaterEq -> "ge"
                    | _ -> assert false
                  in
                  add_line out (Format.sprintf "    cset %s, %s" dest cond)
            in
            match dest with
            | Reg reg -> compile_operator reg op
            | Stack index ->
                compile_operator second_buffer_reg op;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" second_buffer_reg
                     (stack_position sp_shift index)))
        | CMovIns (cond, label) ->
            let cond =
              match cond with
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
                  buffer_reg
              | BasicReg (Reg src) -> src
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                    (stack_position sp_shift src)
                  |> add_line out;
                  buffer_reg
            in
            add_line out (Format.sprintf "    cbnz %s, label_%d" cond label)
        | JmpIns label -> add_line out (Format.sprintf "    b label_%d" label)
        | LblIns label -> add_line out (Format.sprintf "label_%d:" label)
        | FuncIns (dest, name, args) ->
            let load_arg =
             fun shift dest arg ->
              match arg with
              | BasicReg (Reg src) ->
                  Format.sprintf "    mov %s, %s" dest src |> add_line out
              | BasicReg (Stack src) ->
                  Format.sprintf "    ldr %s, [sp, #%d]" dest
                    (stack_position (sp_shift + shift) src)
                  |> add_line out
              | BasicInt int ->
                  Format.sprintf "    mov %s, %d" dest int |> add_line out
            in
            (* tail recursion *)
            List.take 8 args
            |> List.mapi (fun index arg ->
                load_arg 0 ("x" ^ string_of_int index) arg)
            |> List.fold_left (fun _ _ -> ()) ();
            if dest = Reg "x0" && name = function_name then (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> ( * ) 8 |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                List.mapi
                  (fun index _ ->
                    add_line out
                      (Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg
                         (index * 8));
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (stack_position (sp_shift + shift) (-1 - index))))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else ();
              add_line out (Format.sprintf "    b start_%s" function_name))
            else (
              if List.length args > 8 then (
                let rest_args =
                  args |> List.rev
                  |> List.take (List.length args - 8)
                  |> List.rev
                in
                let shift = List.length rest_args |> align16 in
                add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
                List.mapi
                  (fun index arg ->
                    load_arg shift buffer_reg arg;
                    add_line out
                      (Format.sprintf "    str %s, [sp, #%d]" buffer_reg
                         (index * 8)))
                  rest_args
                |> List.fold_left (fun _ _ -> ()) ();
                add_line out (Format.sprintf "    bl %s" name);
                add_line out (Format.sprintf "    add sp, sp, #%d" shift))
              else add_line out (Format.sprintf "    bl %s" name);
              match dest with
              | Stack index ->
                  add_line out
                    (Format.sprintf "    str x0, [sp, #%d]"
                       (stack_position sp_shift index))
              | Reg reg -> add_line out (Format.sprintf "    mov %s, x0" reg))

      let compile_function name params instructions (homes : reg StringMap.t) =
        let frame_size =
          StringMap.fold
            (fun _ home max_width ->
              match home with
              | Stack value -> max value max_width
              | _ -> max_width)
            homes 0
          |> ( * ) 8 |> align16
        in
        let out = ref "" in
        add_line out ".text";
        add_line out (".global " ^ name);
        add_line out (name ^ ":");
        add_line out "    stp fp, lr, [sp, #-16]!";
        add_line out ("start_" ^ name ^ ":");
        add_line out (Format.sprintf "    sub sp, sp, #%d" frame_size);

        List.take 8 params
        |> List.fold_left
             (fun index _ ->
               add_line out
                 (Format.sprintf "    str x%d, [sp, #%d]" index
                    (stack_position frame_size (index + 1)));
               index + 1)
             0
        |> fun _ ->
        ();

        List.map
          (fun instruction ->
            compile_instruction out name frame_size instruction)
          instructions
        |> List.fold_left (fun _ _ -> ()) ();

        add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
        add_line out "    ldp fp, lr, [sp], #16";
        add_line out "    ret";
        !out

      type function_data = Generator.function_data

      let rec compile_functions (functions : function_data list)
          (declarations : string list) (cons : string) =
        match functions with
        | [] -> cons
        | { name; parameters; statements } :: rest ->
            let params_map =
              parameters
              |> List.map (fun param -> (param, 0))
              |> StringMap.of_list
            in
            let variables =
              AssignHomes.analyze_variable_use statements params_map
            in
            let stack_list =
              List.take 8 parameters
              |> List.fold_left_map
                   (fun index name -> (index + 1, (name, index)))
                   1
              |> snd
              |> List.append
                   (List.drop 8 parameters
                   |> List.fold_left_map
                        (fun index name -> (index - 1, (name, index)))
                        (-2)
                   |> snd)
              |> StringMap.of_list
            in
            let homes =
              StringMap.map (fun index -> AssignHomes.Stack index) stack_list
            in
            let homes =
              AssignHomes.assign_homes statements
                {
                  variables;
                  register_table =
                    temp_regs
                    |> List.map (fun reg -> (reg, None))
                    |> StringMap.of_list;
                  stack_table = stack_list;
                  free_stack = [];
                }
                homes
            in
            let instructions =
              List.map
                (fun stmt -> BasicInstructions.compile_statement stmt homes)
                statements
            in
            let cons =
              cons ^ "\n" ^ compile_function name parameters instructions homes
            in
            compile_functions rest declarations cons

      let compile_code functions =
        let declarations =
          functions
          |> List.map (fun (func : function_data) -> func.name)
          |> List.append Generator.builtin_functions
        in
        let builtin =
          "\n\
           .text\n\
           .global print_bool\n\
           print_bool:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    sub sp, sp, #16\n\
          \    cmp w0, #0\n\
          \    adr x0, fmt_false\n\
          \    b.eq 1f\n\
          \    adr x0, fmt_true\n\
           1:\n\
          \    bl printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global print_int\n\
           print_int:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    mov x19, x0\n\
          \    adr x0, fmt_int\n\
          \    str x19, [sp, #-16]!\n\
          \    bl  printf\n\
          \    mov x0, #0\n\
          \    add sp, sp, #16\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\
           .text\n\
           .global read\n\
           read:\n\
          \    stp fp, lr, [sp, #-16]!\n\
          \    adr x0, fmt_read\n\
          \    adr x11, num\n\
          \    str x11, [SP, #-16]!\n\
          \    bl scanf\n\
          \    add sp, sp, #16\n\
          \    adr x11, num\n\
          \    ldr x0, [x11]\n\
          \    ldp fp, lr, [sp], #16\n\
          \    ret\n\n\n\
           .data\n\n\
           .balign 4\n\
           fmt_true:\n\
          \    .asciz \"true\\n\"\n\
           .balign 4\n\
           fmt_false:\n\
          \    .asciz \"false\\n\"\n\
           .balign 4\n\
           fmt_read:\n\
          \    .asciz \"%lld\"\n\
           .balign 4\n\
           fmt_int:\n\
          \    .asciz \"%lld\\n\"\n\
           .balign 4\n\
           num:    .quad 0\n\
          \      "
        in
        compile_functions functions declarations builtin
    end

    type function_data = {
      name : string;
      parameters : string list;
      statements : ExplicateControl.stmt list;
    }

    let builtin_functions = [ "read"; "print_int"; "print_bool" ]
  end

  (*
  Passes:
  - Uniquify
  - Monadize
  - Explicate control
  - Assign homes
  - Generate basic instructions
  - Generate assembly
  *)
  open Utils
  open Parser.Ast

  let compile (verbose : bool) (target : Config.target_architecture)
      (functions : Parser.Ast.implementation list) =
    let declarations =
      functions
      |> List.map (fun (func : Parser.Ast.implementation) -> func.name)
      |> List.append AsmGenerator.builtin_functions
    in
    let compile_function verbose (func : Parser.Ast.implementation) :
        AsmGenerator.function_data =
      if verbose then (
        print_endline "function:";
        print_endline (Parser.Ast.print_ast func));
      let { name; parameters; expression } = func in
      let params_map =
        List.append declarations parameters
        |> List.map (fun name -> (name, name))
        |> StringMap.of_list
      in
      let expr, count = expression |> Uniquify.uniquify_expr 0 params_map in
      if verbose then (
        print_endline "uniquified expression:";
        print_endline (Parser.Ast.print_ast_expr expr));
      let params_map =
        parameters |> List.map (fun param -> (param, true)) |> StringMap.of_list
      in
      let expr, count = Monadic.remove_complex_operands params_map count expr in

      if verbose then (
        print_endline "monadized expression:";
        print_endline (Monadic.print_monadic expr));
      let statements, _ = ExplicateControl.explicate_control expr None count in
      { name; parameters; statements }
    in
    let functions = List.map (compile_function verbose) functions in
    match target with
    | Config.Arm64Darwin -> AsmGenerator.Arm64Darwin.compile_code functions
    | Config.Arm64Linux -> AsmGenerator.Arm64Linux.compile_code functions
end
