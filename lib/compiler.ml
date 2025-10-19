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

module AssignHomes = struct
  open Monadic
  open ExplicateControl

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

  let temp_regs = [ "x9"; "x10"; "x11"; "x12"; "x13"; "x14"; "x15" ]

  type reg = Reg of string | Stack of int

  type assign_homes_context = {
    variables : int StringMap.t;
    register_table : string option StringMap.t;
    stack_table : int StringMap.t;
    free_stack : int list;
  }

  (* first 8 arguments should be pushed into stack
     rest should be with indices -2, -3, -4, etc.*)
  let rec assign_homes statements context (homes : reg StringMap.t) =
    let analyze_atm atm context =
      match atm with
      | AtmVar var ->
          let variables =
            StringMap.update var (Option.map (fun x -> x - 1)) context.variables
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
          |> List.fold_left (fun context atm -> analyze_atm atm context) context
      | Operator (_, lhs, rhs) ->
          [ lhs; rhs ]
          |> List.fold_left (fun context atm -> analyze_atm atm context) context
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
  open Monadic
  open BasicInstructions
  open AssignHomes

  let align16 n = (n + 15) / 16 * 16
  let add_line acc s = acc := !acc ^ s ^ "\n"
  let buffer_reg = "x16"
  let second_buffer_reg = "x20"
  let additional_buffer_reg = "x21"
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
                  Format.sprintf "    mov %s, %d" buffer_reg int |> add_line out;
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
              add_line out (Format.sprintf "    add %s, %s, %s" dest lhs rhs)
          | OpSub ->
              add_line out (Format.sprintf "    sub %s, %s, %s" dest lhs rhs)
          | OpMul ->
              add_line out (Format.sprintf "    mul %s, %s, %s" dest lhs rhs)
          | OpDiv ->
              add_line out (Format.sprintf "    sdiv %s, %s, %s" dest lhs rhs)
          | OpXor ->
              add_line out (Format.sprintf "    eor %s, %s, %s" dest lhs rhs)
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
        if dest == Reg "x0" && name == function_name then (
          if List.length args > 8 then (
            let rest_args =
              args |> List.rev |> List.take (List.length args - 8) |> List.rev
            in
            let shift = List.length rest_args |> ( * ) 8 |> align16 in
            add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
            List.mapi
              (fun index arg ->
                load_arg shift buffer_reg arg;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" buffer_reg (index * 8)))
              rest_args
            |> List.fold_left (fun _ _ -> ()) ();
            List.mapi
              (fun index _ ->
                add_line out
                  (Format.sprintf "    ldr %s, [sp, #%d]" buffer_reg (index * 8));
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
              args |> List.rev |> List.take (List.length args - 8) |> List.rev
            in
            let shift = List.length rest_args |> align16 in
            add_line out (Format.sprintf "    sub sp, sp, #%d" shift);
            List.mapi
              (fun index arg ->
                load_arg shift buffer_reg arg;
                add_line out
                  (Format.sprintf "    str %s, [sp, #%d]" buffer_reg (index * 8)))
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
          match home with Stack value -> max value max_width | _ -> max_width)
        homes 0
      |> ( * ) 8 |> align16
    in
    let out = ref "" in
    add_line out ".text";
    add_line out (".global _" ^ name);
    add_line out ("_" ^ name ^ ":");
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
      (fun instruction -> compile_instruction out name frame_size instruction)
      instructions
    |> List.fold_left (fun _ _ -> ()) ();

    add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
    add_line out "    ldp fp, lr, [sp], #16";
    add_line out "    ret";
    !out
end

type implementation = Parser.implementation
type declaration = Parser.declaration
type substring = Parser.substring

let rec compile_functions (functions : implementation list)
    (declarations : string list) count cons =
  match functions with
  | [] -> cons
  | { position = _; name; parameters; expression } :: rest ->
      let name = name.str in
      let params =
        List.map (fun (substr : substring) -> substr.str) parameters
      in
      let params_map =
        List.append declarations params
        |> List.map (fun name -> (name, name))
        |> StringMap.of_list
      in
      print_endline "uniquify";
      let expr, count = expression |> Uniquify.uniquify_expr count params_map in
      let params_map =
        params |> List.map (fun param -> (param, true)) |> StringMap.of_list
      in
      print_endline "monadize";
      let expr, count = Monadic.remove_complex_operands params_map count expr in
      print_endline "explicate control";
      let stmts, _ = ExplicateControl.explicate_control expr None count in
      stmts
      |> List.map (fun stmt ->
             print_endline (ExplicateControl.format_statement stmt))
      |> List.fold_left (fun _ _ -> ()) ();
      let params_map =
        params |> List.map (fun param -> (param, 0)) |> StringMap.of_list
      in
      print_endline "analyze variable use";
      let variables = AssignHomes.analyze_variable_use stmts params_map in
      let stack_list =
        List.take 8 params
        |> List.fold_left_map (fun index name -> (index + 1, (name, index))) 1
        |> snd
        |> List.append
             (List.drop 8 params
             |> List.fold_left_map
                  (fun index name -> (index - 1, (name, index)))
                  (-2)
             |> snd)
        |> StringMap.of_list
      in
      let homes =
        StringMap.map (fun index -> AssignHomes.Stack index) stack_list
      in
      print_endline "assign homes";
      let homes =
        AssignHomes.assign_homes stmts
          {
            variables;
            register_table = StringMap.empty;
            stack_table = stack_list;
            free_stack = [];
          }
          homes
      in
      (StringMap.mapi
         (fun name reg ->
           print_string (name ^ ": ");
           (match reg with
           | AssignHomes.Reg reg -> print_string ("reg " ^ reg)
           | AssignHomes.Stack index ->
               print_string ("sp #" ^ string_of_int index));
           print_endline "")
         homes
      |> StringMap.fold (fun _ _ _ -> ()))
        ();
      let instructions =
        List.map
          (fun stmt -> BasicInstructions.compile_statement stmt homes)
          stmts
      in
      let cons =
        cons ^ "\n"
        ^ AsmGenerator.compile_function name params instructions homes
      in
      compile_functions rest declarations count cons

let compile_code code =
  let builtin_functions = [ "read"; "print_int"; "print_bool" ] in
  let functions, _ = code |> Lexer.lexer |> Parser.parse_stmts [] [] in
  match functions with
  | Ok (declarations, functions) ->
      let declarations =
        declarations
        |> List.map (fun (decl : declaration) -> decl.name.str)
        |> List.append builtin_functions
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
      Ok (compile_functions functions declarations 0 builtin)
  | Error err -> Error err

let compile_to_file file code =
  match compile_code code with
  | Ok code ->
      let file = open_out file in
      Printf.fprintf file "%s" code;
      close_out file
  | Error error -> Parser.print_error error |> Printf.printf "%s\n"

let compile_file_to_file input output =
  let input = open_in input in
  let code = In_channel.input_all input in
  compile_to_file output code;
  close_in input
