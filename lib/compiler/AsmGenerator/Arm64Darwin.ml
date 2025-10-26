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
      |> List.mapi (fun index arg -> load_arg 0 ("x" ^ string_of_int index) arg)
      |> List.fold_left (fun _ _ -> ()) ();
      if dest = Reg "x0" && name = function_name then
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
          add_line out (Format.sprintf "    b start_%s" function_name);
          add_line out (Format.sprintf "    add sp, sp, #%d" shift))
        else add_line out (Format.sprintf "    b start_%s" function_name)
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

type function_data = Generator.function_data

let rec compile_functions (functions : function_data list)
    (declarations : string list) (cons : string) =
  match functions with
  | [] -> cons
  | { name; parameters; statements } :: rest ->
      let params_map =
        parameters |> List.map (fun param -> (param, 0)) |> StringMap.of_list
      in
      let variables = AssignHomes.analyze_variable_use statements params_map in
      let stack_list =
        List.take 8 parameters
        |> List.fold_left_map (fun index name -> (index + 1, (name, index))) 1
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
