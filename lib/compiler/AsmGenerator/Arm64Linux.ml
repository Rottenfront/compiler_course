open Monadic
open BasicInstructions
open AssignHomes
open Utils

let align16 n = (n + 15) / 16 * 16
let add_line out s = Buffer.add_string out (s ^ "\n")

let load_integer out reg value =
  let value = Int64.of_int value in
  for i = 0 to 3 do
    let chunk =
      Int64.logand (Int64.shift_right_logical value (16 * i)) 0xffffL
    in
    if i = 0 then add_line out (Format.sprintf "    movz %s, #%Ld" reg chunk)
    else if chunk <> 0L then
      add_line out
        (Format.sprintf "    movk %s, #%Ld, lsl #%d" reg chunk (16 * i))
  done

(* Homes are relative to the saved frame pointer, so outgoing arguments do not
   change local addresses. Negative homes denote incoming stack arguments. *)
let stack_address out index =
  load_integer out "x12" (abs index * 8);
  add_line out
    (if index >= 0 then "    sub x12, x29, x12" else "    add x12, x29, x12")

let load_data out reg = function
  | BasicInt value -> load_integer out reg value
  | BasicReg (Reg source) ->
      add_line out (Format.sprintf "    mov %s, %s" reg source)
  | BasicReg (Stack index) ->
      stack_address out index;
      add_line out (Format.sprintf "    ldr %s, [x12]" reg)

let store_data out home reg =
  match home with
  | Reg dest -> add_line out (Format.sprintf "    mov %s, %s" dest reg)
  | Stack index ->
      stack_address out index;
      add_line out (Format.sprintf "    str %s, [x12]" reg)

let compile_instruction out function_name instruction =
  let label index = Format.sprintf ".L%s_%d" function_name index in
  match instruction with
  | MovIns (dest, source) ->
      load_data out "x9" source;
      store_data out dest "x9"
  | OpIns (dest, op, lhs, rhs) ->
      load_data out "x9" lhs;
      load_data out "x10" rhs;
      (match op with
      | OpAdd -> add_line out "    add x9, x9, x10"
      | OpSub -> add_line out "    sub x9, x9, x10"
      | OpMul -> add_line out "    mul x9, x9, x10"
      | OpDiv -> add_line out "    sdiv x9, x9, x10"
      | OpXor -> add_line out "    eor x9, x9, x10"
      | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
          let condition =
            match op with
            | OpEq -> "eq"
            | OpNe -> "ne"
            | OpLess -> "lt"
            | OpGreater -> "gt"
            | OpLessEq -> "le"
            | OpGreaterEq -> "ge"
            | _ -> assert false
          in
          add_line out "    cmp x9, x10";
          add_line out ("    cset x9, " ^ condition));
      store_data out dest "x9"
  | CMovIns (test, target) ->
      load_data out "x9" test;
      add_line out ("    cbnz x9, " ^ label target)
  | JmpIns target -> add_line out ("    b " ^ label target)
  | LblIns target -> add_line out (label target ^ ":")
  | FuncIns (dest, name, args) ->
      let rest = List.drop 8 args in
      let outgoing = align16 (List.length rest * 8) in
      if outgoing > 0 then (
        load_integer out "x9" outgoing;
        add_line out "    sub sp, sp, x9");
      List.iteri
        (fun index arg ->
          load_data out "x9" arg;
          load_integer out "x12" (index * 8);
          add_line out "    add x12, sp, x12";
          add_line out "    str x9, [x12]")
        rest;
      List.iteri
        (fun index arg -> load_data out ("x" ^ string_of_int index) arg)
        (List.take 8 args);
      if dest = Reg "x0" && name = function_name then (
        (* Stage all stack arguments before overwriting any incoming home. *)
        List.iteri
          (fun index _ ->
            load_integer out "x12" (index * 8);
            add_line out "    add x12, sp, x12";
            add_line out "    ldr x9, [x12]";
            store_data out (Stack (-2 - index)) "x9")
          rest;
        if outgoing > 0 then (
          load_integer out "x9" outgoing;
          add_line out "    add sp, sp, x9");
        add_line out ("    b .Lstart_" ^ function_name))
      else (
        add_line out ("    bl " ^ name);
        if outgoing > 0 then (
          load_integer out "x9" outgoing;
          add_line out "    add sp, sp, x9");
        store_data out dest "x0")

let compile_function name parameters statements =
  let homes =
    parameters
    |> List.mapi (fun index name ->
        (name, Stack (if index < 8 then index + 1 else -2 - (index - 8))))
    |> StringMap.of_list
  in
  (* Never recycle slots across branches: linear usage counts do not model
     control-flow liveness, and can overwrite values still needed at a join. *)
  let next = ref (min 8 (List.length parameters) + 1) in
  let homes =
    List.fold_left
      (fun homes statement ->
        match statement with
        | ExplicateControl.Assign (name, _) when not (StringMap.mem name homes)
          ->
            let index = !next in
            incr next;
            StringMap.add name (Stack index) homes
        | _ -> homes)
      homes statements
  in
  let frame_size = align16 ((!next - 1) * 8) in
  let out = Buffer.create 1024 in
  add_line out ".text";
  add_line out ".balign 4";
  add_line out (".global " ^ name);
  add_line out (".type " ^ name ^ ", %function");
  add_line out (name ^ ":");
  add_line out "    stp x29, x30, [sp, #-16]!";
  add_line out "    mov x29, sp";
  load_integer out "x9" frame_size;
  add_line out "    sub sp, sp, x9";
  (* Tail calls restart after frame allocation and refresh register parameters. *)
  add_line out (".Lstart_" ^ name ^ ":");
  List.iteri
    (fun index _ ->
      store_data out (Stack (index + 1)) ("x" ^ string_of_int index))
    (List.take 8 parameters);
  List.iter
    (fun statement ->
      BasicInstructions.compile_statement statement homes
      |> compile_instruction out name)
    statements;
  add_line out "    mov sp, x29";
  add_line out "    ldp x29, x30, [sp], #16";
  add_line out "    ret";
  add_line out (".size " ^ name ^ ", .-" ^ name);
  Buffer.contents out

type function_data = Generator.function_data

let runtime =
  {|.section .rodata
fmt_true: .asciz "true\n"
fmt_false: .asciz "false\n"
fmt_read: .asciz "%lld"
fmt_int: .asciz "%lld\n"
.text
.balign 4
.global print_bool
print_bool:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    cmp x0, #0
    adrp x0, fmt_false
    add x0, x0, :lo12:fmt_false
    adrp x1, fmt_true
    add x1, x1, :lo12:fmt_true
    csel x0, x1, x0, ne
    bl printf
    mov x0, #0
    ldp x29, x30, [sp], #16
    ret
.global print_int
print_int:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    mov x1, x0
    adrp x0, fmt_int
    add x0, x0, :lo12:fmt_int
    bl printf
    mov x0, #0
    ldp x29, x30, [sp], #16
    ret
.global read
read:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    str xzr, [sp]
    mov x1, sp
    adrp x0, fmt_read
    add x0, x0, :lo12:fmt_read
    bl scanf
    ldr x0, [sp]
    add sp, sp, #16
    ldp x29, x30, [sp], #16
    ret
|}

let compile_code (functions : function_data list) =
  runtime
  ^ String.concat "\n"
      (List.map
         (fun ({ name; parameters; statements } : function_data) ->
           compile_function name parameters statements)
         functions)
  ^ "\n.section .note.GNU-stack,\"\",%progbits\n"
