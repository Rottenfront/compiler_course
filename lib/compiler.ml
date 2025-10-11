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
  open Monadic
  open ExplicateControl

  let add_line acc s = acc := !acc ^ s ^ "\n"
  let caller_arg_regs = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ]
  let temp_regs = [ "x9"; "x10"; "x11"; "x12"; "x13"; "x14"; "x15" ]

  let scratch_reg =
    "x16" (* used transiently for memory <-> register moves when needed *)

  let align16 n = (n + 15) / 16 * 16

  let collect_locals params stmts =
    let set = Hashtbl.create 16 in
    List.iter (fun p -> Hashtbl.replace set p ()) params;
    List.iter
      (function Assign (v, _) -> Hashtbl.replace set v () | _ -> ())
      stmts;
    Hashtbl.fold (fun k _ acc -> k :: acc) set []

  let make_stack_layout locals =
    let slot_size = 8 in
    let rec assign acc idx = function
      | [] -> List.rev acc
      | name :: rest -> assign ((name, idx * slot_size) :: acc) (idx + 1) rest
    in
    assign [] 1 locals

  let lookup_offset layout name =
    try List.assoc name layout
    with Not_found -> failwith ("Unknown local: " ^ name)

  let label_name i = Format.sprintf "label%d" i

  (* ----------------------
   Two-pass analysis to compute precise spill area
   ----------------------
   We compute the maximum number of simultaneously live temporaries needed while
   evaluating expressions under a left-to-right evaluation strategy. The algorithm
   simulates the allocation pattern used by the emitter: evaluate left subtree fully,
   leaving its result live in one temporary, then evaluate right subtree (which may
   itself require temporaries while the left result occupies one). For function call
   arguments, we evaluate left-to-right; each evaluated arg remains live until moved
   into an argument register or stored to the preallocated arg stack area.
*)

  (* compute max temps needed by an expression *)
  let max_temps_exp e =
    match e with
    | Atm _ -> 1
    | Operator _ -> 2
    | Function (_, args) -> List.length args

  (* compute maximum temps needed across all statements *)
  let max_temps_stmts stmts =
    let rec loop acc = function
      | [] -> acc
      | Label _ :: rest | Jmp _ :: rest -> loop acc rest
      | CMov _ :: rest -> loop (max acc 1) rest
      | Assign (_, e) :: rest -> loop (max acc (max_temps_exp e)) rest
      | Return e :: rest -> loop (max acc (max_temps_exp e)) rest
    in
    loop 0 stmts

  (* ----------------------
   Code emission with preallocated spill area
   ----------------------
   We will: perform analysis to get max_needed, compute num_spill_slots = max(0, max_needed - num_temp_regs)
   Reserve spill slots inside the frame: spill_slot i is at [fp, #- (locals_bytes + 8*i)] (i from 1..num_spill_slots)

   During emission, values can be represented by:
     - Reg r
     - Spill i  (meaning stored at spill slot index i)
   When an operation requires a register, memory operands are loaded into a temporary register
   (allocating from free temp_regs or reusing scratch_reg transiently).
*)

  type value_loc = Reg of string | Spill of int (* 1-based index *)

  type emit_state = {
    mutable free_regs : string list;
    num_spill_slots : int;
    spill_base_offset : int; (* bytes from fp to first spill slot (positive) *)
    mutable next_spill_alloc : int;
        (* next spill slot to use when we need to spill a live value *)
  }

  let init_emit_state ~num_spill_slots ~spill_base_offset =
    {
      free_regs = temp_regs;
      num_spill_slots;
      spill_base_offset;
      next_spill_alloc = 1;
    }

  let alloc_reg st =
    match st.free_regs with
    | r :: rest ->
        st.free_regs <- rest;
        Some r
    | [] -> None

  let free_reg st r = st.free_regs <- r :: st.free_regs

  let alloc_spill_slot st =
    if st.next_spill_alloc > st.num_spill_slots then
      failwith "ran out of spill slots (logic error)";
    let i = st.next_spill_alloc in
    st.next_spill_alloc <- st.next_spill_alloc + 1;
    i

  (* helper to ensure a value is in a register; returns Reg r and ensures register is allocated and value loaded if needed. *)
  let ensure_in_reg st out v =
    match v with
    | Reg r -> Reg r
    | Spill i -> (
        match alloc_reg st with
        | Some r ->
            (* load spill slot into r *)
            let off = st.spill_base_offset + ((i - 1) * 8) in
            add_line out (Format.sprintf "    ldr %s, [fp, #-%d]" r off);
            Reg r
        | None ->
            (* No free regs: use scratch_reg to load, then store back into another spill slot
              but this situation should be avoided because num_spill_slots computed from analysis.
              For robustness, load into scratch and return Reg scratch. *)
            let off = st.spill_base_offset + ((i - 1) * 8) in
            add_line out
              (Format.sprintf "    ldr %s, [fp, #-%d]" scratch_reg off);
            Reg scratch_reg)

  (* Compile atom yields a value_loc. It will allocate a register if available, otherwise write into spill slot. *)
  let compile_atm layout st out atm =
    match atm with
    | AtmInt n -> (
        match alloc_reg st with
        | Some r ->
            add_line out (Format.sprintf "    mov %s, #%d" r n);
            Reg r
        | None ->
            let slot = alloc_spill_slot st in
            let off = st.spill_base_offset + ((slot - 1) * 8) in
            add_line out
              (Format.sprintf "    mov %s, #%d\n    str %s, [fp, #-%d]"
                 scratch_reg n scratch_reg off);
            Spill slot)
    | AtmBool b -> (
        let v = if b then 1 else 0 in
        match alloc_reg st with
        | Some r ->
            add_line out (Format.sprintf "    mov %s, #%d" r v);
            Reg r
        | None ->
            let slot = alloc_spill_slot st in
            let off = st.spill_base_offset + ((slot - 1) * 8) in
            add_line out
              (Format.sprintf "    mov %s, #%d\n    str %s, [fp, #-%d]"
                 scratch_reg v scratch_reg off);
            Spill slot)
    | AtmVar name -> (
        let off_local = lookup_offset layout name in
        match alloc_reg st with
        | Some r ->
            add_line out (Format.sprintf "    ldr %s, [fp, #-%d]" r off_local);
            Reg r
        | None ->
            let slot = alloc_spill_slot st in
            let off = st.spill_base_offset + ((slot - 1) * 8) in
            add_line out
              (Format.sprintf "    ldr %s, [fp, #-%d]\n    str %s, [fp, #-%d]"
                 scratch_reg off_local scratch_reg off);
            Spill slot)

  (* Compile binary op: ensure both operands are in registers, then emit instruction, free right reg and keep result in left reg. *)
  let compile_binop st out op v1 v2 =
    let rv1 =
      match ensure_in_reg st out v1 with
      | Reg r -> r
      | _ -> failwith "ensure_in_reg returned non-reg"
    in
    let rv2 =
      match ensure_in_reg st out v2 with
      | Reg r -> r
      | _ -> failwith "ensure_in_reg returned non-reg"
    in
    (match op with
    | OpAdd -> add_line out (Format.sprintf "    add %s, %s, %s" rv1 rv1 rv2)
    | OpSub -> add_line out (Format.sprintf "    sub %s, %s, %s" rv1 rv1 rv2)
    | OpMul -> add_line out (Format.sprintf "    mul %s, %s, %s" rv1 rv1 rv2)
    | OpDiv -> add_line out (Format.sprintf "    sdiv %s, %s, %s" rv1 rv1 rv2)
    | OpXor -> add_line out (Format.sprintf "    eor %s, %s, %s" rv1 rv1 rv2)
    | OpEq | OpNe | OpLess | OpGreater | OpLessEq | OpGreaterEq ->
        add_line out (Format.sprintf "    cmp %s, %s" rv1 rv2);
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
        let wreg = "w" ^ String.sub rv1 1 (String.length rv1 - 1) in
        add_line out (Format.sprintf "    cset %s, %s" wreg cond);
        add_line out (Format.sprintf "    uxtw %s, %s" rv1 wreg));
    (* free rv2 register back to pool *)
    free_reg st rv2;
    Reg rv1

  (* Compile expression returning a value_loc *)
  let rec compile_exp layout st out e =
    match e with
    | Atm a -> compile_atm layout st out a
    | Operator (op, a1, a2) ->
        let v1 = compile_exp layout st out (Atm a1) in
        let v2 = compile_exp layout st out (Atm a2) in
        compile_binop st out op v1 v2
    | Function (fname, args) -> (
        List.iteri
          (fun i a ->
            let v = compile_atm layout st out a in
            match ensure_in_reg st out v with
            | Reg r ->
                if i < List.length caller_arg_regs then
                  add_line out
                    (Format.sprintf "    mov %s, %s"
                       (List.nth caller_arg_regs i)
                       r)
                else
                  let caller_stack_offset = 8 * (i - 8) in
                  add_line out
                    (Format.sprintf "    str %s, [sp, #%d]" r
                       caller_stack_offset);
                  (* free r after moving/storing *) free_reg st r
            | _ -> failwith "unexpected non-reg after ensure_in_reg")
          args;
        add_line out (Format.sprintf "    bl _%s" fname);
        match alloc_reg st with
        | Some r ->
            add_line out (Format.sprintf "    mov %s, x0" r);
            Reg r
        | None ->
            let slot = alloc_spill_slot st in
            let off = st.spill_base_offset + ((slot - 1) * 8) in
            add_line out
              (Format.sprintf "    mov %s, x0\n    str %s, [fp, #-%d]"
                 scratch_reg scratch_reg off);
            Spill slot)

  let compile_stmts stmts layout num_spill_slots spill_base_offset out =
    let st = init_emit_state ~num_spill_slots ~spill_base_offset in
    let rec compile_stmt_list = function
      | [] -> ()
      | Label i :: rest ->
          add_line out (Format.sprintf "%s:" (label_name i));
          compile_stmt_list rest
      | Jmp i :: rest ->
          add_line out (Format.sprintf "    b %s" (label_name i));
          compile_stmt_list rest
      | CMov (atm, lbl) :: rest ->
          let v = compile_atm layout st out atm in
          (match ensure_in_reg st out v with
          | Reg r ->
              add_line out (Format.sprintf "    cbnz %s, %s" r (label_name lbl));
              free_reg st r
          | _ -> failwith "CMov: unexpected non-reg");
          compile_stmt_list rest
      | Assign (v, e) :: rest ->
          let val_loc =
            match e with
            | Atm a -> compile_atm layout st out a
            | _ -> compile_exp layout st out e
          in
          (match ensure_in_reg st out val_loc with
          | Reg r ->
              let off = lookup_offset layout v in
              add_line out (Format.sprintf "    str %s, [fp, #-%d]" r off);
              free_reg st r
          | _ -> failwith "Assign: unexpected non-reg after ensure_in_reg");
          compile_stmt_list rest
      | Return e :: _ -> (
          let val_loc =
            match e with
            | Atm a -> compile_atm layout st out a
            | _ -> compile_exp layout st out e
          in
          match ensure_in_reg st out val_loc with
          | Reg r ->
              add_line out (Format.sprintf "    mov x0, %s" r);
              free_reg st r
          | _ -> failwith "Return: unexpected non-reg after ensure_in_reg")
    in
    compile_stmt_list stmts

  (* Top-level compile: two-pass. First pass: compute max temps needed -> compute number of spill slots -> compute frame size. Second pass: emit code using offsets. *)
  let compile_function ~name ~params ~stmts =
    (* prepare locals layout (locals do not include spill slots) *)
    let locals = collect_locals params stmts in
    let layout = make_stack_layout locals in
    (* analysis pass *)
    let max_needed = max_temps_stmts stmts in
    let num_temp_regs = List.length temp_regs in
    let num_spill_slots = max 0 (max_needed - num_temp_regs) in
    (* compute bytes: locals + stack_params + spill area + saved fp/lr *)
    let num_locals = List.length layout in
    let stack_params = max 0 (List.length params - 8) in
    let locals_bytes = num_locals * 8 in
    let stack_params_bytes = stack_params * 8 in
    let spill_area_bytes = num_spill_slots * 8 in
    let spill_base_offset = locals_bytes + 8 in
    let frame_size =
      align16 (locals_bytes + spill_area_bytes + stack_params_bytes + 16)
    in

    (* emission pass *)
    let out = ref "" in
    add_line out ".text";
    add_line out (Format.sprintf ".globl _%s" name);
    add_line out (Format.sprintf "_%s:" name);
    add_line out "    stp fp, lr, [sp, #-16]!";
    add_line out "    mov fp, sp";
    add_line out (Format.sprintf "    sub sp, sp, #%d" frame_size);
    (* store incoming regs into locals *)
    List.iteri
      (fun i p ->
        if List.mem p locals then
          let off = lookup_offset layout p in
          if i < 8 then
            add_line out
              (Format.sprintf "    str %s, [fp, #-%d]"
                 (List.nth caller_arg_regs i)
                 off)
          else
            add_line out
              (Format.sprintf "    ldr x%d, [fp, #%d]\n    str x%d, [fp, #-%d]"
                 i
                 (((i - 8) * 8) + 16)
                 i off))
      params;

    compile_stmts stmts layout num_spill_slots spill_base_offset out;

    add_line out (Format.sprintf "    add sp, sp, #%d" frame_size);
    add_line out (Format.sprintf "    ldp fp, lr, [sp], #%d" 16);
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
      let expr, count = expression |> Uniquify.uniquify_expr count params_map in
      let params_map =
        params |> List.map (fun param -> (param, true)) |> StringMap.of_list
      in
      let expr, count = Monadic.remove_complex_operands params_map count expr in
      let stmts, _ = ExplicateControl.explicate_control expr None count in
      let cons =
        cons ^ "\n" ^ AsmGenerator.compile_function ~name ~params ~stmts
      in
      compile_functions rest declarations count cons

let compile_code code =
  let builtin_functions = [ "read"; "print_int"; "print_bool" ] in
  let functions, _ = code |> Lexer.lexer |> Parser.parse_stmts [] [] in
  let declarations, functions = Result.get_ok functions in
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
    \    add sp, sp, #16\n\
    \    ldp fp, lr, [sp], #16\n\
    \    ret\n\n\
     .text\n\
     .global _print_int\n\
     _print_int:\n\
    \    stp fp, lr, [sp, #-16]!\n\
    \    mov x0, x19\n\
    \    ADRP X0, fmt_int@PAGE\n\
    \    ADD X0, X0, fmt_int@PAGEOFF\n\
    \    STR x19, [SP, #-16]!\n\
    \    BL  _printf\n\
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
    \    .asciz \"%lld\\n\"\n\
     .balign 4\n\
     fmt_int:\n\
    \    .asciz \"%lld\\n\"\n\
     .balign 4\n\
     num:    .quad 0\n\
    \      "
  in
  compile_functions functions declarations 0 builtin
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
