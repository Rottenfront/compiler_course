open Utils

type literal = Cst.literal
type operator = Cst.operator_type

type expr =
  | TmLiteral of literal
  | TmApplication of { name : string; arguments : expr list }
  | TmOpApp of { operator : operator; lhs : expr; rhs : expr }
  | TmLet of { name : string; value : expr; expression : expr }
  | TmIf of { condition : expr; if_true : expr; if_false : expr }
  | TmSequence of expr list

type implementation = {
  name : string;
  parameters : string list;
  expression : expr;
}

let rec cst_expr_to_ast (expr : Cst.expr_node) : expr =
  match expr.value with
  | Cst.TmParenth inner -> cst_expr_to_ast inner
  | Cst.TmApplication { name; arguments } ->
      let name = name.str in
      let arguments = List.map cst_expr_to_ast arguments in
      TmApplication { name; arguments }
  | Cst.TmIf { condition; if_true; if_false } ->
      let condition = cst_expr_to_ast condition in
      let if_true = cst_expr_to_ast if_true in
      let if_false = cst_expr_to_ast if_false in
      TmIf { condition; if_true; if_false }
  | Cst.TmLiteral literal -> TmLiteral literal
  | Cst.TmLet { name; value; expression } ->
      let name = name.str in
      let value = cst_expr_to_ast value in
      let expression = cst_expr_to_ast expression in
      TmLet { name; value; expression }
  | Cst.TmOpApp { operator; lhs; rhs } ->
      let operator = operator.type_ in
      let lhs = cst_expr_to_ast lhs in
      let rhs = cst_expr_to_ast rhs in
      TmOpApp { operator; lhs; rhs }
  | Cst.TmSequence expressions ->
      TmSequence (List.map cst_expr_to_ast expressions)

let rec cst_functions_to_ast (functions : Cst.implementation list)
    (result : implementation list) =
  match functions with
  | [] -> result
  | func :: rest ->
      let name = func.name.str in
      let parameters = List.map (fun (name, _) -> name.str) func.parameters in
      let expression = cst_expr_to_ast func.expression in
      cst_functions_to_ast rest ({ name; parameters; expression } :: result)

let repeat_space times = String.init times (fun _ -> ' ')

let rec print_ast_expr (indent : int) (expr : expr) : string =
  let indent = indent + 2 in
  let print_with_indent expr =
    "\n" ^ repeat_space indent ^ print_ast_expr indent expr
  in
  match expr with
  | TmLiteral lit -> Cst.print_literal lit
  | TmApplication { name; arguments } ->
      if List.is_empty arguments then name
      else
        "(" ^ name ^ "\n"
        ^ (List.map print_with_indent arguments |> String.concat "")
        ^ ")"
  | TmIf { condition; if_true; if_false } ->
      "(if "
      ^ (List.map print_with_indent [ condition; if_true; if_false ]
        |> String.concat "")
      ^ ")"
  | TmLet { name; value; expression } ->
      "(let ([" ^ name ^ " "
      ^ print_ast_expr indent value
      ^ "])"
      ^ print_with_indent expression
      ^ ")"
  | TmOpApp { operator; lhs; rhs } ->
      "("
      ^ Cst.print_operator operator
      ^ print_with_indent lhs ^ print_with_indent rhs ^ ")"
  | TmSequence expressions ->
      "(begin"
      ^ (List.map print_with_indent expressions |> String.concat "")
      ^ ")"

let print_functions (func : implementation) : string =
  "(func " ^ func.name ^ " ("
  ^ String.concat " " func.parameters
  ^ ") "
  ^ print_ast_expr 0 func.expression
  ^ ")"
