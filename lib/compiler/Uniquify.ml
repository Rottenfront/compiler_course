open Utils
open Parser.Ast

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
    | TmApplication { name; arguments } ->
        let arguments, count = uniquify_list count context [] arguments in
        (TmApplication { name = StringMap.find name context; arguments }, count)
    | TmLiteral lit -> (TmLiteral lit, count)
    | TmOpApp { lhs; operator; rhs } ->
        let lhs, count = uniquify_expr count context lhs in
        let rhs, count = uniquify_expr count context rhs in
        (TmOpApp { lhs; operator; rhs }, count)
    | TmIf { condition; if_true; if_false } ->
        let condition, count = uniquify_expr count context condition in
        let if_true, count = uniquify_expr count context if_true in
        let if_false, count = uniquify_expr count context if_false in
        (TmIf { condition; if_true; if_false }, count)
    | TmLet { name; value; expression } ->
        let value, count = uniquify_expr count context value in
        let new_name = Format.sprintf "%s.%d" name count in
        let context = StringMap.add name new_name context in
        let count = count + 1 in
        let expression, count = uniquify_expr count context expression in
        (TmLet { name; value; expression }, count)
