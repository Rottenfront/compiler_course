open Ast
open Utils

type checker_error =
  | TypeMismatch
  | UnknownFunction of string
  | UnknownVar of string
  | UnknownType of string
  | RecursiveType of string
  | WrongArgumentCount of string * int * int

type bty = BInt | BBool | BUnit | BTuple of bty list

type typed_expr =
  | TInt of int
  | TBool of bool
  | TVar of bty * string
  | TIf of bty * typed_expr * typed_expr * typed_expr
  | TCall of bty * string * typed_expr list
  | TLet of bty * string * typed_expr * typed_expr
  | TCreateTuple of bty * typed_expr list
  | TAccessTuple of bty * int * typed_expr

let typeof expr =
  match expr with
  | TInt _ -> BInt
  | TBool _ -> BBool
  | TVar (ty, _) -> ty
  | TIf (ty, _, _, _) -> ty
  | TCall (ty, _, _) -> ty
  | TLet (ty, _, _, _) -> ty
  | TCreateTuple (ty, _) -> ty
  | TAccessTuple (ty, _, _) -> ty

type checker_result = (typed_expr, checker_error) result
type function_desc = bty list * bty

type checker_context = {
  mutable typenames : bty StringMap.t;
  mutable functions : function_desc StringMap.t;
}

let rec plain_type context ty =
  match ty with
  | TyNamed "int" -> BInt
  | TyNamed "bool" -> BBool
  | TyUnit -> BUnit
  | TyNamed name ->
      if StringMap.mem name context.typenames then
        StringMap.find name context.typenames
      else failwith ("Unknown type " ^ name)
  | TyTuple types -> BTuple (List.map (plain_type context) types)

let bound_type context name ty =
  let newtype = plain_type context ty in
  context.typenames <- StringMap.add name newtype context.typenames;
  ()

let bound_function context name params result_type expr =
  let params =
    List.map (fun (name, ty) -> (name, plain_type context ty)) params
  in
  let desc = (List.map snd params, plain_type context result_type) in
  context.functions <- StringMap.add name desc context.functions;
  (name, params, expr)

let rec bound_definitions context definitions functions =
  match definitions with
  | [] -> List.rev functions
  | Define ((name, params, result), expr) :: rest ->
      let func = bound_function context name params result expr in
      bound_definitions context rest (func :: functions)
  | TypeDef (name, ty) :: rest ->
      bound_type context name ty;
      bound_definitions context rest functions

let rec type_expression context local_vars expr =
  let rec check_arguments arguments result =
    match arguments with
    | [] -> Ok (List.rev result)
    | (arg, ty) :: rest -> (
        let arg = type_expression context local_vars arg in
        match arg with
        | Error err -> Error err
        | Ok arg ->
            if typeof arg = ty then check_arguments rest (arg :: result)
            else Error TypeMismatch)
  in
  let rec check_tuple_creation arguments result =
    match arguments with
    | [] -> Ok (List.rev result)
    | arg :: rest -> (
        let arg = type_expression context local_vars arg in
        match arg with
        | Error err -> Error err
        | Ok arg -> check_tuple_creation rest (arg :: result))
  in
  match expr with
  | Int value -> Ok (TInt value)
  | Bool value -> Ok (TBool value)
  | Var name ->
      if StringMap.mem name local_vars then
        Ok (TVar (StringMap.find name local_vars, name))
      else Error (UnknownVar name)
  | Call (name, args) ->
      if StringMap.mem name context.functions then
        let parameters, result = StringMap.find name context.functions in
        if List.length parameters != List.length args then
          Error
            (WrongArgumentCount (name, List.length parameters, List.length args))
        else
          let args = check_arguments (List.combine args parameters) [] in
          match args with
          | Error err -> Error err
          | Ok args -> Ok (TCall (result, name, args))
      else Error (UnknownFunction name)
  | If (cond, tru, fls) -> (
      let cond = type_expression context local_vars cond in
      let tru = type_expression context local_vars tru in
      let fls = type_expression context local_vars fls in
      match cond with
      | Error err -> Error err
      | Ok cond -> (
          if typeof cond != BBool then Error TypeMismatch
          else
            match tru with
            | Error err -> Error err
            | Ok tru -> (
                match fls with
                | Error err -> Error err
                | Ok fls ->
                    if typeof tru = typeof fls then
                      Ok (TIf (typeof tru, cond, tru, fls))
                    else Error TypeMismatch)))
  | Let ([], expr) -> type_expression context local_vars expr
  | Let ((name, value) :: rest, expr) -> (
      let value = type_expression context local_vars value in
      match value with
      | Error err -> Error err
      | Ok value -> (
          let local_vars = StringMap.add name (typeof value) local_vars in
          let expr = type_expression context local_vars (Let (rest, expr)) in
          match expr with
          | Error err -> Error err
          | Ok expr -> Ok (TLet (typeof expr, name, value, expr))))
  | Tuple args -> (
      match check_tuple_creation args [] with
      | Error err -> Error err
      | Ok args -> Ok (TCreateTuple (BTuple (List.map typeof args), args)))
  | AccessTuple (index, expr) -> (
      match type_expression context local_vars expr with
      | Error err -> Error err
      | Ok expr -> (
          match typeof expr with
          | BTuple subtypes ->
              if index < List.length subtypes then
                Ok (TAccessTuple (List.nth subtypes index, index, expr))
              else Error TypeMismatch
          | _ -> Error TypeMismatch))
