open Ast
open Utils

type checker_error =
  | TypeMismatch
  | UnknownFunction of string
  | UnknownType of string
  | RecursiveType of string
  | WrongArgumentCount of string * int * int

type typed_expr =
  | TInt of int
  | TBool of bool
  | TVar of ty * string
  | TIf of ty * typed_expr * typed_expr * typed_expr
  | TCall of ty * string * typed_expr list
  | TLet of ty * string * typed_expr * typed_expr
  | TCreateTuple of ty * typed_expr list
  | TAccessTuple of ty * int * typed_expr

let typeof expr =
  match expr with
  | TInt _ -> TyNamed "int"
  | TBool _ -> TyNamed "bool"
  | TVar (ty, _) -> ty
  | TIf (ty, _, _, _) -> ty
  | TCall (ty, _, _) -> ty
  | TLet (ty, _, _, _) -> ty
  | TCreateTuple (ty, _) -> ty
  | TAccessTuple (ty, _, _) -> ty

type checker_result = (typed_expr, checker_error) result
type function_desc = ty list * ty

type checker_context = {
  mutable typenames : ty StringMap.t;
  mutable functions : function_desc StringMap.t;
}

let rec plain_type context ty =
  match ty with
  | TyNamed "int" -> ty
  | TyNamed "bool" -> ty
  | TyUnit -> ty
  | TyNamed name ->
      if StringMap.mem name context.typenames then
        StringMap.find name context.typenames
      else failwith ("Unknown type " ^ name)
  | TyTuple types -> TyTuple (List.map (plain_type context) types)

let bound_type context name ty =
  let newtype = plain_type context ty in
  context.typenames <- StringMap.add name newtype context.typenames;
  ()

let bound_function context name params result_type expr =
  let params =
    List.map (fun (name, ty) -> (name, plain_type context ty)) params
  in
  let desc = (List.map snd params, result_type) in
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
