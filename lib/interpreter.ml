open Parser
open Utils

type interp_context = { variables : int StringMap.t }

let rec interpret context expr =
  match expr with
  | TkVar (_, name) -> StringMap.find name context.variables
  | TkApplication (_, { name = { str = name; position = _ }; arguments }) -> (
      match name with
      | "read" -> (
          match arguments with
          | [] -> read_int ()
          | _ -> failwith "unreachable if code is checked")
      | "+" -> (
          match arguments with
          | [ lhs; rhs ] -> interpret context lhs + interpret context rhs
          | _ -> failwith "unreachable if code is checked")
      | "-" -> (
          match arguments with
          | [ value ] -> ~-(interpret context value)
          | [ lhs; rhs ] -> interpret context lhs + interpret context rhs
          | _ -> failwith "unreachable if code is checked")
      | _ -> failwith "unreachable if code is checked")
  | TkValueNumber (_, value) -> value
  | TkLet (_, vars, expr) ->
      let rec add_variables context variables =
        match variables with
        | [] -> context
        | { name = { str = name; position = _ }; value } :: rest ->
            let value' = interpret context value in
            let context' = StringMap.add name value' context.variables in
            add_variables { variables = context' } rest
      in
      interpret (add_variables context vars) expr

let racket_interpreter input =
  let errors = Checker.racket_checker input in
  if List.is_empty errors then
    Ok (interpret { variables = StringMap.empty } input)
  else Error errors
