open Parser
open Utils

type wrong_argument_count = { got : int; expected : int list }

type check_error =
  | UnknownFunction of substring * node_info
  | WrongArgumentCount of wrong_argument_count * node_info
  | UnknownVariable of string * node_info

type check_context = { existing_names : StringSet.t }

let print_wrong_argument_count error =
  match error.expected with
  | [] -> failwith "unreachable"
  | [ value ] -> Printf.sprintf "Expected %d, got %d" value error.got
  | value :: rest ->
      let rec collect_values values =
        match values with
        | [] -> ""
        | value :: rest -> Printf.sprintf " or %d" value ^ collect_values rest
      in
      Printf.sprintf "Expected %d%s, got %d" value (collect_values rest)
        error.got

let rec check_expression (context : check_context) (expression : racket_node) =
  let check_expressions context expressions =
    List.concat
      (List.map (fun expr -> check_expression context expr) expressions)
  in
  match expression with
  | RkVar (info, name) ->
      if StringSet.mem name context.existing_names then []
      else [ UnknownVariable (name, info) ]
  | RkValueNumber _ -> []
  | RkApplication (info, { name = { str = name; position }; arguments }) -> (
      match name with
      | "read" ->
          if List.is_empty arguments then []
          else
            [
              WrongArgumentCount
                ({ got = List.length arguments; expected = [ 0 ] }, info);
            ]
      | "-" -> (
          match List.length arguments with
          | 1 -> check_expressions context arguments
          | 2 -> check_expressions context arguments
          | got -> [ WrongArgumentCount ({ got; expected = [ 1; 2 ] }, info) ])
      | "+" -> (
          match List.length arguments with
          | 2 -> check_expressions context arguments
          | got -> [ WrongArgumentCount ({ got; expected = [ 1; 2 ] }, info) ])
      | name -> [ UnknownFunction ({ str = name; position }, info) ])
  | RkLet (_, vars, node) ->
      let rec add_variables context variables =
        match variables with
        | [] -> (context, [])
        | { name = { str = name; position = _ }; value } :: rest ->
            let errors = check_expression context value in
            let context' =
              { existing_names = StringSet.add name context.existing_names }
            in
            let context'', next_errors = add_variables context' rest in
            (context'', List.append errors next_errors)
      in
      let context', errors = add_variables context vars in
      List.append errors (check_expression context' node)

let racket_checker input =
  check_expression { existing_names = StringSet.empty } input
