type function_data = {
  name : string;
  parameters : string list;
  statements : ExplicateControl.stmt list;
}

let builtin_functions = [ "read"; "print_int"; "print_bool" ]
