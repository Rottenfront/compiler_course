(* open CompilerCourse.Parser *)
(* open CompilerCourse.Compiler *)
open CompilerCourse

let usage_msg =
  "compiler_course [-verbose] -source <source_language> -arch <arch> <source> \
   -o <output>"

let verbose = ref false
let input_file = ref ""
let output_file = ref ""
let arch = ref ""
let source_language = ref ""

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ( "-arch",
      Arg.Set_string arch,
      "Target architecture (arm64-darwin, arm64-linux)" );
    ("-source", Arg.Set_string source_language, "Source language (lama, racket)");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist (fun filename -> input_file := filename) usage_msg;
  let target =
    match !arch with
    | "arm64-darwin" -> Config.Arm64Darwin
    | "arm64-linux" -> Config.Arm64Linux
    | _ -> failwith "Unknown target architecture"
  in
  let source =
    match !source_language with
    | "lama" -> Config.Lama
    | "racket" -> Config.Racket
    | _ -> failwith "Unknown source language"
  in
  Compiler.compile_file_to_file target source !input_file !output_file
