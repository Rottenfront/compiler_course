# Repository Guidelines

## Project Structure & Module Organization

This OCaml compiler accepts Lama and Racket syntax and emits ARM64 assembly for Linux or Darwin.

- `bin/main.ml`: command-line entry point and compilation pipeline.
- `lib/`: lexer, checker, configuration, and shared utilities.
- `lib/parser/`: language parsers, concrete syntax tree (CST), and abstract syntax tree (AST).
- `lib/compiler/`: transformation passes and `AsmGenerator/` backends.
- `test/test_compiler_course.ml`: Alcotest runner; `test/samples/` contains Lama (`.cmp`) and Racket (`.rkt`) fixtures.
- `dune-project`: package metadata; `CompilerCourse.opam` is generated from it. `_build/` contains generated artifacts.

## Build, Test, and Development Commands

Use OCaml with Dune 3.19 or newer. In an opam environment, install development tools with `opam install dune alcotest ocamlformat`; Alcotest is required by `test/dune` but absent from the package dependency list.

- `dune build`: build the compiler and test executable.
- `dune build bin/main.exe`: build only the compiler.
- `dune runtest`: run the test alias.
- `dune fmt`: format sources using `.ocamlformat`; use `dune fmt --preview` to inspect changes first.

Compile a sample:

```sh
dune exec CompilerCourse -- -source lama -arch arm64-linux test/samples/1.cmp -o /tmp/example.s
```

Use `-source racket` for `.rkt` inputs, `-arch arm64-darwin` for Darwin, and `-verbose` for debugging.

## Coding Style & Naming Conventions

Follow existing OCaml formatting: two-space indentation, `snake_case` functions and values, and capitalized modules and variant constructors. Preserve existing compiler-pass names such as `ExplicateControl.ml`. Keep parser changes in `lib/parser/` and architecture-specific assembly logic in its backend module. Update `dune-project` rather than editing generated opam metadata.

## Testing Guidelines

The suite discovers samples by extension and validates parsing and checking. Add regression fixtures for accepted programs; add explicit Alcotest cases for expected failures or compiler behavior. No coverage threshold is configured.

Tests expect `samples/` beneath their working directory, but `test/dune` currently omits fixture dependencies. If the test alias cannot locate samples, run:

```sh
dune build test/test_compiler_course.exe
(cd test && ../_build/default/test/test_compiler_course.exe)
```

## Commit & Pull Request Guidelines

History uses short, descriptive subjects such as “Fixed compiler executable”; no mandatory prefix convention is evident. Keep commits focused. Pull requests should explain behavior changes, include a relevant source example, identify affected languages or targets, link applicable issues, and report validation commands and results.
