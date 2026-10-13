# Haskell compiler course

A standalone Cabal project implementing the repository's small, typed language
with interchangeable Parsec frontends and Linux assembly backends. The existing
OCaml project remains at the repository root.

## Build and run

Requires GHC, Cabal, Parsec, `containers`, and `mtl`. These packages are available
in the current environment. `cabal.project` uses installed packages and disables
remote repositories; enable repositories there if you need to download dependencies.

```sh
cd haskell_course
cabal build --offline
cabal run --offline haskell-course -- \
  -source caml -arch x86_64-linux examples/2.cmp -o /tmp/fib.s
gcc /tmp/fib.s -o /tmp/fib
printf '10\n' | /tmp/fib
```

The example prints `10`, `89`, and `89`, each on its own line. The compiler writes
assembly; GCC assembles and links it with libc. Generated assembly supports Linux
PIE executables.

For ARM64 Linux, use `-arch aarch64-linux` and link with an ARM64 GCC toolchain:

```sh
cabal run --offline haskell-course -- \
  -source lisp -arch aarch64-linux examples/2.rkt -o /tmp/fib-arm.s
aarch64-linux-gnu-gcc /tmp/fib-arm.s -o /tmp/fib-arm
QEMU_LD_PREFIX=/usr/aarch64-linux-gnu qemu-aarch64 /tmp/fib-arm
```

Aliases `lama`, `racket`, and `arm64-linux` match the original compiler's flags.
`-verbose` prints the AST and IR to stderr. `--help` lists the command syntax.
Diagnostics include source positions and failures exit unsuccessfully.

## Source languages

Both frontends produce the same AST. Functions require parameter and result types:
`int`, `bool`, or `()`. Higher-order signatures such as `[int] int` are parsed but
rejected by the checker. Functions can refer to declarations later in the file.

Caml-like syntax accepts `define`, `func`, and top-level `let`/`let rec`:

```ocaml
func sum (n : int) (acc : int) : int =
  if n == 0 then acc else sum (n - 1) (acc + n)

func main : () =
  let n = read in
  print_int (sum n 0)
```

Use parentheses around complex arguments and negative arguments: `f (x + 1)`
or `f (-3)`. Infix precedence, from highest to lowest, is multiplication/division,
addition/subtraction, ordering, equality, AND, OR, XOR, and sequencing. Operators
associate left. Parentheses override precedence. Comments use `//` or nested `(* *)`.

LISP-like syntax uses prefix calls and operators:

```scheme
(define (sum (n : int) (acc : int) : int)
  (if (== n 0) acc (sum (- n 1) (+ acc n))))

(define (main : ())
  (let ([n (read)]) (print_int (sum n 0))))
```

Parentheses, brackets, and braces delimit forms. Multiple `let` bindings are
sequential, so a later initializer can reference an earlier binding. `(begin a b)`
sequences expressions. Prefix operators with several operands fold left; at least
one operand is required. Booleans accept `true`/`false` and `#t`/`#f`.
Comments use `;` or nested `#| |#`.

## Shared semantics

- Signed 64-bit integers; addition, subtraction, and multiplication wrap.
- Signed division truncates toward zero. Like AArch64 `sdiv`, division by zero
  yields zero and the minimum integer divided by `-1` yields the minimum integer.
- Arithmetic: `+ - * /`; signed comparisons: `< > <= >=`; equality: `== !=`.
  Equality requires matching operand types and accepts integers, booleans, or unit.
- Boolean AND/OR short-circuit; XOR evaluates both operands. Spellings are
  `&&`/`and`, `||`/`or`, and `^`/`xor`.
- Expressions and call arguments evaluate left to right. Bindings have lexical
  scope. Both branches of `if` must have the same type.
- `read` reads a signed decimal integer, returning zero if conversion fails.
  `print_int` and `print_bool` print a newline and return unit, represented as zero.
- `main` takes no parameters and returns `int` or unit. Its integer result becomes
  the process status under Linux; unit returns zero.
- Self-recursive calls in tail position reuse the current frame. Other calls,
  including mutual recursion, use ordinary stack frames. Calls support arguments
  beyond the platform's register limit.

## Architecture and extension points

`src/Course/Parser/Caml.hs` and `Lisp.hs` implement Parsec grammars.
`Syntax.hs` defines the common AST; `Check.hs` checks names, arity, and types.
`Lower.hs` lowers expressions to explicit slots, branches, calls, and tail calls
in `IR.hs`. Each backend owns instruction selection, its Linux ABI, and libc I/O
wrappers. `Backend/Linux.hs` shares symbol encoding and data formats.

To add a syntax, implement a parser returning `Program`, add a `Language` variant,
and register it in `Parser.hs` and the CLI. To add a platform, implement a generator
over `[FunctionIR]`, add a `Target` variant, and register it in `Compiler.hs` and
the CLI. Add matching execution tests before extending either dispatch.

## Tests

```sh
cabal test --offline --test-show-details=direct
```

Tests validate both parsers, invalid programs, lowering, and both generators.
On Linux with GCC, they assemble and execute the native backend for both syntaxes,
checking output and exit status, recursion, stack arguments, and arithmetic edges.

To execute ARM64 cases on an x86_64 host:

```sh
AARCH64_CC=aarch64-linux-gnu-gcc AARCH64_RUNNER=qemu-aarch64 \
  QEMU_LD_PREFIX=/usr/aarch64-linux-gnu \
  cabal test --offline --test-show-details=direct
```

Compiler and runner variables must name executables, without additional arguments.
For a toolchain with static libc, set `AARCH64_STATIC=1` instead of providing a
dynamic sysroot. Tests report unavailable execution platforms as skipped.
