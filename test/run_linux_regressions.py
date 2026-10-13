#!/usr/bin/env python3
"""Execute OCaml-generated ARM64 Linux binaries, optionally under QEMU."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]

# Both frontends must preserve the same observable behavior.
CASES = [
    (
        "boolean short circuit",
        """define flag (n : int) : bool = print_int n; true
define main : () =
  print_bool (true || flag 1); print_bool (false || flag 2);
  print_bool (false && flag 3); print_bool (true && flag 4)
""",
        """(define (flag (n : int) : bool) (begin (print_int n) true))
(define (main : ()) (begin
  (print_bool (or true (flag 1))) (print_bool (or false (flag 2)))
  (print_bool (and false (flag 3))) (print_bool (and true (flag 4)))))
""",
        "", "true\n2\ntrue\nfalse\n4\ntrue\n", 0,
    ),
    (
        "I/O, function result binding, and signed division",
        """define identity (n : int) : int = n
define main : () = let x = identity read in
  print_int x; print_int ((0 - 7) / 3); print_int (9 / 0)
""",
        """(define (identity (n : int) : int) n)
(define (main : ()) (let ([x (identity (read))])
  (begin (print_int x) (print_int (/ (- 0 7) 3)) (print_int (/ 9 0)))))
""",
        "1234567890123\n", "1234567890123\n-2\n0\n", 0,
    ),
    (
        "integer equality, associativity, and branch joins",
        """define main : () = let x = 42 in
  let y = if 1 == 1 then x + 1 else x + 2 in
  print_int (x + y); print_int (10 - 3 - 2); print_int (24 / 3 / 2)
""",
        """(define (main : ()) (let ([x 42] [y (if (== 1 1) (+ x 1) (+ x 2))])
  (begin (print_int (+ x y)) (print_int (- (- 10 3) 2)) (print_int (/ (/ 24 3) 2)))))
""",
        "", "85\n5\n4\n", 0,
    ),
    (
        "tail recursion with stack arguments and swaps",
        """define loop (n : int) (a : int) (b : int) (c : int) (d : int)
  (e : int) (f : int) (g : int) (h : int) (i : int) (j : int) : int =
  if n == 0 then a + j else loop (n - 1) j i h g f e d c b a
define main : () = print_int (loop 100001 1 2 3 4 5 6 7 8 9 10)
""",
        """(define (loop (n : int) (a : int) (b : int) (c : int) (d : int)
  (e : int) (f : int) (g : int) (h : int) (i : int) (j : int) : int)
  (if (== n 0) (+ a j) (loop (- n 1) j i h g f e d c b a)))
(define (main : ()) (print_int (loop 100001 1 2 3 4 5 6 7 8 9 10)))
""",
        "", "11\n", 0,
    ),
    (
        "discarded recursive results",
        """define loop (n : int) : int =
  if n == 0 then 0 else loop (n - 1); 7
define main : () = print_int (loop 3)
""",
        """(define (loop (n : int) : int)
  (if (== n 0) 0 (begin (loop (- n 1)) 7)))
(define (main : ()) (print_int (loop 3)))
""",
        "", "7\n", 0,
    ),
    (
        "unit values and invalid input",
        "define main : () = (); print_int read; ()",
        "(define (main : ()) (begin () (print_int (read)) ()))",
        "invalid\n", "0\n", 0,
    ),
    (
        "integer exit status",
        "define main : int = 17", "(define (main : int) 17)",
        "", "", 17,
    ),
]

CASES += [
    ("64-bit overflow and signed division boundary",
     "define main : () = let min = (4611686018427387903 + 1) * 2 in print_int min; print_int (min / (0 - 1))",
     "(define (main : ()) (let ([min (* (+ 4611686018427387903 1) 2)]) (begin (print_int min) (print_int (/ min (- 0 1))))))",
     "", "-9223372036854775808\n-9223372036854775808\n", 0),
    ("large stack frames",
     "define main : () = let x0 = 0 in "
     + "".join(f"let x{n} = x{n - 1} + 1 in " for n in range(1, 601))
     + "print_int x600",
     "(define (main : ()) (let ([x0 0] "
     + "".join(f"[x{n} (+ x{n - 1} 1)] " for n in range(1, 601))
     + ") (print_int x600)))", "", "600\n", 0),
    ("existing sample 1", (ROOT / "test/samples/1.cmp").read_text(),
     (ROOT / "test/samples/1.rkt").read_text(), "10 4\n", "14\n", 0),
    ("existing sample 2", (ROOT / "test/samples/2.cmp").read_text(),
     (ROOT / "test/samples/2.rkt").read_text(), "10\n", "10\n89\n89\n", 0),
]


def run(command, stdin=""):
    result = subprocess.run(command, input=stdin, capture_output=True,
                            text=True, timeout=30)
    return result


def checked(command):
    result = run(command)
    if result.returncode:
        raise RuntimeError(f"{' '.join(map(str, command))}\n{result.stdout}{result.stderr}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", default=str(ROOT / "_build/default/bin/main.exe"))
    parser.add_argument("--cc", default=os.environ.get("AARCH64_CC", "aarch64-linux-gnu-gcc"))
    parser.add_argument("--runner", default=os.environ.get("AARCH64_RUNNER", "qemu-aarch64"),
                        help="emulator executable; use an empty string on native ARM64 Linux")
    parser.add_argument("--static", action="store_true")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="compiler-course-arm64-") as directory:
        temp = Path(directory)
        for index, (name, lama, racket, stdin, stdout, exit_code) in enumerate(CASES):
            for language, suffix, source in [("lama", "cmp", lama), ("racket", "rkt", racket)]:
                source_path = temp / f"{index}.{suffix}"
                assembly_path = temp / f"{index}.s"
                executable_path = temp / f"{index}.exe"
                source_path.write_text(source)
                checked([args.compiler, "-source", language, "-arch", "arm64-linux",
                         str(source_path), "-o", str(assembly_path)])
                checked([args.cc, *(["-static"] if args.static else []),
                         str(assembly_path), "-o", str(executable_path)])
                result = run(([args.runner] if args.runner else []) + [str(executable_path)], stdin)
                if (result.returncode, result.stdout) != (exit_code, stdout):
                    raise AssertionError(f"{language} {name}: exit {result.returncode}, "
                                         f"output {result.stdout!r}\n{result.stderr}")
                print(f"PASS ARM64 Linux {language}: {name}")


if __name__ == "__main__":
    main()
