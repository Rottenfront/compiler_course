module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath ((</>))
import System.Info (arch, os)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Course.Syntax
import Course.Parser
import Course.Check
import Course.Compiler
import Course.Lower
import Course.IR

data Example = Example String String String String String ExitCode

examples :: [Example]
examples =
  [ Example "arithmetic and left associativity"
    "func main : () = print_int (10 - 3 - 2); print_int (24 / 3 / 2); print_int (2 + 3 * 4)"
    "(define (main : ()) (begin (print_int (- (- 10 3) 2)) (print_int (/ (/ 24 3) 2)) (print_int (+ 2 (* 3 4)))))"
    "" "5\n4\n14\n" ExitSuccess
  , Example "signed division, zero and overflow"
    "func main : () = print_int ((-7) / 3); print_int (9 / 0); print_int ((-9223372036854775808) / (-1)); print_int (9223372036854775807 + 1)"
    "(define (main : ()) (begin (print_int (/ -7 3)) (print_int (/ 9 0)) (print_int (/ -9223372036854775808 -1)) (print_int (+ 9223372036854775807 1))))"
    "" "-2\n0\n-9223372036854775808\n-9223372036854775808\n" ExitSuccess
  , Example "signed comparisons and equality"
    "func main : () = print_bool ((-2) < 1); print_bool (4 >= 4); print_bool (3 == 3); print_bool (true != false); print_bool (3 > 4); print_bool (3 <= 4)"
    "(define (main : ()) (begin (print_bool (< -2 1)) (print_bool (>= 4 4)) (print_bool (== 3 3)) (print_bool (!= true false)) (print_bool (> 3 4)) (print_bool (<= 3 4))))"
    "" "true\ntrue\ntrue\ntrue\nfalse\ntrue\n" ExitSuccess
  , Example "Boolean short circuit and XOR"
    "func flag (n : int) : bool = print_int n; true\nfunc main : () = print_bool (true || flag 1); print_bool (false || flag 2); print_bool (false && flag 3); print_bool (true && flag 4); print_bool (true ^ false)"
    "(define (flag (n : int) : bool) (begin (print_int n) true))\n(define (main : ()) (begin (print_bool (or true (flag 1))) (print_bool (or false (flag 2))) (print_bool (and false (flag 3))) (print_bool (and true (flag 4))) (print_bool (xor true false))))"
    "" "true\n2\ntrue\nfalse\n4\ntrue\ntrue\n" ExitSuccess
  , Example "input, evaluation order and lexical shadowing"
    "func main : () = let x = read in let x = x + 1 in let y = read in print_int (x - y); print_int (read - read)"
    "(define (main : ()) (let ([x (read)] [x (+ x 1)] [y (read)]) (begin (print_int (- x y)) (print_int (- (read) (read))))))"
    "10 4 9 2\n" "7\n7\n" ExitSuccess
  , Example "non-tail recursion"
    "func fib (n : int) : int = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)\nfunc main : () = print_int (fib 10)"
    "(define (fib (n : int) : int) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))\n(define (main : ()) (print_int (fib 10)))"
    "" "89\n" ExitSuccess
  , Example "million-iteration self-tail recursion"
    "func loop (n : int) (acc : int) : int = if n == 0 then acc else loop (n - 1) (acc + n)\nfunc main : () = print_int (loop 1000000 0)"
    "(define (loop (n : int) (acc : int) : int) (if (== n 0) acc (loop (- n 1) (+ acc n))))\n(define (main : ()) (print_int (loop 1000000 0)))"
    "" "500000500000\n" ExitSuccess
  , Example "mutual recursion and separate branch labels"
    "func even (n : int) : bool = if n == 0 then true else odd (n - 1)\nfunc odd (n : int) : bool = if n == 0 then false else even (n - 1)\nfunc main : () = print_bool (even 30); print_bool (odd 31)"
    "(define (even (n : int) : bool) (if (== n 0) true (odd (- n 1))))\n(define (odd (n : int) : bool) (if (== n 0) false (even (- n 1))))\n(define (main : ()) (begin (print_bool (even 30)) (print_bool (odd 31))))"
    "" "true\ntrue\n" ExitSuccess
  , Example "stack arguments and tail parameter swaps"
    "func loop (n : int) (a : int) (b : int) (c : int) (d : int) (e : int) (f : int) (g : int) (h : int) (i : int) (j : int) : int = if n == 0 then a + j else loop (n - 1) j i h g f e d c b a\nfunc main : () = print_int (loop 100001 1 2 3 4 5 6 7 8 9 10)"
    "(define (loop (n : int) (a : int) (b : int) (c : int) (d : int) (e : int) (f : int) (g : int) (h : int) (i : int) (j : int) : int) (if (== n 0) (+ a j) (loop (- n 1) j i h g f e d c b a)))\n(define (main : ()) (print_int (loop 100001 1 2 3 4 5 6 7 8 9 10)))"
    "" "11\n" ExitSuccess
  , Example "calls in discarded sequence positions"
    "func loop (n : int) : int = if n == 0 then 0 else loop (n - 1); 7\nfunc main : () = print_int (loop 3)"
    "(define (loop (n : int) : int) (if (== n 0) 0 (begin (loop (- n 1)) 7)))\n(define (main : ()) (print_int (loop 3)))"
    "" "7\n" ExitSuccess
  , Example "unit and failed input"
    "func nothing : () = ()\nfunc main : () = nothing; print_int read"
    "(define (nothing : ()) ())\n(define (main : ()) (begin (nothing) (print_int (read))))"
    "invalid\n" "0\n" ExitSuccess
  , Example "integer exit status"
    "func main : int = 17" "(define (main : int) 17)" "" "" (ExitFailure 17)
  , Example "encoded identifiers"
    "func id' (x : int) : int = x\nfunc main : () = print_int (id' 42)"
    "(define (id' (x : int) : int) x)\n(define (main : ()) (print_int (id' 42)))"
    "" "42\n" ExitSuccess
  , Example "large stack frames"
    ("func main : () = let x0 = 0 in " ++ concat ["let x" ++ show n ++ " = x" ++ show (n - 1) ++ " + 1 in " | n <- [1 :: Int ..600]] ++ "print_int x600")
    ("(define (main : ()) (let ([x0 0] " ++ concat ["[x" ++ show n ++ " (+ x" ++ show (n - 1) ++ " 1)] " | n <- [1 :: Int ..600]] ++ ") (print_int x600)))")
    "" "600\n" ExitSuccess
  ]

main :: IO ()
main = do
  pureTests
  bracket temporary removePathForcibly $ \dir -> do
    native <- findExecutable "gcc"
    case (os, arch, native) of
      ("linux", "x86_64", Just cc) -> integration dir X86_64Linux cc Nothing
      ("linux", "aarch64", Just cc) -> integration dir AArch64Linux cc Nothing
      _ -> putStrLn "SKIP native execution: Linux GCC on x86_64 or AArch64 is required"
    explicitCC <- lookupEnv "AARCH64_CC"
    defaultCC <- findExecutable "aarch64-linux-gnu-gcc"
    explicitRunner <- lookupEnv "AARCH64_RUNNER"
    defaultRunner <- findExecutable "qemu-aarch64"
    let cc = choose explicitCC defaultCC
        runner = choose explicitRunner defaultRunner
    case (cc, runner) of
      (Just compiler, Just emulator) -> integration dir AArch64Linux compiler (Just emulator)
      _ -> putStrLn "SKIP cross execution: set AARCH64_CC and AARCH64_RUNNER (Linux cross compiler and QEMU)"
  putStrLn "All available compiler tests passed."

choose :: Maybe a -> Maybe a -> Maybe a
choose (Just a) _ = Just a
choose Nothing b = b

temporary :: IO FilePath
temporary = do
  root <- getTemporaryDirectory
  (path, handle) <- openTempFile root "haskell-course-tests"
  hClose handle
  removeFile path
  createDirectory path
  pure path

pureTests :: IO ()
pureTests = do
  forM_ examples $ \(Example name caml lisp _ _ _) ->
    forM_ [(Caml, caml), (Lisp, lisp)] $ \(language, source) -> do
      _ <- success (name ++ " " ++ show language) (parseProgram language "test" source >>= checkProgram)
      forM_ [AArch64Linux, X86_64Linux] $ \target -> do
        assembly <- success name (compileSource language target "test" source)
        assert name (not (null assembly))
  forM_ badPrograms $ \(source, message) -> case parseProgram Caml "negative" source >>= checkProgram of
    Left err -> assert ("diagnostic: " ++ message) (message `isInfixOf` err)
    Right _ -> fail ("accepted invalid program: " ++ source)
  forM_ [(Caml, "func main : int = 1 garbage"), (Lisp, "(define (main : int) 1) garbage")]
    $ \(language, source) -> case parseProgram language "trailing" source of
      Left _ -> pure ()
      Right _ -> fail "parser accepted trailing garbage"
  _ <- success "Caml keyword operators and comments"
    (compileSource Caml X86_64Linux "aliases" "(* nested (* comment *) *) let rec main : () = // line comment\n print_bool (true or false and true xor false)")
  _ <- success "LISP booleans and comments"
    (compileSource Lisp AArch64Linux "aliases" "; line comment\n #| nested #| comment |# |# (define (main : ()) (print_bool (and #t #f)))")
  program <- success "tail-call IR" (parseProgram Caml "tail" "func f (n : int) : int = if n == 0 then 0 else f (n - 1)\nfunc main : int = f 5" >>= checkProgram)
  assert "tail call lowered" (any isTail (concatMap irInstructions (lowerProgram program)))
  let manySlots = "func main : () = " ++ concat (replicate 600 "print_int 1; ") ++ "()"
  _ <- success "large stack frame" (compileSource Caml AArch64Linux "large" manySlots)
  putStrLn "PASS parser, type checker, lowering and both assembly generators"
  where
    isTail (TailCall _) = True
    isTail _ = False

badPrograms :: [(String, String)]
badPrograms =
  [("func main : int = true", "expected int")
  ,("func main : int = missing", "unknown name")
  ,("func main : int = read 1", "expects 0 arguments")
  ,("func main : int = let x = 1 in x 2", "not a function")
  ,("func main : int = if 1 then 2 else 3", "expected bool")
  ,("func main : int = if true then 1 else false", "expected int")
  ,("func main : int = 1\nfunc main : int = 2", "duplicate function")
  ,("func f (x : int) (x : int) : int = x\nfunc main : int = 0", "duplicate parameter")
  ,("func print_int : int = 0\nfunc main : int = 0", "duplicate function")
  ,("func main : int = 9223372036854775808", "64-bit range")
  ,("func f (g : [int] int) : int = 0\nfunc main : int = 0", "higher-order")
  ,("func main (x : int) : int = x", "main must")]

integration :: FilePath -> Target -> FilePath -> Maybe FilePath -> IO ()
integration dir target cc runner = forM_ (zip [0 :: Int ..] examples) $ \(index, Example name caml lisp stdin stdout exit) ->
  forM_ [(Caml, caml), (Lisp, lisp)] $ \(language, source) -> do
    useStatic <- lookupEnv "AARCH64_STATIC"
    assembly <- success name (compileSource language target "integration" source)
    let stem = dir </> (show target ++ show language ++ show index)
        assemblyPath = stem ++ ".s"
        executablePath = stem ++ ".exe"
        static = ["-static" | runner /= Nothing && useStatic == Just "1"]
    writeFile assemblyPath assembly
    (built, compilerOut, compilerErr) <- readProcessWithExitCode cc (static ++ [assemblyPath, "-o", executablePath]) ""
    unless (built == ExitSuccess) (fail (name ++ " assembly failed:\n" ++ compilerOut ++ compilerErr))
    (actualExit, actualOut, actualErr) <- case runner of
      Nothing -> readProcessWithExitCode executablePath [] stdin
      Just emulator -> readProcessWithExitCode emulator [executablePath] stdin
    assert (name ++ " exit: " ++ show actualExit ++ "\n" ++ actualErr) (actualExit == exit)
    assert (name ++ " output: " ++ show actualOut) (actualOut == stdout)
    putStrLn ("PASS " ++ show target ++ " " ++ show language ++ " " ++ name)

success :: String -> Either String a -> IO a
success context = either (fail . ((context ++ ": ") ++)) pure

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)
