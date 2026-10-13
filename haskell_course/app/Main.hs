module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Course.Syntax
import Course.Compiler
import Course.Parser
import Course.Check
import Course.Lower

data Options = Options
  { language :: Maybe Language, target :: Maybe Target
  , input :: Maybe FilePath, output :: Maybe FilePath, verbose :: Bool }

usage :: String
usage = "Usage: haskell-course -source <caml|lama|lisp|racket> -arch <aarch64-linux|arm64-linux|x86_64-linux> <input> -o <output.s> [-verbose]"

options :: Options -> [String] -> Either String Options
options current [] = Right current
options current ("-source":value:rest) = do
  lang <- case value of
    "caml" -> Right Caml; "lama" -> Right Caml
    "lisp" -> Right Lisp; "racket" -> Right Lisp
    _ -> Left ("unknown language: " ++ value)
  options (current { language = Just lang }) rest
options current ("-arch":value:rest) = do
  arch <- case value of
    "aarch64-linux" -> Right AArch64Linux; "arm64-linux" -> Right AArch64Linux
    "x86_64-linux" -> Right X86_64Linux
    _ -> Left ("unknown target: " ++ value)
  options (current { target = Just arch }) rest
options current ("-o":value:rest) = options (current { output = Just value }) rest
options current ("-verbose":rest) = options (current { verbose = True }) rest
options current (value:rest)
  | take 1 value == "-" = Left ("unknown or incomplete option: " ++ value)
  | otherwise = case input current of
      Nothing -> options (current { input = Just value }) rest
      Just _ -> Left "provide exactly one input file"

main :: IO ()
main = do
  args <- getArgs
  when (args == ["--help"] || args == ["-h"]) (putStrLn usage >> exitSuccess)
  case options (Options Nothing Nothing Nothing Nothing False) args of
    Left err -> die (err ++ "\n" ++ usage)
    Right opts -> case (language opts, target opts, input opts, output opts) of
      (Just lang, Just arch, Just sourcePath, Just assemblyPath) -> do
        result <- try (run opts lang arch sourcePath assemblyPath) :: IO (Either IOException ())
        either (die . show) pure result
      _ -> die usage

run :: Options -> Language -> Target -> FilePath -> FilePath -> IO ()
run opts lang arch sourcePath assemblyPath = do
  source <- readFile sourcePath
  case compileSource lang arch sourcePath source of
    Left err -> die err
    Right assembly -> do
      when (verbose opts) $ case parseProgram lang sourcePath source >>= checkProgram of
        Left err -> die err
        Right ast -> hPutStrLn stderr ("AST:\n" ++ show ast ++ "\nIR:\n" ++ show (lowerProgram ast))
      writeFile assemblyPath assembly

die :: String -> IO a
die message = hPutStrLn stderr message >> exitFailure
