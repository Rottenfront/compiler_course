module Course.Parser (parseProgram) where

import Text.Parsec (parse)
import Course.Syntax
import qualified Course.Parser.Caml as Caml
import qualified Course.Parser.Lisp as Lisp

parseProgram :: Language -> FilePath -> String -> Either String Program
parseProgram language filename source =
  either (Left . show) Right (parse parser filename source)
  where
    parser = case language of Caml -> Caml.program; Lisp -> Lisp.program
