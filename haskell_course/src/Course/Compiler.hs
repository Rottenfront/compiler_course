module Course.Compiler (compileSource) where

import Course.Syntax
import Course.Parser
import Course.Check
import Course.Lower
import qualified Course.Backend.AArch64 as AArch64
import qualified Course.Backend.X86_64 as X86_64

compileSource :: Language -> Target -> FilePath -> String -> Either String String
compileSource language target filename source = do
  program <- parseProgram language filename source >>= checkProgram
  let ir = lowerProgram program
  pure $ case target of
    AArch64Linux -> AArch64.generate ir
    X86_64Linux -> X86_64.generate ir
