module Course.Backend.Linux (align16, symbol, runtimeSymbol, formats) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import Course.Syntax (Name)

align16 :: Int -> Int
align16 n = ((n + 15) `div` 16) * 16

-- Encode names so apostrophes and Unicode identifiers remain valid assembly.
symbol :: Name -> String
symbol name = "hc_f_" ++ intercalate "_" (map (\c -> showHex (ord c) "") name)

runtimeSymbol :: Name -> String
runtimeSymbol name
  | name `elem` ["read", "print_int", "print_bool"] = "hc_" ++ name
  | otherwise = symbol name

formats :: [String]
formats = [".section .rodata", ".Lhc_fmt_int:", "  .asciz \"%lld\\n\""
          ,".Lhc_fmt_read:", "  .asciz \"%lld\""
          ,".Lhc_true:", "  .asciz \"true\\n\""
          ,".Lhc_false:", "  .asciz \"false\\n\""]
