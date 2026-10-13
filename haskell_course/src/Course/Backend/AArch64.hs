module Course.Backend.AArch64 (generate) where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int64)
import Data.Word (Word64)
import Course.Syntax
import Course.IR
import Course.Backend.Linux

generate :: [FunctionIR] -> String
generate functions = unlines $
  formats ++ runtime ++ concatMap generateFunction functions ++
  [".text", ".global main", "main:", "  stp x29, x30, [sp, #-16]!", "  mov x29, sp"
  ,"  bl " ++ symbol "main", "  ldp x29, x30, [sp], #16", "  ret"
  ,".section .note.GNU-stack,\"\",%progbits"]

-- MOVZ/MOVK handle every signed 64-bit literal, not just MOV's immediate subset.
constant :: String -> Int64 -> [String]
constant reg n =
  let bits = fromIntegral n :: Word64
      chunk i = show ((bits `shiftR` (16 * i)) .&. 65535)
  in ["  movz " ++ reg ++ ", #" ++ chunk 0] ++
     ["  movk " ++ reg ++ ", #" ++ chunk i ++ ", lsl #" ++ show (16 * i) | i <- [1..3], chunk i /= "0"]

address :: Slot -> [String]
address n = constant "x12" (fromIntegral (8 * (n + 1))) ++ ["  sub x12, x29, x12"]

load :: String -> Slot -> [String]
load reg n = address n ++ ["  ldr " ++ reg ++ ", [x12]"]

store :: Slot -> String -> [String]
store n reg = address n ++ ["  str " ++ reg ++ ", [x12]"]

generateFunction :: FunctionIR -> [String]
generateFunction f =
  [".text", ".balign 4", ".type " ++ name ++ ", %function", name ++ ":"
  ,"  stp x29, x30, [sp, #-16]!", "  mov x29, sp"] ++
  constant "x9" (fromIntegral frame) ++ ["  sub sp, sp, x9"] ++
  concatMap parameterCode (zip [0 :: Int ..] (irParameters f)) ++
  [body ++ ":"] ++ concatMap instruction (irInstructions f) ++
  [exit ++ ":", "  mov sp, x29", "  ldp x29, x30, [sp], #16", "  ret"
  ,".size " ++ name ++ ", .-" ++ name]
  where
    name = symbol (irName f)
    body = ".L" ++ name ++ "_body"
    exit = ".L" ++ name ++ "_exit"
    frame = align16 (8 * irSlotCount f)
    local n = ".L" ++ name ++ "_" ++ show n
    parameterCode (i, dest)
      | i < 8 = store dest ("x" ++ show i)
      | otherwise = constant "x12" (fromIntegral (16 + 8 * (i - 8))) ++
                    ["  add x12, x29, x12", "  ldr x9, [x12]"] ++ store dest "x9"
    instruction ins = case ins of
      Constant dest n -> constant "x9" n ++ store dest "x9"
      Copy dest src -> load "x9" src ++ store dest "x9"
      Operation dest op a b -> load "x9" a ++ load "x10" b ++ operation op ++ store dest "x9"
      Call dest callee args ->
        let rest = drop 8 args
            outgoing = align16 (8 * length rest)
        in (if outgoing == 0 then [] else constant "x9" (fromIntegral outgoing) ++ ["  sub sp, sp, x9"]) ++
           concat [load "x9" arg ++ constant "x12" (fromIntegral (8 * i)) ++
                   ["  add x12, sp, x12", "  str x9, [x12]"] | (i, arg) <- zip [0 :: Int ..] rest] ++
           concat [load ("x" ++ show i) arg | (i, arg) <- zip [0 :: Int ..] (take 8 args)] ++
           ["  bl " ++ runtimeSymbol callee] ++
           (if outgoing == 0 then [] else constant "x9" (fromIntegral outgoing) ++ ["  add sp, sp, x9"]) ++
           store dest "x0"
      Branch test yes no -> load "x9" test ++ ["  cbnz x9, " ++ local yes, "  b " ++ local no]
      Jump dest -> ["  b " ++ local dest]
      Mark dest -> [local dest ++ ":"]
      Return src -> load "x0" src ++ ["  b " ++ exit]
      TailCall args -> concat (zipWith (\dest src -> load "x9" src ++ store dest "x9") (irParameters f) args)
                       ++ ["  b " ++ body]

operation :: Op -> [String]
operation Add = ["  add x9, x9, x10"]
operation Sub = ["  sub x9, x9, x10"]
operation Mul = ["  mul x9, x9, x10"]
operation Div = ["  sdiv x9, x9, x10"]
operation Xor = ["  eor x9, x9, x10"]
operation op = ["  cmp x9, x10", "  cset x9, " ++ condition op]

condition :: Op -> String
condition Eq = "eq"
condition Ne = "ne"
condition Lt = "lt"
condition Gt = "gt"
condition Le = "le"
condition Ge = "ge"
condition op = error ("unexpected lowered comparison: " ++ show op)

runtime :: [String]
runtime =
  [".text", ".balign 4", "hc_print_int:", "  stp x29, x30, [sp, #-16]!", "  mov x29, sp"
  ,"  mov x1, x0", "  adrp x0, .Lhc_fmt_int", "  add x0, x0, :lo12:.Lhc_fmt_int"
  ,"  bl printf", "  mov x0, #0", "  ldp x29, x30, [sp], #16", "  ret"
  ,"hc_print_bool:", "  stp x29, x30, [sp, #-16]!", "  mov x29, sp"
  ,"  cmp x0, #0", "  adrp x0, .Lhc_false", "  add x0, x0, :lo12:.Lhc_false"
  ,"  adrp x1, .Lhc_true", "  add x1, x1, :lo12:.Lhc_true", "  csel x0, x1, x0, ne"
  ,"  bl printf", "  mov x0, #0", "  ldp x29, x30, [sp], #16", "  ret"
  ,"hc_read:", "  stp x29, x30, [sp, #-16]!", "  mov x29, sp", "  sub sp, sp, #16"
  ,"  str xzr, [sp]", "  mov x1, sp", "  adrp x0, .Lhc_fmt_read"
  ,"  add x0, x0, :lo12:.Lhc_fmt_read", "  bl scanf", "  ldr x0, [sp]"
  ,"  add sp, sp, #16", "  ldp x29, x30, [sp], #16", "  ret"]
