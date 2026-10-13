module Course.Backend.X86_64 (generate) where

import Data.Int (Int64)
import Course.Syntax
import Course.IR
import Course.Backend.Linux

generate :: [FunctionIR] -> String
generate functions = unlines $
  [".intel_syntax noprefix"] ++ formats ++ runtime ++ concatMap generateFunction functions ++
  [".text", ".global main", "main:", "  push rbp", "  mov rbp, rsp"
  ,"  call " ++ symbol "main", "  pop rbp", "  ret"
  ,".section .note.GNU-stack,\"\",@progbits"]

memory :: Slot -> String
memory n = "QWORD PTR [rbp - " ++ show (8 * (n + 1)) ++ "]"

load :: String -> Slot -> [String]
load reg n = ["  mov " ++ reg ++ ", " ++ memory n]

store :: Slot -> String -> [String]
store n reg = ["  mov " ++ memory n ++ ", " ++ reg]

argumentRegisters :: [String]
argumentRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

generateFunction :: FunctionIR -> [String]
generateFunction f =
  [".text", ".type " ++ name ++ ", @function", name ++ ":"
  ,"  push rbp", "  mov rbp, rsp", "  sub rsp, " ++ show frame] ++
  concatMap parameterCode (zip [0..] (irParameters f)) ++
  [body ++ ":"] ++ concatMap instruction (irInstructions f) ++
  [exit ++ ":", "  leave", "  ret", ".size " ++ name ++ ", .-" ++ name]
  where
    name = symbol (irName f)
    body = ".L" ++ name ++ "_body"
    exit = ".L" ++ name ++ "_exit"
    frame = align16 (8 * irSlotCount f)
    local n = ".L" ++ name ++ "_" ++ show n
    parameterCode (i, dest)
      | i < 6 = store dest (argumentRegisters !! i)
      | otherwise = ["  mov rax, QWORD PTR [rbp + " ++ show (16 + 8 * (i - 6)) ++ "]"] ++ store dest "rax"
    instruction ins = case ins of
      Constant dest n -> ["  mov rax, " ++ show n] ++ store dest "rax"
      Copy dest src -> load "rax" src ++ store dest "rax"
      Operation dest op a b -> load "rax" a ++ load "r10" b ++ operation dest op ++ store dest "rax"
      Call dest callee args ->
        let rest = drop 6 args
            outgoing = align16 (8 * length rest)
        in ["  sub rsp, " ++ show outgoing | outgoing /= 0] ++
           concat [load "rax" arg ++ ["  mov QWORD PTR [rsp + " ++ show (8 * i) ++ "], rax"]
                  | (i, arg) <- zip [0 :: Int ..] rest] ++
           concat (zipWith load argumentRegisters (take 6 args)) ++
           ["  call " ++ runtimeSymbol callee] ++
           ["  add rsp, " ++ show outgoing | outgoing /= 0] ++ store dest "rax"
      Branch test yes no -> load "rax" test ++ ["  test rax, rax", "  jne " ++ local yes, "  jmp " ++ local no]
      Jump dest -> ["  jmp " ++ local dest]
      Mark dest -> [local dest ++ ":"]
      Return src -> load "rax" src ++ ["  jmp " ++ exit]
      TailCall args -> concat (zipWith (\dest src -> load "rax" src ++ store dest "rax") (irParameters f) args)
                       ++ ["  jmp " ++ body]
    operation dest op = case op of
      Add -> ["  add rax, r10"]
      Sub -> ["  sub rax, r10"]
      Mul -> ["  imul rax, r10"]
      Xor -> ["  xor rax, r10"]
      Div -> division dest
      _ -> ["  cmp rax, r10", "  set" ++ condition op ++ " al", "  movzx rax, al"]
    division dest =
      let zero = ".L" ++ name ++ "_div_zero_" ++ show dest
          divide = ".L" ++ name ++ "_divide_" ++ show dest
          done = ".L" ++ name ++ "_div_done_" ++ show dest
      in ["  test r10, r10", "  je " ++ zero
         ,"  mov r11, " ++ show (minBound :: Int64), "  cmp rax, r11", "  jne " ++ divide
         ,"  cmp r10, -1", "  je " ++ done
         ,divide ++ ":", "  cqo", "  idiv r10", "  jmp " ++ done
         ,zero ++ ":", "  xor eax, eax", done ++ ":"]

condition :: Op -> String
condition Eq = "e"
condition Ne = "ne"
condition Lt = "l"
condition Gt = "g"
condition Le = "le"
condition Ge = "ge"
condition op = error ("unexpected lowered comparison: " ++ show op)

runtime :: [String]
runtime =
  [".text", "hc_print_int:", "  push rbp", "  mov rbp, rsp"
  ,"  mov rsi, rdi", "  lea rdi, [rip + .Lhc_fmt_int]", "  xor eax, eax"
  ,"  call printf@PLT", "  xor eax, eax", "  pop rbp", "  ret"
  ,"hc_print_bool:", "  push rbp", "  mov rbp, rsp"
  ,"  test rdi, rdi", "  lea rdi, [rip + .Lhc_false]", "  lea rsi, [rip + .Lhc_true]"
  ,"  cmovne rdi, rsi", "  xor eax, eax", "  call printf@PLT"
  ,"  xor eax, eax", "  pop rbp", "  ret"
  ,"hc_read:", "  push rbp", "  mov rbp, rsp", "  sub rsp, 16"
  ,"  mov QWORD PTR [rbp - 8], 0", "  lea rsi, [rbp - 8]"
  ,"  lea rdi, [rip + .Lhc_fmt_read]", "  xor eax, eax", "  call scanf@PLT"
  ,"  mov rax, QWORD PTR [rbp - 8]", "  leave", "  ret"]
