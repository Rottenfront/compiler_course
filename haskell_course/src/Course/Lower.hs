module Course.Lower (lowerProgram) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Course.Syntax
import Course.IR

data Supply = Supply { nextSlot :: Int, nextLabel :: Int, reversedCode :: [Instruction] }
type Lower = State Supply
type Locals = Map.Map Name Slot

slot :: Lower Slot
slot = do
  n <- gets nextSlot
  modify' (\s -> s { nextSlot = n + 1 })
  pure n

label :: Lower Label
label = do
  n <- gets nextLabel
  modify' (\s -> s { nextLabel = n + 1 })
  pure n

emit :: Instruction -> Lower ()
emit ins = modify' (\s -> s { reversedCode = ins : reversedCode s })

lowerProgram :: Program -> [FunctionIR]
lowerProgram = map lowerFunction

lowerFunction :: Function -> FunctionIR
lowerFunction f =
  let params = zip (map fst (functionParameters f)) [0..]
      count = length params
      final = execState (tailExpr (functionName f) (Map.fromList params) (functionBody f)) (Supply count 0 [])
  in FunctionIR (functionName f) (map snd params) (nextSlot final) (reverse (reversedCode final))

value :: Locals -> Expr -> Lower Slot
value env (Expr _ node) = case node of
  Number n -> constant n
  Boolean b -> constant (if b then 1 else 0)
  Unit -> constant 0
  Apply name args -> case Map.lookup name env of
    Just local -> do
      dest <- slot
      emit (Copy dest local)
      pure dest
    Nothing -> do
      arguments <- mapM (value env) args
      dest <- slot
      emit (Call dest name arguments)
      pure dest
  Let name initial body -> do
    initialSlot <- value env initial
    value (Map.insert name initialSlot env) body
  If cond yes no -> conditional env cond yes no
  Binary Sequence a b -> value env a >> value env b
  Binary And a b -> conditional env a b (Expr (exprPosition a) (Boolean False))
  Binary Or a b -> conditional env a (Expr (exprPosition a) (Boolean True)) b
  Binary op a b -> do
    left <- value env a
    right <- value env b
    dest <- slot
    emit (Operation dest op left right)
    pure dest
  where
    constant n = do
      dest <- slot
      emit (Constant dest n)
      pure dest

conditional :: Locals -> Expr -> Expr -> Expr -> Lower Slot
conditional env cond yes no = do
  test <- value env cond
  dest <- slot
  yesLabel <- label
  noLabel <- label
  doneLabel <- label
  emit (Branch test yesLabel noLabel)
  emit (Mark yesLabel)
  a <- value env yes
  emit (Copy dest a)
  emit (Jump doneLabel)
  emit (Mark noLabel)
  b <- value env no
  emit (Copy dest b)
  emit (Mark doneLabel)
  pure dest

tailExpr :: Name -> Locals -> Expr -> Lower ()
tailExpr self env expr@(Expr _ node) = case node of
  Apply name args | name == self && Map.notMember name env ->
    mapM (value env) args >>= emit . TailCall
  Let name initial body -> do
    initialSlot <- value env initial
    tailExpr self (Map.insert name initialSlot env) body
  Binary Sequence a b -> value env a >> tailExpr self env b
  If cond yes no -> do
    test <- value env cond
    yesLabel <- label
    noLabel <- label
    emit (Branch test yesLabel noLabel)
    emit (Mark yesLabel)
    tailExpr self env yes
    emit (Mark noLabel)
    tailExpr self env no
  _ -> value env expr >>= emit . Return
