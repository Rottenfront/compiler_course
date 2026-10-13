module Course.Check (checkProgram) where

import Control.Monad (foldM, unless, when, zipWithM_)
import qualified Data.Map.Strict as Map
import Course.Syntax

type Context = Map.Map Name Type

builtins :: Context
builtins = Map.fromList [("read", TFunction [] TInt)
                       ,("print_int", TFunction [TInt] TUnit)
                       ,("print_bool", TFunction [TBool] TUnit)]

checkProgram :: Program -> Either String Program
checkProgram functions = do
  context <- foldM declare builtins functions
  case Map.lookup "main" context of
    Just (TFunction [] result) | result == TInt || result == TUnit -> pure ()
    _ -> Left "main must have no parameters and return int or ()"
  mapM_ (checkFunction context) functions
  pure functions
  where
    declare context f = at (functionPosition f) $ do
      when (Map.member (functionName f) context) $ Left ("duplicate function: " ++ functionName f)
      mapM_ firstOrder (functionResult f : map snd (functionParameters f))
      pure (Map.insert (functionName f) (TFunction (map snd (functionParameters f)) (functionResult f)) context)
    firstOrder (TFunction _ _) = Left "higher-order functions are not supported"
    firstOrder _ = Right ()

checkFunction :: Context -> Function -> Either String ()
checkFunction context f = at (functionPosition f) $ do
  local <- foldM bind Map.empty (functionParameters f)
  ty <- infer (Map.union local context) (functionBody f)
  expect (functionResult f) ty
  where
    bind local (name, ty)
      | Map.member name local = Left ("duplicate parameter: " ++ name)
      | otherwise = Right (Map.insert name ty local)

infer :: Context -> Expr -> Either String Type
infer context (Expr pos node) = at pos $ case node of
  Number _ -> pure TInt
  Boolean _ -> pure TBool
  Unit -> pure TUnit
  Apply name args -> case Map.lookup name context of
    Nothing -> Left ("unknown name: " ++ name)
    Just (TFunction parameters result) -> do
      unless (length parameters == length args) $
        Left (name ++ " expects " ++ show (length parameters) ++ " arguments, got " ++ show (length args))
      actual <- mapM (infer context) args
      zipWithM_ expect parameters actual
      pure result
    Just ty -> if null args then pure ty else Left (name ++ " is a value, not a function")
  Let name value body -> do
    ty <- infer context value
    infer (Map.insert name ty context) body
  If condition yes no -> do
    infer context condition >>= expect TBool
    a <- infer context yes
    b <- infer context no
    expect a b
    pure a
  Binary Sequence a b -> infer context a >> infer context b
  Binary op a b -> do
    left <- infer context a
    right <- infer context b
    case op of
      Eq -> equality left right
      Ne -> equality left right
      _ -> do
        let operand = if op `elem` [And, Or, Xor] then TBool else TInt
            result = if op `elem` [Add, Sub, Mul, Div] then TInt else TBool
        expect operand left
        expect operand right
        pure result
  where
    equality a b = expect a b >> pure TBool

expect :: Type -> Type -> Either String ()
expect expected actual = unless (expected == actual) $
  Left ("expected " ++ prettyType expected ++ ", got " ++ prettyType actual)

at :: Show a => a -> Either String b -> Either String b
at pos = either (Left . ((show pos ++ ": ") ++)) Right
