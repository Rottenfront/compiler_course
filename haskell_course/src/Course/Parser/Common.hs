module Course.Parser.Common (lexer, typeParser, parameter, located, integer64) where

import Data.Int (Int64)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Course.Syntax

lexer :: Language -> Tok.TokenParser ()
lexer language = Tok.makeTokenParser emptyDef
  { Tok.commentStart = if language == Caml then "(*" else "#|"
  , Tok.commentEnd = if language == Caml then "*)" else "|#"
  , Tok.commentLine = if language == Caml then "//" else ";"
  , Tok.nestedComments = True
  , Tok.identStart = letter <|> char '_'
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  , Tok.opStart = oneOf "+-*/=!<>&|^:;"
  , Tok.opLetter = oneOf "+-*/=!<>&|^:;"
  , Tok.reservedNames = ["define", "func", "let", "rec", "in", "if", "then", "else"
                       ,"true", "false", "int", "bool", "and", "or", "xor", "begin"]
  , Tok.reservedOpNames = ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="
                         ,"&&", "||", "^", ";", ":", "="]
  }

typeParser :: Tok.TokenParser () -> Parser Type
typeParser tok =
      (Tok.reserved tok "int" >> pure TInt)
  <|> (Tok.reserved tok "bool" >> pure TBool)
  <|> (Tok.parens tok (pure TUnit))
  <|> (TFunction <$> Tok.brackets tok (many (typeParser tok)) <*> typeParser tok)
  <?> "type (int, bool, (), or [argument types] result)"

parameter :: Tok.TokenParser () -> Parser (Name, Type)
parameter tok = Tok.parens tok $ do
  name <- Tok.identifier tok
  Tok.reservedOp tok ":"
  ty <- typeParser tok
  pure (name, ty)

located :: Parser ExprNode -> Parser Expr
located parser = Expr <$> getPosition <*> parser

integer64 :: Tok.TokenParser () -> Bool -> Parser Int64
integer64 tok signed = do
  n <- if signed then Tok.integer tok else Tok.natural tok
  if n < toInteger (minBound :: Int64) || n > toInteger (maxBound :: Int64)
    then parserFail "integer literal is outside the signed 64-bit range"
    else pure (fromInteger n)
