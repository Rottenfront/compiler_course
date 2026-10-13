module Course.Parser.Caml (program) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Course.Syntax
import Course.Parser.Common

tok :: Tok.TokenParser ()
tok = lexer Caml

program :: Parser Program
program = Tok.whiteSpace tok *> many1 declaration <* eof

declaration :: Parser Function
declaration = do
  pos <- getPosition
  (Tok.reserved tok "define" <|> Tok.reserved tok "func"
    <|> (Tok.reserved tok "let" >> optional (Tok.reserved tok "rec")))
  name <- Tok.identifier tok
  params <- many (parameter tok)
  Tok.reservedOp tok ":"
  result <- typeParser tok
  Tok.reservedOp tok "="
  body <- expression
  pure (Function pos name params result body)

expression :: Parser Expr
expression = buildExpressionParser table term
  where
    binary name op = Infix (do
      pos <- getPosition
      Tok.reservedOp tok name
      pure (\a b -> Expr pos (Binary op a b))) AssocLeft
    word name op = Infix (do
      pos <- getPosition
      Tok.reserved tok name
      pure (\a b -> Expr pos (Binary op a b))) AssocLeft
    -- Preserve the original precedence, including XOR below OR.
    table = [[binary "*" Mul, binary "/" Div]
            ,[binary "+" Add, binary "-" Sub]
            ,[binary "<=" Le, binary ">=" Ge, binary "<" Lt, binary ">" Gt]
            ,[binary "==" Eq, binary "!=" Ne]
            ,[binary "&&" And, word "and" And], [binary "||" Or, word "or" Or]
            ,[binary "^" Xor, word "xor" Xor]
            ,[binary ";" Sequence]]

term :: Parser Expr
term = letExpr <|> ifExpr <|> application <|> atom

letExpr :: Parser Expr
letExpr = located $ do
  Tok.reserved tok "let"
  name <- Tok.identifier tok
  Tok.reservedOp tok "="
  value <- expression
  Tok.reserved tok "in"
  Let name value <$> expression

ifExpr :: Parser Expr
ifExpr = located $ do
  Tok.reserved tok "if"
  condition <- expression
  Tok.reserved tok "then"
  yes <- expression
  Tok.reserved tok "else"
  If condition yes <$> expression

application :: Parser Expr
application = located $ Apply <$> Tok.identifier tok <*> many argument
  where
    -- Negative arguments require parentheses, as in OCaml application syntax.
    argument = lookAhead (noneOf "-") *> atom

atom :: Parser Expr
atom = located (Boolean True <$ Tok.reserved tok "true"
            <|> Boolean False <$ Tok.reserved tok "false"
            <|> Number <$> (integer64 tok False <|> try negative)
            <|> Apply <$> Tok.identifier tok <*> pure [])
   <|> Tok.parens tok (located (pure Unit) <* lookAhead (char ')') <|> expression)
  where
    -- Signed literals require adjacency, so `a - 1` remains subtraction.
    negative = Tok.lexeme tok $ do
      _ <- char '-'
      digits <- many1 digit
      let n = negate (read digits :: Integer)
      if n < -9223372036854775808
        then parserFail "integer literal is outside the signed 64-bit range"
        else pure (fromInteger n)
