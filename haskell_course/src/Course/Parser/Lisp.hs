module Course.Parser.Lisp (program) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Course.Syntax
import Course.Parser.Common

tok :: Tok.TokenParser ()
tok = lexer Lisp

group :: Parser a -> Parser a
group p = Tok.parens tok p <|> Tok.brackets tok p <|> Tok.braces tok p

program :: Parser Program
program = Tok.whiteSpace tok *> many1 declaration <* eof

declaration :: Parser Function
declaration = group $ do
  pos <- getPosition
  Tok.reserved tok "define" <|> Tok.reserved tok "func"
  (name, params, result) <- group $ do
    name <- Tok.identifier tok
    params <- many (group $ do
      n <- Tok.identifier tok
      Tok.reservedOp tok ":"
      ty <- typeParser tok
      pure (n, ty))
    Tok.reservedOp tok ":"
    result <- typeParser tok
    pure (name, params, result)
  Function pos name params result <$> expression

expression :: Parser Expr
expression = located (Boolean True <$ (Tok.reserved tok "true" <|> Tok.reserved tok "#t")
                 <|> Boolean False <$ (Tok.reserved tok "false" <|> Tok.reserved tok "#f")
                 <|> Number <$> integer64 tok True
                 <|> Apply <$> Tok.identifier tok <*> pure [])
         <|> group (located form)

form :: Parser ExprNode
form = conditional <|> binding <|> operation <|> call <|> pure Unit
  where
    conditional = do
      Tok.reserved tok "if"
      If <$> expression <*> expression <*> expression
    binding = do
      Tok.reserved tok "let"
      pairs <- group (many (group ((,) <$> Tok.identifier tok <*> expression)))
      body <- expression
      -- Bindings are sequential, matching the original Racket-like syntax.
      pure (exprNode (foldr (\(n, value) rest -> Expr (exprPosition value) (Let n value rest)) body pairs))
    operation = do
      op <- choice (map parseOperator operatorNames)
      args <- many1 expression
      pure (exprNode (foldl1 (\a b -> Expr (exprPosition a) (Binary op a b)) args))
    parseOperator (name, op)
      | name `elem` ["and", "or", "xor", "begin"] = op <$ Tok.reserved tok name
      | otherwise = op <$ Tok.reservedOp tok name
    call = Apply <$> Tok.identifier tok <*> many expression
