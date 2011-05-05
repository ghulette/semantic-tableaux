module Parser (parseFormula) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Lexer
import Expr

atom :: Parser Expr
atom = lexeme $ do
  x <- lower
  xs <- many alphaNum
  return $ Atom (x:xs)

formula :: Parser Expr
formula = Ex.buildExpressionParser table factor
  where prefixOp x f = Ex.Prefix (reservedOp x >> return f)
        infixOp  x f = Ex.Infix (reservedOp x >> return f)
        infixId  x f = Ex.Infix (reserved x >> return f)
        table = [[prefixOp "~"  Neg],
                 [infixId "and" And Ex.AssocLeft],
                 [infixId "or"  Or  Ex.AssocLeft],
                 [infixId "xor" Xor Ex.AssocLeft],
                 [infixOp "->"  Imp Ex.AssocLeft],
                 [infixOp "<->" Eqv Ex.AssocLeft]]
        factor =  parens formula
              <|> atom
              <?> "proposition"

-- Wrappers

parseFormula :: String -> Either ParseError Expr
parseFormula = parse (allOf formula) "Formula"
