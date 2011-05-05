module Lexer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language (haskellStyle)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser haskellStyle 
  { Tok.reservedOpNames = ["->","<->","~"]
  , Tok.reservedNames   = ["or","and","xor"]
  }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

natural :: Parser Integer
natural = Tok.natural lexer

comma :: Parser ()
comma = Tok.comma lexer >> return ()

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

allOf :: Parser a -> Parser a
allOf p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
