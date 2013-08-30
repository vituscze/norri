module Compiler.Lexer
    (
      lexer
    , parens
    , identifier
    , reserved
    , reservedOp
    , semi
    , integer
    , everything
    )
    where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser style
  where
    reservedOps   = ["->", "\\", ":", "|"]  -- TODO: Arithmetic?
    reservedNames = ["data", "let", "in"]  -- TODO: Add where
    identLetter   = alphaNum <|> oneOf "_"

    style = haskellStyle
        { Tok.reservedOpNames = reservedOps
        , Tok.reservedNames   = reservedNames
        , Tok.identLetter     = identLetter
        }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

semi :: Parser ()
semi = () <$ Tok.semi lexer

integer :: Parser Integer
integer = Tok.integer lexer

everything :: Parser a -> Parser a
everything p = Tok.whiteSpace lexer *> p <* eof
