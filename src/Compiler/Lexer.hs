-- | Provides few basic parsers and parser combinators that deal with
--   whitespace and comments properly.
module Compiler.Lexer
    (
      parens
    , anyIdent
    , upIdent
    , lowIdent
    , reserved
    , reservedOp
    , semi
    , integer
    , everything
    )
    where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- TODO: Add C++ keywords and fix.
lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser style
  where
    reservedOps   = ["->", "\\", ":", "|"]
    reservedNames = ["data", "let", "in"]
    identLetter   = char '_' <|> alphaNum

    style = haskellStyle
        { Tok.reservedOpNames = reservedOps
        , Tok.reservedNames   = reservedNames
        , Tok.identLetter     = identLetter
        }

-- | Parses a value of type @a@ inside parentheses.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parses any kind of identifier.
anyIdent :: Parser String
anyIdent = Tok.identifier lexer

-- | Parses an identifier which starts with an upper case letter.
upIdent :: Parser String
upIdent = try (mfilter (isUpper . head) anyIdent) <?> "upper case identifier"

-- | Parses an identifier which starts with a lower case letter.
lowIdent :: Parser String
lowIdent = try (mfilter (isLower . head) anyIdent) <?> "lower case identifier"

-- | Parses a reserved keyword given by a 'String'.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Parses a reserved operator keyword given by a 'String'.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Parses a semicolon and throws away the result.
semi :: Parser ()
semi = () <$ Tok.semi lexer

-- | Parses an integer literal.
integer :: Parser Integer
integer = Tok.integer lexer

-- | Forces a parser to parse whole input stream.
everything :: Parser a -> Parser a
everything p = Tok.whiteSpace lexer *> p <* eof
