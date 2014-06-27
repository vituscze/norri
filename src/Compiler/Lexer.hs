-- | Provides few basic parsers and parser combinators that deal with
--   whitespace and comments properly.
module Compiler.Lexer
    (
    -- * Reserved names.
      reservedNames

    -- * Parsers and parser combinators.
    , parens
    , anyIdent
    , upIdent
    , up'Ident
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

-- | List of C++ keywords.
reservedCpp :: [String]
reservedCpp =
    [ "alignas",   "alignof",       "and",          "and_eq"
    , "asm" ,      "auto",          "bitand",       "bitor"
    , "bool",      "break",         "case",         "catch"
    , "char",      "char16_t",      "char32_t",     "class"
    , "compl",     "const",         "constexpr",    "const_cast"
    , "continue",  "decltype",      "default",      "delete"
    , "do",        "double",        "dynamic_cast", "else"
    , "enum",      "explicit",      "export",       "extern"
    , "false",     "float",         "for" ,         "friend"
    , "goto",      "if",            "inline",       "int"
    , "long",      "mutable",       "namespace",    "new"
    , "noexcept",  "not",           "not_eq",       "nullptr"
    , "operator",  "or",            "or_eq",        "private"
    , "protected", "public",        "register",     "reinterpret_cast"
    , "return",    "short",         "signed",       "sizeof"
    , "static",    "static_assert", "static_cast",  "struct"
    , "switch",    "template",      "this",         "thread_local"
    , "throw",     "true",          "try",          "typedef"
    , "typeid",    "typename",      "union",        "unsigned"
    , "using",     "virtual",       "void",         "volatile"
    , "wchar_t",   "while",         "xor",          "xor_eq"
    ]

-- | List of names reserved for implementation.
reservedImpl :: [String]
reservedImpl =
    [ "fix", "dummy"
    ]

-- | List of names reserved for actual language.
reservedLang :: [String]
reservedLang =
    [ "data", "let", "in"
    ]

-- | List of reserved names.
--
--   First three names are used in the actual language, next two are reserved
--   for implementation purposes and the rest is to make sure that the compiler
--   produces valid C++ code.
reservedNames :: [String]
reservedNames = reservedCpp ++ reservedImpl ++ reservedLang

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser style
  where
    reservedOps   = ["->", "\\", ":", "|"]
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

-- | Parses an identifier which starts with an upper case letter and is
--   not a capitalised version of a reserved name (with exception of
--   "Let", "In" and "Data").
up'Ident :: Parser String
up'Ident = try (mfilter check upIdent) <?> msg
  where
    msg = "identifier not conflicting with a keyword"

    check []     = True
    check (x:xs) = (toLower x:xs) `notElem` (reservedCpp ++ reservedImpl)

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
