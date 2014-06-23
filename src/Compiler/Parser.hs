-- | Provides main parsers for the language.
--
--   TODO: Deal with C++ keywords and implementation reserved keywords.
module Compiler.Parser
    (
    -- * Expressions
      expr

    -- * Value definitions
    , def
    , defOnly

    -- * Type signatures
    , typ
    , typeSig

    -- * Data definitions
    , dataDef

    -- * Top level entities
    , topLevel

    -- * Files
    , file
    )
    where

import Control.Applicative hiding ((<|>), empty, many)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

import Compiler.AST
import Compiler.Lexer

-- | Parses a variable of any sort (starting with lower or upper case letter).
var :: Parser Expr
var = Var <$> anyIdent

-- | Parses a lambda abstraction.
lambda :: Parser Expr
lambda = flip (foldr Lam)
    <$> (reservedOp "\\"
     *> many1 lowIdent)
    <*> (reservedOp "->"
     *> expr)

-- | Parses a let-in expression.
letin :: Parser Expr
letin = Let <$> (reserved "let"
             *> defOnly `sepBy1` semi)
            <*> (reserved "in"
             *> expr)

-- | Parses an integer literal.
numLit :: Parser Expr
numLit = NumLit <$> integer

-- | Parses an 'Expr' without any operators or application at the outermost
--   level.
factor :: Parser Expr
factor =  parens expr
      <|> var
      <|> numLit
      <|> lambda
      <|> letin
      <?> "variable, literal, lambda or let"

-- | Parses an 'Expr' without any operator at the outermost level.
manyFactors :: Parser Expr
manyFactors = foldl1 App <$> many1 factor

-- | Parses an 'Expr' given a parser for an operator for forcing
--   type unification.
exprOp :: Parser (Expr -> Type -> Expr) -> Parser Expr
exprOp colon = manyFactors >>= ((<|>) <$> lassocP <*> return)
  where
    -- Implementation inspired by 'buildExpressionParser' from parsec.
    lassocP x = do
        f <- colon
        t <- typ
        lassocP1 (f x t)

    lassocP1 = (<|>) <$> lassocP <*> return

-- | Parses an 'Expr'.
expr :: Parser Expr
expr = exprOp (reservedOp ":" *> pure SetType)

-- | Parses a definition given the identifier of the entity being defined.
def :: String -> Parser ValueDef
def s = ValueDef s <$> (reservedOp "=" *> expr)

-- | Parses a whole definition.
--
--   Unlike 'def', this also parses the identifier on its own.
defOnly :: Parser ValueDef
defOnly = lowIdent >>= def

-- | Parses a type name, which is an 'identifier' startih with an upper
--   case letter.
tyName :: Parser Type
tyName = TyData <$> upIdent

-- | Parses a type variable, which as an 'identifier' starting with a lower
--   case letter.
tyVar :: Parser Type
tyVar = TyVar <$> lowIdent

-- | Parser for a 'Type' without any operators or type application at the
--   outermost level.
tyFactor :: Parser Type
tyFactor =  parens typ
        <|> tyName
        <|> tyVar
        <?> "type variable or name"

-- | Parser for a 'Type' without any operators at the outermost level.
manyTyFactors :: Parser Type
manyTyFactors = foldl1 TyApp <$> many1 tyFactor

-- | Parser for a 'Type'.
typ :: Parser Type
typ = Ex.buildExpressionParser table manyTyFactors
  where
    table = [[Ex.Infix (reservedOp "->" *> pure TyArr) Ex.AssocRight]]

-- | Parser for a type signature 'TypeSig'.
typeSig :: String -> Parser TypeSig
typeSig s = Sig s <$> (reservedOp ":" *> typ)

-- | Parses a vertical bar.
vbar :: Parser ()
vbar = reservedOp "|"

-- | Parses a type constructor 'TyCon'.
tyCon :: Parser TyCon
tyCon = TyCon <$> upIdent
              <*> many lowIdent

-- | Parses a 'Variant' of a data type definition.
variant :: Parser Variant
variant = DataCon <$> upIdent
                  <*> many (tyVar <|> tyName <|> parens typ)

-- | Parses a data type definition.
--
--   Note that this parser also allows empty data definitions.
dataDef :: Parser DataDef
dataDef = DataDef <$> (reserved "data"
                   *> tyCon)
                  <*> (reserved "=" *> variant `sepBy1` vbar
                  <|> pure [])

-- | Parses a definition or a type signature.
--
--   This parser exists to factor out the requirement of both
--   'def' and 'typeSig' to parse identifier first. This way we can avoid
--   backtracking after parsing identifier and realizing that what follows
--   isn't a type signature when definition is expected or vice versa.
defOrSig :: Parser TopLevel
defOrSig = lowIdent >>= \i ->
    (Value <$> def i) <|> (Type <$> typeSig i)


-- | Parses a 'TopLevel' entity.
topLevel :: Parser TopLevel
topLevel = (Data <$> dataDef) <|> defOrSig

-- | Parses whole 'Module'.
--
--   Consumes everything in the input stream.
file :: Parser Module
file = Module <$> everything (topLevel `sepBy` semi)
