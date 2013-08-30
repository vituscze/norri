module Compiler.Parser
    (
      expr  -- todo
    )
    where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Char
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

import Compiler.AST
import Compiler.Lexer

var :: Parser Expr
var = Var <$> identifier

lambda :: Parser Expr
lambda = flip (foldr Lam)
    <$> (reservedOp "\\"
     *> many1 identifier)
    <*> (reservedOp "->"
     *> expr)

letin :: Parser Expr
letin = Let <$> (reserved "let"
             *> sepBy1 def semi)
            <*> (reserved "in"
             *> expr)

numLit :: Parser Expr
numLit = NumLit <$> integer

factor :: Parser Expr
factor =  parens expr
      <|> var
      <|> numLit
      <|> lambda
      <|> letin
      <?> "variable, literal, lambda or let"

manyFactors :: Parser Expr
manyFactors = foldl1 App <$> many1 factor

exprOp :: Parser (Expr -> Type -> Expr) -> Parser Expr
exprOp colon = manyFactors >>= ((<|>) <$> lassocP <*> return)
  where
    lassocP x = do
        f <- colon
        t <- typ
        lassocP1 (f x t)

    lassocP1 = (<|>) <$> lassocP <*> return

expr :: Parser Expr
expr = exprOp (reservedOp ":" *> pure SetType)

def :: Parser ValueDef
def = ValueDef <$> identifier
               <*> (reservedOp "="
                *> expr)

-- TODO: Be more efficient?
-- | Parses a type name, which is an 'identifier' startih with an upper
--   case letter.
tyName :: Parser Type
tyName = TyData <$> try (mfilter (isUpper . head) identifier)

-- | Parses a type variable, which as an 'identifier' starting with a lower
--   case letter.
tyVar :: Parser Type
tyVar = TyVar <$> try (mfilter (isLower . head) identifier)

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
    table = [[Ex.Infix (reservedOp "->" *> pure arrow) Ex.AssocRight]]
    arrow = TyApp . TyApp (TyData "->")

-- | Parser for a type signature 'TypeSig'.
typeSig :: Parser TypeSig
typeSig = Sig <$> identifier
              <*> (reservedOp ":"
               *> typ)
