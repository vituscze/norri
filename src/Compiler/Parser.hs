module Compiler.Parser
    (
      expr  -- todo
    , def
    , typ
    , dataDef
    )
    where

import Control.Applicative hiding ((<|>), empty, many)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

import Compiler.AST
import Compiler.Lexer

var :: Parser Expr
var = Var <$> anyIdent

lambda :: Parser Expr
lambda = flip (foldr Lam)
    <$> (reservedOp "\\"
     *> many1 lowIdent)
    <*> (reservedOp "->"
     *> expr)

letin :: Parser Expr
letin = Let <$> (reserved "let"
             *> def `sepBy1` semi)
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
def = ValueDef <$> lowIdent
               <*> (reservedOp "="
                *> expr)

-- TODO: Be more efficient?
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
    table = [[Ex.Infix (reservedOp "->" *> pure arrow) Ex.AssocRight]]
    arrow = TyApp . TyApp (TyData "->")

-- | Parser for a type signature 'TypeSig'.
typeSig :: Parser TypeSig
typeSig = Sig <$> lowIdent
              <*> (reservedOp ":"
               *> typ)

vbar :: Parser ()
vbar = reservedOp "|"

tyCon :: Parser TyCon
tyCon = TyCon <$> upIdent
              <*> many lowIdent

variant :: Parser Variant
variant = DataCon <$> upIdent
                  <*> many (tyVar <|> parens typ)

dataDef :: Parser DataDef
dataDef = DataDef <$> (reserved "data"
                   *> tyCon)
                  <*> (reserved "=" *> variant `sepBy1` vbar
                  <|> pure [])
