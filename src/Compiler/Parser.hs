-- | Provides main parsers for the language.
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
    , scheme

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
import Compiler.TypeChecking.Free  -- For type quantification.

-- | Parse a variable of any sort (starting with lower or upper case letter).
var :: Parser Expr
var = Var <$> anyIdent

-- | Parse a lambda abstraction.
lambda :: Parser Expr
lambda = flip (foldr Lam)
    <$> (reservedOp "\\"
     *> many1 lowIdent)
    <*> (reservedOp "->"
     *> expr)

-- | Parse a let-in expression.
letin :: Parser Expr
letin = Let <$> (reserved "let"
             *> defOnly `sepBy1` semi)
            <*> (reserved "in"
             *> expr)

-- | Parse an integer literal.
numLit :: Parser Expr
numLit = NumLit <$> natural

-- | Parse a boolean literal.
boolLit :: Parser Expr
boolLit = BoolLit <$>
      ( True  <$ reserved "True"
    <|> False <$ reserved "False"
      )

-- | Parse an 'Expr' without any operators or application at the outermost
--   level.
factor :: Parser Expr
factor =  parens expr
      <|> var
      <|> numLit
      <|> boolLit
      <|> lambda
      <|> letin
      <?> "variable, literal, lambda or let"

-- | Parse an 'Expr' without any operator at the outermost level.
manyFactors :: Parser Expr
manyFactors = foldl1 App <$> many1 factor

-- | Parse an 'Expr'.
expr :: Parser Expr
expr = exprOp >>= ((<|>) <$> lassocP <*> return)
  where
    -- Implementation inspired by 'buildExpressionParser' from parsec.
    lassocP x = do
        reservedOp ":"
        t <- scheme
        lassocP1 (SetType x t)

    lassocP1 = (<|>) <$> lassocP <*> return

-- | Parse an 'Expr' without type unification construct (@SetType@).
exprOp :: Parser Expr
exprOp = Ex.buildExpressionParser
    [ [ prefix "~" "neg"
      , prefix "!" "not_"
      ]

    , [ binary "*" "mul" Ex.AssocLeft
      , binary "/" "div" Ex.AssocLeft
      , binary "%" "rem" Ex.AssocLeft
      ]

    , [ binary "+" "plus"  Ex.AssocLeft
      , binary "-" "minus" Ex.AssocLeft
      ]

    , [ binary "<"  "lt"  Ex.AssocNone
      , binary "<=" "le"  Ex.AssocNone
      , binary ">"  "gt"  Ex.AssocNone
      , binary ">=" "ge"  Ex.AssocNone
      , binary "==" "eq"  Ex.AssocNone
      , binary "/=" "neq" Ex.AssocNone
      ]

    , [ binary "&&" "and_" Ex.AssocRight
      ]

    , [ binary "^"  "xor_" Ex.AssocRight
      ]

    , [ binary "||" "or_"  Ex.AssocRight
      ]
    ]
    manyFactors
  where
    prefix op name = Ex.Prefix (do
        reservedOp op
        return $ App (Var name))

    binary op name = Ex.Infix (do
        reservedOp op
        return $ App . App (Var name))

-- | Parse a definition given the identifier of the entity being defined.
def :: String -> Parser ValueDef
def s = (ValueDef s .) . flip (foldr Lam)
     <$> many lowIdent
     <*> (reservedOp "=" *> expr)

-- | Parse a whole definition.
--
--   Unlike 'def', this also parses the identifier on its own.
defOnly :: Parser ValueDef
defOnly = lowIdent >>= def

-- | Parse a type name, which is an 'identifier' starting with an upper
--   case letter.
tyName :: Parser Type
tyName = TyData <$> upIdent

-- | Parse a type variable, which as an 'identifier' starting with a lower
--   case letter.
tyVar :: Parser Type
tyVar = TyVar <$> lowIdent

-- | Parse a 'Type' without any operators or type application at the outermost
--   level.
tyFactor :: Parser Type
tyFactor =  parens typ
        <|> tyName
        <|> tyVar
        <?> "type variable or name"

-- | Parse a 'Type' without any operators at the outermost level.
manyTyFactors :: Parser Type
manyTyFactors = foldl1 TyApp <$> many1 tyFactor

-- | Parse a 'Type'.
typ :: Parser Type
typ = Ex.buildExpressionParser table manyTyFactors
  where
    table = [[Ex.Infix (reservedOp "->" *> pure TyArr) Ex.AssocRight]]

-- | Parse a type 'Scheme'.
scheme :: Parser Scheme
scheme = (\t -> quantify (free t) t) <$> typ

-- | Parse a type signature 'TypeSig'.
typeSig :: String -> Parser TypeSig
typeSig s = Sig s <$> (reservedOp ":" *> scheme)

-- | Parse a vertical bar.
vbar :: Parser ()
vbar = reservedOp "|"

-- | Parse a type constructor 'TyCon'.
--
--   Note that since eliminator's name is just a noncapitalized data type name,
--   this parser also checks if this name won't conflict with any non language
--   keyword.
tyCon :: Parser TyCon
tyCon = TyCon <$> up'Ident
              <*> many lowIdent

-- | Parse a 'Variant' of a data type definition.
variant :: Parser Variant
variant = DataCon <$> upIdent
                  <*> many (tyVar <|> tyName <|> parens typ)

-- | Parse a data type definition.
--
--   Note that this parser also allows empty data definitions.
dataDef :: Parser DataDef
dataDef = DataDef <$> (reserved "data"
                   *> tyCon)
                  <*> (reserved "=" *> variant `sepBy1` vbar
                  <|> pure [])

-- | Parse a definition or a type signature.
--
--   This parser exists to factor out the requirement of both
--   'def' and 'typeSig' to parse identifier first. This way we can avoid
--   backtracking after parsing identifier and realizing that what follows
--   isn't a type signature when definition is expected or vice versa.
defOrSig :: Parser TopLevel
defOrSig = lowIdent >>= \i ->
    (Value <$> def i) <|> (Type <$> typeSig i)


-- | Parse a 'TopLevel' entity.
topLevel :: Parser TopLevel
topLevel = (Data <$> dataDef) <|> defOrSig

-- | Parse whole 'Module'.
--
--   It also consumes everything in the input stream.
file :: Parser Module
file = Module <$> everything (topLevel `sepBy` semi)
