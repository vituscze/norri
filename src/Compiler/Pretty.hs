-- | Expression and type pretty printing.
module Compiler.Pretty
    (
    -- * Running pretty printers
      runP

    -- * Pretty printing
    , prettyType
    , prettyScheme
    , prettyCon
    , prettyTyCon
    , prettyDD
    , prettyExpr
    , prettyVD
    , prettyTS
    , prettyAssume
    )
    where

import Data.List

import Compiler.AST

-- | Extracts the 'String' in a 'ShowS' value.
runP :: ShowS -> String
runP = ($ "")

-- | Transform a 'String' into 'ShowS'
str :: String -> ShowS
str = showString

-- | Concatenate a list of 'ShowS' "strings".
concatD :: [ShowS] -> ShowS
concatD = foldr (.) id

-- | Pretty print a type given a precedence level of the surrounding context.
prettyTypePrec :: Int -> Type -> ShowS
prettyTypePrec = go
  where
    -- Precendence levels.
    [apP, arP] = [10, 1] :: [Int]

    go _ (TyData d)  = str d
    go _ (TyGen i)   = str "g" . shows i
    go _ (TyVar v)   = str v
    go p (TyApp t u) = showParen (p > apP) . concatD $
        [ go apP t
        , str " "
        , go (apP + 1) u
        ]
    go p (TyArr t u) = showParen (p > arP) . concatD $
        [ go (arP + 1) t
        , str " -> "
        , go arP u
        ]

-- | Pretty print a type.
--
-- >>> runP $ prettyType (TyArr (TyVar "a") (TyVar "b"))
-- "a -> b"
prettyType :: Type -> ShowS
prettyType = prettyTypePrec 0

-- | Pretty print a type scheme.
prettyScheme :: Scheme -> ShowS
prettyScheme (Scheme _ ts) = prettyType ts

-- | Pretty print a single data constructor.
prettyCon :: Variant -> ShowS
prettyCon (DataCon n ts) =
    concatD . intersperse (str " ") . (str n:) . map (prettyTypePrec 11) $ ts

-- | Pretty print a type constructor.
prettyTyCon :: TyCon -> ShowS
prettyTyCon (TyCon n tvs) =
    concatD . intersperse (str " ") . (str n:) . map str $ tvs

-- | Pretty print a data definition.
prettyDD :: DataDef -> ShowS
prettyDD (DataDef tyc vs) = concatD
    [ str "data "
    , prettyTyCon tyc
    , if null vs
        then id
        else str " = "
    , concatD . intersperse (str " | ") . map prettyCon $ vs
    ]

-- | Pretty print an expression given the precedence level of the surrounding
--   context.
prettyExprPrec :: Int -> Expr -> ShowS
prettyExprPrec = go
  where
    [apP, stP, biP] = [10, 1, 0] :: [Int]

    go _ (Var v) = str v
    go p l@(Lam _ _) = showParen (p > biP) . concatD $
        [ str "\\"
        , concatD . intersperse (str " ") . map str $ vs
        , str " -> "
        , go biP ex'
        ]
      where
        (vs, ex') = dig l

        dig (Lam x e) = (x:xs, e')
          where
            (xs, e') = dig e
        dig e = ([], e)
    go p (App e1 e2) = showParen (p > apP) . concatD $
        [ go apP e1
        , str " "
        , go (apP + 1) e2
        ]
    go p (Let ds e) = showParen (p > biP) . concatD $
        [ str "let "
        , concatD . intersperse (str "; ") . map prettyVD $ ds
        , str " in "
        , go biP e
        ]
    go p (SetType e t) = showParen (p > stP) . concatD $
        [ go stP e
        , str " : "
        , prettyScheme t
        ]
    go _ (NumLit  i) = shows i
    go _ (BoolLit b) = shows b

    -- Stay as close to the original code as possible.
    go p (Fix _ e) = go p e


-- | Pretty print an expression.
--
-- >>> runP $ prettyExpr (Lam "x" (App (Var "id") (Var "x")))
-- "\x -> id x"
prettyExpr :: Expr -> ShowS
prettyExpr = prettyExprPrec 0

-- | Pretty print a value definition.
--
-- >>> runP $ prettyVD (ValueDef "id" (Lam "y" (Var "y")))
-- "id y = y"
prettyVD :: ValueDef -> ShowS
prettyVD (ValueDef n ex) = concatD
    [ concatD . intersperse (str " ") . (str n:) . map str $ vs
    , str " = "
    , prettyExpr ex'
    ]
  where
    (vs, ex') = dig ex

    -- Since we do not print 'Fix', we can safely skip it.
    dig (Fix _ e) = dig e
    dig (Lam x e) = (x:xs, e')
      where
        (xs, e') = dig e
    dig e = ([], e)

-- | Pretty print a type signature.
--
-- >>> runP $ prettyTS (Sig "x" (TyData "Int"))
-- "x : Int"
prettyTS :: TypeSig -> ShowS
prettyTS (Sig n ts) = concatD
    [ str n
    , str " : "
    , prettyScheme ts
    ]

-- | Pretty print an assumption.
--
-- >>> runP $ prettyAssume (Sig "x" (TyArr (TyData "Int") (TyData "Bool")))
-- "assume x : Int -> Bool"
prettyAssume :: TypeSig -> ShowS
prettyAssume s = concatD
    [ str "assume "
    , prettyTS s
    ]
