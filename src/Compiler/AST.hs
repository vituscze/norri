-- | Provides data type representation for the abstract syntax tree of
--   the language.
module Compiler.AST
    (
    -- * Module structure
      Module(..)
    , TopLevel(..)

    -- * Data definitions
    , DataDef(..)
    , TyCon(..)
    , Variant(..)

    -- * Value definitions
    , ValueDef(..)
    , Expr(..)

    -- * Type definitions
    , TypeSig(..)
    , Type(..)
    , Scheme(..)

    -- * Shortcuts
    , Name
    , TyName
    , TyVar
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

type Name = String

type TyName = String
type TyVar = String

-- | AST for a module consists of a list of top level definitions and/or
--   declarations.
data Module
    = Module [TopLevel]
    deriving (Eq, Show)


-- | A top level definition/declaration is either a data definition,
--   value definition or a type signature.
data TopLevel
    = Data DataDef    -- ^ Data definition.
    | Value ValueDef  -- ^ Value definition.
    | Type TypeSig    -- ^ Type signature.
    deriving (Eq, Show)


-- | AST for a data definition, which consists of named type constructor and
--   a list of variants.
data DataDef
    = DataDef TyCon [Variant]
    deriving (Eq, Show)

-- | AST for a type constructor, which consits of its name and a (possibly
--   empty) list of type variables.
data TyCon
    = TyCon TyName [TyVar]
    deriving (Eq, Show)

-- | AST for a variant of a data type, which consits of a named constructor
--   and a (possibly empty) list of fields, which are specified by their type.
data Variant
    = DataCon Name [Type]
    deriving (Eq, Show)


-- | AST for a value definition, which consists of a name and an expression.
data ValueDef
    = ValueDef Name Expr
    deriving (Eq, Show)

-- | AST for an expression, which can be either a variable, lambda abstraction,
--   application of two expressions, a @let@ declaration, numeric literal
--   or fix point combinator.
data Expr
    = Var Name             -- ^ Single variable.
    | Lam Name Expr        -- ^ Lambda abstraction.
    | App Expr Expr        -- ^ Application of two expressions.
    | Let [ValueDef] Expr  -- ^ @let@ declaration. @let decls in expr@
                           --   is represented as @Let [[decls]] [[expr]]@.
    | SetType Expr Scheme  -- ^ Explicit declaration of expression type.
    | NumLit Integer       -- ^ Integer literal.
    | Fix Name Expr        -- ^ Fix point operator.
    deriving (Eq, Show)


-- | AST for a type signature, which consists of the name of the entity and
--   a type scheme.
data TypeSig
    = Sig Name Scheme
    deriving (Eq, Show)

-- | AST for a type, which can either be a concrete type, type variable or
--   application of two types (with correct kind).
data Type
    = TyData TyName    -- ^ Concrete type.
    | TyGen Int        -- ^ Quantified type, used only in type schemes.
    | TyVar TyVar      -- ^ Type variable.
    | TyApp Type Type  -- ^ Application of a type constructor.
    | TyArr Type Type  -- ^ Function types.
    deriving (Show)

-- | 'Type's need special equality because for example types
--   @forall a b. a -> b@ and @forall a b. b -> a@ should be equal.
--
--   This implementation basically checks if constructors other than 'TyGen' are
--   equal and if there exists a bijective function from 'TyGen's on one side to
--   'TyGen's on the other side.
instance Eq Type where
    t == u = isJust (go (Map.empty, Set.empty) t u)
      where
        bToM True  x = Just x
        bToM False _ = Nothing

        go ms (TyData d1) (TyData d2) = bToM (d1 == d2) ms
        go (m, s) (TyGen i1) (TyGen i2) = case Map.lookup i1 m of
            Just i2'
                | i2 == i2' -> return (m, s)
                | otherwise -> Nothing
            _
                | i2 `Set.member` s -> Nothing
                | otherwise -> return (Map.insert i1 i2 m, Set.insert i2 s)
        go ms (TyVar v1) (TyVar v2)   = bToM (v1 == v2) ms
        go ms (TyApp t1 t2) (TyApp u1 u2) = do
            ms' <- go ms t1 u1
            go ms' t2 u2
        go ms (TyArr t1 t2) (TyArr u1 u2) = do
            ms' <- go ms t1 u1
            go ms' t2 u2
        go _ _ _ = Nothing

-- | A type scheme, which is a 'Type' with possibly quantified type variables.
--   The number of quantified variables is given by the first field.
data Scheme = Scheme Int Type
    deriving (Eq, Show)
