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

    -- * Shortcuts
    , Name
    , TyName
    , TyVar
    )
    where

type Name = String

type TyName = String
type TyVar = String

-- | AST for a module consists of a list of top level definitions and/or
--   declarations.
data Module
    = Module [TopLevel]
    deriving (Show)


-- | A top level definition/declaration is either a data definition,
--   value definition or a type signature.
data TopLevel
    = Data DataDef    -- ^ Data definition.
    | Value ValueDef  -- ^ Value definition.
    | Type TypeSig    -- ^ Type signature.
    deriving (Show)


-- | AST for a data definition, which consists of named type constructor and
--   a list of variants.
data DataDef
    = DataDef TyCon [Variant]
    deriving (Show)

-- | AST for a type constructor, which consits of its name and a (possibly
--   empty) list of type variables.
data TyCon
    = TyCon TyName [TyVar]  -- TODO: abstract representation for type variables
    deriving (Show)

-- | AST for a variant of a data type, which consits of a named constructor
--   and a (possibly empty) list of fields, which are specified by their type.
data Variant
    = DataCon Name [Type]
    deriving (Show)


-- | AST for a value definition, which consists of a name and an expression.
data ValueDef
    = ValueDef Name Expr
    deriving (Show)

-- | AST for an expression, which can be either a variable, lambda abstraction,
--   application of two expressions, a @let@ declaration, numeric literal
--   or fix point combinator.
data Expr
    = Var Name             -- ^ Single variable.
    | Lam Name Expr        -- ^ Lambda abstraction.
    | App Expr Expr        -- ^ Application of two expressions.
    | Let [ValueDef] Expr  -- ^ @let@ declaration. @let decls in expr@
                           --   is represented as @Let [[decls]] [[expr]]@.
    | SetType Expr Type    -- ^ Explicit declaration of expression type.
    | NumLit Integer       -- ^ Integer literal.
    | Fix Name Expr        -- ^ Fix point operator.
    deriving (Show)


-- | AST for a type signature, which consists of the name of the entity and
--   a type.
data TypeSig
    = Sig Name Type
    deriving (Show)

-- | AST for a type, which can either be a concrete type, type variable or
--   application of two types (with correct kind).
data Type
    = TyData TyName    -- ^ Concrete type.
    | TyVar TyVar      -- ^ Type variable.
    | TyApp Type Type  -- ^ Application of a type constructor.
    | TyArr Type Type  -- ^ Function types.
    deriving (Show)
