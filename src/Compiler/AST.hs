module Compiler.AST
    (
    -- * Module structure.
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
    )
    where

type Name = String

type TyName = String
type TyVar = String

-- | AST for a module consists of a list of top level definitions and/or
--   declarations.
data Module
    = Module [TopLevel]


-- | A top level definition/declaration is either a data definition,
--   value definition or a type signature.
data TopLevel
    = Data DataDef    -- ^ A data definition.
    | Value ValueDef  -- ^ A value definition.
    | Type TypeSig    -- ^ A type signature.


-- | AST for a data definition, which consists of named type constructor and
--   a list of variants.
data DataDef
    = DataDef TyCon [Variant]

-- | AST for a type constructor, which consits of its name and a (possibly
--   empty) list of type variables.
data TyCon
    = TyCon TyName [TyVar]  -- TODO: abstract representation for type variables

-- | AST for a variant of a data type, which consits of a named constructor
--   and a (possibly empty) list of fields, which are specified by their type.
data Variant
    = DataCon Name [Type]


-- | AST for a value definition, which consists of a name and an expression.
data ValueDef
    = ValueDef Name Expr

-- | AST for an expression, which can be either a variable, lambda abstraction,
--   application of two expressions or a @let@ declaration.
data Expr
    = Var Name             -- ^ A single variable.
    | Lam Name Expr        -- ^ A lambda abstraction.
    | App Expr Expr        -- ^ An application of two expressions.
    | Let [ValueDef] Expr  -- ^ @let@ declaration. @let decls in expr@
                           --   is represented as @Let [[decls]] [[expr]]@


-- | AST for a type signature, which consists of the name of the entity and
--   a type.
data TypeSig
    = Sig Name Type

-- | AST for a type, which can either be a concrete type, type variable or
--   application of two types (with correct kind).
data Type
    = TyData TyName    -- ^ A concrete type.
    | TyVar TyVar      -- ^ A type variable.
    | TyApp Type Type  -- ^ Application of a type constructor.
