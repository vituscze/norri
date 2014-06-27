-- | Errors that can happen during the type inference process.
module Compiler.TypeChecking.Error
    (
    -- * Unification errors.
      UnificationError(..)

    -- * Kind errors.
    , KindError(..)

    -- * Scope errors.
    , ScopeError(..)

    -- * Type errors.
    , TypeError(..)

    -- * All errors.
    , TCError(..)
    )
    where

import Control.Monad.Error

import Compiler.AST

-- | Errors that occur during unification.
data UnificationError
    = OccursCheck TyVar Type   -- ^ Occurs check failure.
    | TyConMismatch Type Type  -- ^ Mismatched type constructors.
    deriving (Eq, Show)

-- | Errors that occur during kind checking.
data KindError
    = KindMismatch
    deriving (Eq, Show)

-- | Scope errors - duplicate definition, unbound variables, etc.
data ScopeError
    = UnboundVariable Name   -- ^ Undefined variable.
    | UndefinedType TyName   -- ^ Undefined data type.
    | ValueRedefined Name    -- ^ Duplicate value definition.
    | TypeRedefined TyName   -- ^ Duplicate data type definition.
    | TypeSigRedefined Name  -- ^ Duplicate type declaration.
    deriving (Eq, Show)

-- | Type errors that occur when checking user declared types.
data TypeError
    = TypeTooGeneral  -- ^ Declared type is too general.
    deriving (Eq, Show)

-- | All kinds of errors that can happen during type inference.
data TCError
    = UError UnificationError  -- ^ Unification errors.
    | KError KindError         -- ^ Kind checking errors.
    | SError ScopeError        -- ^ Scope errors.
    | TError TypeError         -- ^ Type errors.
    | UnknownError String      -- ^ Other errors.
    deriving (Eq, Show)

instance Error TCError where
    noMsg  = UnknownError ""
    strMsg = UnknownError
