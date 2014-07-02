-- | Errors that can happen during the type inference process.
--
--   Unless specified otherwise, actual types and/or kinds are mentioned
--   before expected types and/or kinds.
module Compiler.TypeChecking.Error
    (
    -- * Erorr location
      Location
    , LocationStep(..)
    , DefKind(..)

    -- * Unification errors
    , UnificationError(..)
    , FullUnificationError(..)

    -- * Kind errors
    , KindError(..)

    -- * Scope errors
    , ScopeError(..)

    -- * Type errors
    , TypeError(..)

    -- * All errors
    , ErrorKind(..)
    , TCError(..)
    )
    where

import Compiler.AST

-- | A full location of any given error is represented by list of steps.
--
--   If an error happens for example in an expression inside @Let@ definition
--   inside top level @ValueDef@inition, the location would be represented by
--   @[InExpr e, InLocalDef d, InDef d']@.
type Location = [LocationStep]

-- | A definition kind.
data DefKind
    = Local     -- ^ Local definition.
    | TopLevel  -- ^ Top level definition.
    deriving (Eq, Show)

-- | A single step of a full location.
data LocationStep
    = InExpr        Expr      -- ^ Inside an expression.
    | InDef DefKind ValueDef  -- ^ In a definition.
    | InTyCon       TyCon     -- ^ In a type constructor.
    | InVariant     Variant   -- ^ In a data construtor.
    | InElim                  -- ^ In an eliminator for a data type.
    | InDataDef     DataDef   -- ^ In a data type definition.
    | InTypeSig     TypeSig   -- ^ In a type signature.
    | InType        Type      -- ^ In a type.
    | InAssume      TypeSig   -- ^ In an assumption.
    deriving (Eq, Show)

-- | Errors that occur during unification.
data UnificationError
    = OccursCheck TyVar Type   -- ^ Occurs check failure.
    | TyConMismatch Type Type  -- ^ Mismatched type constructors.
    deriving (Eq, Show)

-- | A unification error with extra context - the two types that were unified.
data FullUnificationError
    = FullError Type Type UnificationError
    deriving (Eq, Show)

-- | Errors that occur during kind checking.
data KindError
    = KindMismatch Type Int Int  -- ^ Type and its expected and actual kind.
    deriving (Eq, Show)

-- | Scope errors - duplicate definition, unbound variables, etc.
data ScopeError
    = UnboundVariable Name   -- ^ Undefined variable.
    | UndefinedType TyName   -- ^ Undefined data type.
    | ValueRedefined Name    -- ^ Duplicate value definition.
    | TypeRedefined TyName   -- ^ Duplicate data type definition.
    | TypeSigRedefined Name  -- ^ Duplicate type declaration.
    | TypeSigTooLate Name    -- ^ Type signature found after definition.
    | VarsNotUnique          -- ^ Type variables in scope are not unique.
    deriving (Eq, Show)

-- | Type errors that occur when checking user declared types.
data TypeError
    = TypeTooGeneral Scheme Scheme  -- ^ Declared type is too general.
    deriving (Eq, Show)

-- | All kinds of errors that can happen during type inference.
data ErrorKind
    = UError FullUnificationError  -- ^ Unification errors.
    | KError KindError             -- ^ Kind checking errors.
    | SError ScopeError            -- ^ Scope errors.
    | TError TypeError             -- ^ Type errors.
    deriving (Eq, Show)

-- | Error with a location.
data TCError
    = TCError ErrorKind Location
    deriving (Eq, Show)
