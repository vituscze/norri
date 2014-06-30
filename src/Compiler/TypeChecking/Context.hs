-- | Contexts used during type inference - kind contexts, typing contexts,
--   type signature contexts.
module Compiler.TypeChecking.Context
    (
    -- * Kinds
      Kind
    , KindCtx

    -- * Types
    , TyCtx

    -- * Type signatures
    , SigCtx

    -- * Error context
    , ErrCtx

    -- * Type inference context
    , TICtx
    )
    where

import Data.Map (Map)

import Compiler.AST
import Compiler.TypeChecking.Error

-- | The type for kinds.
--
--   Since the only possible kinds are of the form @* -> * -> ... -> *@,
--   we can just store their arity.
type Kind = Int

-- | Kind context.
--
--   Used for kind checking of data definitions and user-declared types.
--   Internal types have correct kind by construction.
type KindCtx = Map TyName Kind

-- | Typing context.
type TyCtx = Map Name Scheme

-- | Declared type signatures.
type SigCtx = Map Name Scheme

-- | Error location.
type ErrCtx = Location

-- | All contexts needed for type inference.

-- TODO: separate data type
type TICtx = (KindCtx, TyCtx, SigCtx)
