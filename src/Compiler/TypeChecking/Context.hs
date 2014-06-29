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

    -- * Type inference context
    , TICtx
    )
    where

import Data.Map (Map)

import Compiler.AST

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

-- | All contexts needed for type inference.
type TICtx = (KindCtx, TyCtx, SigCtx)
