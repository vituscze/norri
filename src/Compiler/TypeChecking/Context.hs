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
    , TICtx(..)
    , modifyK
    , modifyT
    , modifyS
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
data TICtx
    = TICtx
    { kindCtx :: KindCtx
    , typeCtx :: TyCtx
    , sigCtx  :: SigCtx
    }
    deriving (Eq, Show)

-- | Apply a function @f@ only to the kind context.
modifyK :: (KindCtx -> KindCtx) -> TICtx -> TICtx
modifyK f ctx = ctx { kindCtx = f (kindCtx ctx) }

-- | Apply a function @f@ only to the typing context.
modifyT :: (TyCtx -> TyCtx) -> TICtx -> TICtx
modifyT f ctx = ctx { typeCtx = f (typeCtx ctx) }

-- | Apply a function @f@ only to the type signature context.
modifyS :: (SigCtx -> SigCtx) -> TICtx -> TICtx
modifyS f ctx = ctx { sigCtx = f (sigCtx ctx) }
