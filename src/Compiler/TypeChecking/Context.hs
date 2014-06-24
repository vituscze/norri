module Compiler.TypeChecking.Context
    (
    -- * Kinds.
      Kind
    , KindCtx

    -- * Types.
    , Scheme(..)
    , TyCtx
    )
    where

import qualified Data.Map as Map
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
type KindCtx = Map Name Kind

-- | Typing context.
type TyCtx = Map Name Scheme
