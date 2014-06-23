module Compiler.TypeChecking.Context
    (
    -- * Kinds.
      Kind
    , KindCtx

    -- * Types.
    , Scheme
    , TyCtx
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Compiler.AST

-- | The type for kinds.
--
--   Since the only possible kinds are of the form @* -> * -> ... -> *@,
--   we can just store their arity.
type Kind = Int

-- | Kind context.
type KindCtx = Map Name Kind

-- | Type schemes for algorithm W.
data Scheme
    = Scheme
        Type       -- ^ Actual type.
        (Set Name) -- ^ Quantified variables.
    deriving (Show)

-- | Typing context.
type TyCtx = Map Name Scheme
