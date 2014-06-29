{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Implementation of type substitutions.
module Compiler.TypeChecking.Subst
    (
    -- * Substitution type
      Subst

    -- * Substitution modifications
    , empty
    , add
    , find
    , (@@)

    -- * Substitution application
    , Apply(..)
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)

import Compiler.AST
import Compiler.TypeChecking.Context

-- | A substitution is a map from variables to types.
type Subst = Map TyVar Type

-- | The empty substitution.
empty :: Subst
empty = Map.empty

-- | Add a new substitution from a given variable to a given type.
--
--   Alias for @insert@ing into a map.
add :: TyVar -> Type -> Subst -> Subst
add = Map.insert

-- | Attempt to find a variable @v@ in the substitution. If no substitution
--   can be found, returns just the variable @v@.
find :: TyVar -> Subst -> Type
find v = maybe (TyVar v) id . Map.lookup v

-- | Compose two substitutions into a single substitution.
--
--   Note that this operation is not symmetric.
(@@) :: Subst -> Subst -> Subst
s @@ t = Map.foldrWithKey (\u -> add u . apply s) s t

-- | Type class for substitution application.
class Apply t where
    apply :: Subst -> t -> t

-- | Substitution can be applied to 'Type's simply by looking up the
--   corresponding type for each 'TyVar'.
instance Apply Type where
    apply _ (TyData n)   = TyData n
    apply s (TyVar v)    = find v s
    apply _ (TyGen i)    = TyGen i
    apply s (TyApp t u)  = TyApp (apply s t) (apply s u)
    apply s (TyArr t u)  = TyArr (apply s t) (apply s u)

-- | Substitution can be applied to 'Scheme's by applying the substitution
--   to the inner 'Type'.
instance Apply Scheme where
    apply s (Scheme i t) = Scheme i (apply s t)

-- | Substitution can be applied to typing contexts ('TyCtx') by applying
--   the substitution to each 'Type' contained in the map.
instance Apply TyCtx where
    apply s m = Map.map (apply s) m

-- | Substitution can be applied to type inference context ('TICtx') by
--   applying the substitution only to the typing context ('TyCtx').
instance Apply TICtx where
    apply s (kc, tc, sc) = (kc, apply s tc, sc)
