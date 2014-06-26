{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Implementation of type substitutions.
module Compiler.TypeChecking.Subst
    where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Compiler.AST
import Compiler.TypeChecking.Context

-- | A substitution is a map from variables to types.
type Subst = Map TyVar Type

-- | The empty substitution.
empty :: Subst
empty = Map.empty

-- | Adds a new substitution from a given variable to a given type.
--
--   Alias for @insert@ing into a map.
add :: TyVar -> Type -> Subst -> Subst
add = Map.insert

-- | Attempts to find a variable @v@ in the substitution. If no substitution
--   can be found, returns just the variable @v@.
find :: TyVar -> Subst -> Type
find v = maybe (TyVar v) id . Map.lookup v

-- | Compose two substitutions into a single substitution.
(@@) :: Subst -> Subst -> Subst
s @@ t = Map.foldrWithKey (\u -> add u . apply s) s t

class Apply t where
    apply :: Subst -> t -> t

instance Apply Type where
    apply s (TyData n)   = TyData n
    apply s (TyVar v)    = find v s
    apply s (TyGen i)    = TyGen i
    apply s (TyApp t u)  = TyApp (apply s t) (apply s u)
    apply s (TyArr t u)  = TyArr (apply s t) (apply s u)

instance Apply Scheme where
    apply s (Scheme i t) = Scheme i (apply s t)

instance Apply TyCtx where
    apply s m = Map.map (apply s) m

instance Apply TICtx where
    apply s (kc, tc, sc) = (kc, apply s tc, sc)
