{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Implementation of type substitutions.
module Compiler.TypeChecking.Subst
    (
    -- * Substitution
      Subst

    -- * Substitution modifications
    , emptyS
    , addS
    , findS
    , (@@)

    -- * Substitution application
    , Apply(..)
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Compiler.AST
import Compiler.TypeChecking.Context

-- | A substitution is a map from variables to types.
type Subst = Map TyVar Type

-- | The empty substitution.
emptyS :: Subst
emptyS = Map.empty

-- | Add a new substitution from a given variable to a given type.
--
--   Alias for @insert@ing into a map.
addS :: TyVar -> Type -> Subst -> Subst
addS = Map.insert

-- | Attempt to find a variable @v@ in the substitution. If no substitution
--   can be found, returns just the variable @v@.
findS :: TyVar -> Subst -> Type
findS v = fromMaybe (TyVar v) . Map.lookup v

-- | Compose two substitutions into a single substitution.
--
--   Note that this operation is not symmetric.
(@@) :: Subst -> Subst -> Subst
s @@ t = Map.foldrWithKey (\u -> addS u . apply s) s t

-- | Type class for substitution application.
class Apply t where
    apply :: Subst -> t -> t

-- | Substitution can be applied to 'Type's simply by looking up the
--   corresponding type for each 'TyVar'.
instance Apply Type where
    apply _ (TyData n)   = TyData n
    apply s (TyVar v)    = findS v s
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
    apply = Map.map . apply
