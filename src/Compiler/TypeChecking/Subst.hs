-- | Implementation of type substitutions.
module Compiler.TypeChecking.Subst
    where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Compiler.AST

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

-- | Applies given substitution to a type.
apply :: Subst -> Type -> Type
apply s (TyData n)   = TyData n
apply s (TyVar v)    = find v s
apply s (TyGen i)    = TyGen i
apply s (TyApp t u)  = TyApp (apply s t) (apply s u)
apply s (TyArr t u)  = TyArr (apply s t) (apply s u)
