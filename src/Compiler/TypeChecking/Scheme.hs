-- | Type schemes.
module Compiler.TypeChecking.Scheme
    where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST
import Compiler.TypeChecking.Subst

freeT :: Type -> Set TyVar
freeT (TyData d)  = Set.empty
freeT (TyVar v)   = Set.singleton v
freeT (TyGen i)   = Set.empty
freeT (TyApp t u) = freeT t `Set.union` freeT u
freeT (TyArr t u) = freeT t `Set.union` freeT u

freeS :: Scheme -> Set TyVar
freeS (Scheme _ t) = freeT t

quantify :: Set TyVar -> Type -> Scheme
quantify vars t = Scheme (Set.size vars') (apply sub t)
  where
    vars' = freeT t `Set.intersection` vars
    sub   = Map.fromList $ zip (Set.toList vars') (map TyGen [0 ..])
