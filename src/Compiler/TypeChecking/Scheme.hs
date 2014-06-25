-- | Type schemes.
module Compiler.TypeChecking.Scheme
    (
    -- * Free type variables.
      freeT
    , freeS

    -- * Type variable quantification.
    , quantify
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST
import Compiler.TypeChecking.Subst

-- | Free type variables of a type.
freeT :: Type -> Set TyVar
freeT (TyData d)  = Set.empty
freeT (TyVar v)   = Set.singleton v
freeT (TyGen i)   = Set.empty
freeT (TyApp t u) = freeT t `Set.union` freeT u
freeT (TyArr t u) = freeT t `Set.union` freeT u

-- | Free variables of a type scheme.
--
--   Note that bound variables are represented by the constructor 'TyGen',
--   which means that we can simply apply 'freeT' to the inner type.
freeS :: Scheme -> Set TyVar
freeS (Scheme _ t) = freeT t

-- | Quantify given variables in a type.
--
--   These type variables are replaced with 'TyGen' constructors in the
--   resulting scheme.
quantify :: Set TyVar -> Type -> Scheme
quantify vars t = Scheme (Set.size vars') (apply sub t)
  where
    vars' = freeT t `Set.intersection` vars
    sub   = Map.fromList $ zip (Set.toList vars') (map TyGen [0 ..])
