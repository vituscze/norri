{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Free variables and quantification.
module Compiler.TypeChecking.Free
    where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Compiler.AST
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Subst

class Free t where
    free :: t -> Set TyVar

instance Free Type where
    free (TyData d)  = Set.empty
    free (TyVar v)   = Set.singleton v
    free (TyGen i)   = Set.empty
    free (TyApp t u) = free t `Set.union` free u
    free (TyArr t u) = free t `Set.union` free u

instance Free Scheme where
    free (Scheme _ t) = free t

instance Free TyCtx where
    free = Map.foldr (Set.union . free) Set.empty

instance Free TICtx where
    free (_, tc, _) = free tc

-- | Quantify given variables in a type.
--
--   These type variables are replaced with 'TyGen' constructors in the
--   resulting scheme.
quantify :: Set TyVar -> Type -> Scheme
quantify vars t = Scheme (Set.size vars') (apply sub t)
  where
    vars' = free t `Set.intersection` vars
    sub   = Map.fromList $ zip (Set.toList vars') (map TyGen [0 ..])
