{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Free variables and quantification.
module Compiler.TypeChecking.Free
    (
    -- * Free type variables
      Free(..)

    -- * Quantification
    , quantify
    )
    where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Compiler.AST
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Subst

-- | Type class for determining free variables.
class Free t where
    free :: t -> Set TyVar

-- | Free variables of a type are just the set of all variables.
instance Free Type where
    free (TyData _)  = Set.empty
    free (TyVar v)   = Set.singleton v
    free (TyGen _)   = Set.empty
    free (TyApp t u) = free t `Set.union` free u
    free (TyArr t u) = free t `Set.union` free u

-- | Free variables of a quantified type are again just the set of all
--   since quantified variables are represented by 'TyGen' constructor.
instance Free Scheme where
    free (Scheme _ t) = free t

-- | Free variables of a typing context are union of free variables of
--   all types contained within.
instance Free TyCtx where
    free = Map.foldr (Set.union . free) Set.empty

-- | Free variables of a type inference context are just the free variables
--   of its typing context.
instance Free TICtx where
    free (_, tc, _) = free tc

-- | Quantify given variables in a type.
--
--   These type variables are replaced with 'TyGen' constructors in the
--   resulting scheme.
--
-- >>> quantify (Set.fromList ["a"]) (TyArr (TyVar "a") (TyVar "b"))
-- Scheme 1 (TyArr (TyGen 0) (TyVar "b"))
quantify :: Set TyVar -> Type -> Scheme
quantify vars t = Scheme (Set.size vars') (apply sub t)
  where
    vars' = free t `Set.intersection` vars
    sub   = Map.fromList $ zip (Set.toList vars') (map TyGen [0 ..])
