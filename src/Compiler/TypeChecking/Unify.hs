{-# LANGUAGE FlexibleContexts #-}
module Compiler.TypeChecking.Unify
    where

import Control.Monad
import Control.Monad.Error
import qualified Data.Set as Set
import Data.Set (Set)

import Compiler.AST
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Subst

free :: Type -> Set TyVar
free (TyData d)    = Set.empty
free (TyVar v)     = Set.singleton v
free (TyApp t u)   = free t `Set.union` free u
free (TyArr t u)   = free t `Set.union` free u

unify :: (MonadError TCError m) => Type -> Type -> m Subst
unify (TyData d) (TyData e)
    | d == e = return empty
unify (TyVar v) (TyVar w)
    | v == w = return empty
unify (TyVar v) u
    | Set.notMember v (free u) = return (add v u empty)
    | otherwise                = throwError . UError $ OccursCheck v u
unify t (TyVar w) = unify (TyVar w) t
unify (TyApp t1 t2) (TyApp u1 u2) = do
    s1 <- unify           t1            u1
    s2 <- unify (apply s1 t2) (apply s1 u2)
    return (s2 @@ s1)
unify (TyArr t1 t2) (TyArr u1 u2) = do
    s1 <- unify           t1            u1
    s2 <- unify (apply s1 t2) (apply s1 u2)
    return (s2 @@ s1)
unify t u = throwError . UError $ TyConMismatch t u
