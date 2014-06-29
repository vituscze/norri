{-# LANGUAGE FlexibleContexts #-}
-- | Type unification.
module Compiler.TypeChecking.Unify
    (
    -- * Unification
      unify
    )
    where

import Control.Monad.Except
import qualified Data.Set as Set

import Compiler.AST
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Free
import Compiler.TypeChecking.Subst

-- | Attempt to unify two types. If this operation is successful, the
--   resulting substitution is returned (in a monadic context).
--
--   If the unification is not successful, an error is thrown in the
--   surrounding error monad.
--
-- >>> unify (Var "a") (TyArr (Var "b") (Var "b"))
-- Right (Map.fromList [("a", TyArr (Var "b") (Var "b"))])
--
-- >>> unify (TyData "Int") (TyData "Char")
-- Left (UError $ TyConMismatch (TyData "Int") (TyData "Char"))
unify :: (MonadError TCError m) => Type -> Type -> m Subst
unify (TyData d) (TyData e)
    | d == e = return emptyS
unify (TyVar v) (TyVar w)
    | v == w = return emptyS
unify (TyVar v) u
    | Set.notMember v (free u) = return $ addS v u emptyS
    | otherwise                = throwError . UError $ OccursCheck v u
unify t (TyVar w) = unify (TyVar w) t
unify (TyApp t1 t2) (TyApp u1 u2) = unifyT t1 u1 t2 u2
unify (TyArr t1 t2) (TyArr u1 u2) = unifyT t1 u1 t2 u2
unify t u = throwError . UError $ TyConMismatch t u

-- | Unification of 'TyApp' and 'TyArr'.
unifyT :: (MonadError TCError m) => Type -> Type -> Type -> Type -> m Subst
unifyT t1 u1 t2 u2 = do
    s1 <- unify           t1            u1
    s2 <- unify (apply s1 t2) (apply s1 u2)
    return $ s2 @@ s1
