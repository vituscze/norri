{-# LANGUAGE FlexibleContexts #-}
-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Free
import Compiler.TypeChecking.Subst
import Compiler.TypeChecking.Unify

-- | A type inference monad is a combination of two state monads and one
--   error monad: first one to keep track of current substitution, the second
--   one for generation of unique variables and the last one for keeping track
--   of errors.
type TI a = StateT Subst (StateT Int (Either TCError)) a

-- | Runs a type inference with empty substitution and starting counter
--   for name generation.
runTI :: TI a -> Either TCError a
runTI m = fst . fst <$> runStateT (runStateT m empty) 0

-- | Gets the current substitution.
getSubst :: TI Subst
getSubst = get

-- | Sets the current substitution.
putSubst :: Subst -> TI ()
putSubst = put

-- | Gets the current name generation counter.
getCount :: TI Int
getCount = lift get

-- | Sets the current name generation counter.
putCount :: Int -> TI ()
putCount = lift . put

-- | Creates a fresh type variable.
newVar :: TI Type
newVar = do
    i <- getCount
    putCount (i + 1)
    return . TyVar $ "_t" ++ show i

-- | Replace 'TyGen' with freshly generated type variables.
--
--   Basically an application of substitution that only works on 'TyGen'
--   (instead of 'TyVar').
inst :: Map Int Type -> Type -> Type
inst m = go
  where
    go (TyData d)  = TyData d
    go (TyVar v)   = TyVar v
    go (TyGen i)   = fromMaybe (TyGen i) (Map.lookup i m)
    go (TyApp t u) = TyApp (go t) (go u)
    go (TyArr t u) = TyArr (go t) (go u)

-- | Instantiate all quantified variables in a type scheme to a fresh type
--   variables.
freshInst :: Scheme -> TI Type
freshInst (Scheme i t) = do
    new <- replicateM i newVar
    let m = Map.fromList $ zip [0 ..] new
    return (inst m t)

-- | Extend a global substitution with given one.
extend :: Subst -> TI ()
extend ns = do
    os <- getSubst
    putSubst (ns @@ os)

-- | Try to unify two types and add the resulting substitution to the
--   global one (via 'extend').
unifyE :: Type -> Type -> TI ()
unifyE t u = do
    s <- getSubst
    unify (apply s t) (apply s u) >>= extend

type Infer e t = TICtx -> e -> TI t

-- | Find a type scheme corresponding to a given variable.
--
--   If the name is not present in the context, the variable is unbound
--   and error of appropriate type is produced.
findCtx :: (MonadError TCError m) => TICtx -> Name -> m Scheme
findCtx (_, tc, _) n = case Map.lookup n tc of
    Just t -> return t
    _      -> throwError . SError $ UnboundVariable n

-- | Add a variable with a given type scheme into the context.
addCtx :: Name -> Scheme -> TICtx -> TICtx
addCtx n s (kc, tc, sc) = (kc, Map.insert n s tc, sc)

checkKind :: Infer Scheme ()
checkKind (kc, _, _) (Scheme _ t) = do
    i <- go t
    when (i /= 0) . throwError . KError $ KindMismatch
  where
    go (TyData n) = case Map.lookup n kc of
        Just i -> return i
        _      -> throwError . SError $ UndefinedType n
    go (TyGen _)  = return 0
    go (TyVar _)  = return 0
    go (TyApp t u) = do
        ti <- go t
        when (ti < 1) . throwError . KError $ KindMismatch
        ui <- go u
        when (ui /= 0) . throwError . KError $ KindMismatch
        return (ti - 1)
    go (TyArr t u) = do
        ti <- go t
        ui <- go u
        when (ti /= 0 || ui /= 0) . throwError . KError $ KindMismatch
        return 0

inferExpr :: Infer Expr Type
inferExpr ctx (Var v)   = findCtx ctx v >>= freshInst
inferExpr ctx (Lam x e) = do
    t  <- newVar
    te <- inferExpr (addCtx x (Scheme 0 t) ctx) e
    return (TyArr t te)
inferExpr ctx (App e1 e2) = do
    te1 <- inferExpr ctx e1
    te2 <- inferExpr ctx e2
    t   <- newVar
    unifyE (TyArr te2 t) te1
    return t
inferExpr ctx (Let [] e) = inferExpr ctx e
inferExpr ctx (Let (d:ds) e) = undefined -- infer as Let [d] (Let ds e)
inferExpr ctx (SetType e ts) = do  -- todo: refactor into separate function
    checkKind ctx ts
    t  <- freshInst ts
    te <- inferExpr ctx e
    unifyE t te
    s  <- getSubst
    let t'   = apply s t
        qvar = free t' \\ free (apply s ctx)
        nts  = quantify qvar t'
    if nts /= ts -- TODO: forall a b. a -> b   and forall b a. b -> a  should
                 -- be threated as being equal
        then throwError . TError $ TypeTooGeneral
        else return te
inferExpr ctx (NumLit _) = return (TyData "Int")
inferExpr ctx (Fix x e) = do
    t  <- newVar
    te <- inferExpr (addCtx x (Scheme 0 t) ctx) e
    unifyE t te
    return t
