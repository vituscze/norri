-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Compiler.AST
import Compiler.TypeChecking.Subst

-- | A type inference monad is a combination of two state monads: first one
--   to keep track of current substitution and the second one for generation
--   of unique variables.
type TI a = StateT Subst (State Int) a -- TODO: Add errors!

-- | Runs a type inference with empty substitution and starting counter
--   for name generation.
runTI :: TI a -> a
runTI m = fst . fst $ runState (runStateT m empty) 0

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
