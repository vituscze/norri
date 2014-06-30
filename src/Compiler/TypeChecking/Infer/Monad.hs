-- | Type inference monad.
module Compiler.TypeChecking.Infer.Monad
    (
    -- * Monad type
      TI

    -- * Running the monad
    , runTI

    -- * Extracting information
    , getSubst
    , putSubst
    , getCount
    , putCount
    , askEc
    , askCtx
    , askKc
    , askTc
    , askSc

    -- * Throwing errors
    , throwTCError

    -- * Local modifications
    , localE
    , localK
    , localT
    , localS
    , localCtx

    -- * Unique variables
    , newVar
    )
    where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Compiler.AST
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Subst
import Utility

-- | A type inference monad is a combination of two state monads and one
--   error monad: first one to keep track of current substitution, the second
--   one for generation of unique variables and the last one for keeping track
--   of errors.
type TI a
    = StateT Subst (StateT Int (ReaderT (ErrCtx, TICtx) (Either TCError))) a

-- | Run a type inference with empty substitution and starting counter
--   for name generation.
runTI :: TI a -> ErrCtx -> TICtx -> Either TCError a
runTI m ec tic = runReaderT (evalStateT (evalStateT m emptyS) 0) (ec, tic)

-- | Get the current substitution.
getSubst :: TI Subst
getSubst = get

-- | Set the current substitution.
putSubst :: Subst -> TI ()
putSubst = put

-- | Get the current name generation counter.
getCount :: TI Int
getCount = lift get

-- | Set the current name generation counter.
putCount :: Int -> TI ()
putCount = lift . put

askEc :: TI ErrCtx
askEc = do
    (ec, _) <- ask
    return ec

askCtx :: TI TICtx
askCtx = do
    (_, ctx) <- ask
    return ctx

askKc :: TI KindCtx
askKc = do
    TICtx kc _ _ <- askCtx
    return kc

askTc :: TI TyCtx
askTc = do
    TICtx _ tc _ <- askCtx
    return tc

askSc :: TI SigCtx
askSc = do
    TICtx _ _ sc <- askCtx
    return sc

throwTCError :: ErrorKind -> TI a
throwTCError e = do
    ec <- askEc
    throwError $ TCError e ec

-- | Locally apply a function @f@ to the error context.
localE :: (ErrCtx -> ErrCtx) -> TI a -> TI a
localE f = local go
  where
    go (ec, ctx) = (f ec, ctx)

-- | Locally apply a function @f@ to the kind context.
localK :: (KindCtx -> KindCtx) -> TI a -> TI a
localK f = local go
  where
    go (ec, ctx) = (ec, ctx { kindCtx = f (kindCtx ctx) })

-- | Locally apply a function @f@ to the typing context.
localT :: (TyCtx -> TyCtx) -> TI a -> TI a
localT f = local go
  where
    go (ec, ctx) = (ec, ctx { typeCtx = f (typeCtx ctx) })

-- | Locally apply a function @f@ to the type signature context.
localS :: (SigCtx -> SigCtx) -> TI a -> TI a
localS f = local go
  where
    go (ec, ctx) = (ec, ctx { sigCtx = f (sigCtx ctx) })

-- | Run the computation under a given type inference context.
localCtx :: TICtx -> TI a -> TI a
localCtx ctx = local go
  where
    go (ec, _) = (ec, ctx)

-- | Create a fresh type variable.
--
--   See also: 'nameTyVar'.
newVar :: TI Type
newVar = do
    i <- getCount
    putCount (i + 1)
    return . TyVar . nameTyVar $ i
