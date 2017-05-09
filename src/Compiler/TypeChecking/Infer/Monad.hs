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

-- | A type inference monad is a combination of two state monads, one reader
--   monad and one error monad: the first one to keep track of the current
--   substitution, the second one for generation of unique variables, the third
--   one for typing and error contexts and the last one for errors.
type TI a
    = StateT Subst (StateT Int (ReaderT (ErrCtx, TICtx) (Either TCError))) a

-- | Run a type inference computation with an empty substitution and a default
--   fresh variable counter.
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

-- | Get the current error location context.
askEc :: TI ErrCtx
askEc = do
    (ec, _) <- ask
    return ec

-- | Get the current type inference context.
askCtx :: TI TICtx
askCtx = do
    (_, ctx) <- ask
    return ctx

-- | Get the current kind context.
askKc :: TI KindCtx
askKc = do
    TICtx kc _ _ <- askCtx
    return kc

-- | Get the current typing context.
askTc :: TI TyCtx
askTc = do
    TICtx _ tc _ <- askCtx
    return tc

-- | Get the current type signature context.
askSc :: TI SigCtx
askSc = do
    TICtx _ _ sc <- askCtx
    return sc

-- | Throw an 'ErrorKind' as 'TCError'.
--
--   Error location is automatically filled in.
throwTCError :: ErrorKind -> TI a
throwTCError e = do
    ec <- askEc
    throwError $ TCError e ec

-- | Run a computation in a modified error context 'ErrCtx'.
localE :: (ErrCtx -> ErrCtx) -> TI a -> TI a
localE f = local go
  where
    go (ec, ctx) = (f ec, ctx)

-- | Run a computation in a modified kind context 'KindCtx'.
localK :: (KindCtx -> KindCtx) -> TI a -> TI a
localK f = local go
  where
    go (ec, ctx) = (ec, ctx { kindCtx = f (kindCtx ctx) })

-- | Run a computation in a modified typing context 'TyCtx'.
localT :: (TyCtx -> TyCtx) -> TI a -> TI a
localT f = local go
  where
    go (ec, ctx) = (ec, ctx { typeCtx = f (typeCtx ctx) })

-- | Run a computation in a modified type signature context 'SigCtx'.
localS :: (SigCtx -> SigCtx) -> TI a -> TI a
localS f = local go
  where
    go (ec, ctx) = (ec, ctx { sigCtx = f (sigCtx ctx) })

-- | Run a computation in a modified type inference context 'TICtx'.
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
