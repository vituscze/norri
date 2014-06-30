{-# LANGUAGE FlexibleContexts #-}
-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    (
    -- * Type inference monad
      TI
    , Infer

    -- * Operations on type inference monad
    , runTI

    -- * Fresh variables
    , newVar

    -- * Fresh instantiations of type schemes
    , freshInst

    -- * Unification in TI monad context
    , unifyE

    -- * Kind checking
    , checkKind

    -- * Type inference
    , inferExpr
    , inferValueDef
    , inferVariant
    , inferElim
    , inferDataDef
    , inferTopLevel
    , inferModule
    )
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
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

getEc :: TI ErrCtx
getEc = do
    (ec, _) <- ask
    return ec

getCtx :: TI TICtx
getCtx = do
    (_, ctx) <- ask
    return ctx

getKc :: TI KindCtx
getKc = do
    TICtx kc _ _ <- getCtx
    return kc

getTc :: TI TyCtx
getTc = do
    TICtx _ tc _ <- getCtx
    return tc

getSc :: TI SigCtx
getSc = do
    TICtx _ _ sc <- getCtx
    return sc

throwTCError :: ErrorKind -> TI a
throwTCError e = do
    ec <- getEc
    throwError $ TCError e ec

-- | Apply a function @f@ only to the error context.
localE :: (ErrCtx -> ErrCtx) -> TI a -> TI a
localE f = local go
  where
    go (ec, ctx) = (f ec, ctx)

-- | Apply a function @f@ only to the kind context.
localK :: (KindCtx -> KindCtx) -> TI a -> TI a
localK f = local go
  where
    go (ec, ctx) = (ec, ctx { kindCtx = f (kindCtx ctx) })

-- | Apply a function @f@ only to the typing context.
localT :: (TyCtx -> TyCtx) -> TI a -> TI a
localT f = local go
  where
    go (ec, ctx) = (ec, ctx { typeCtx = f (typeCtx ctx) })

-- | Apply a function @f@ only to the type signature context.
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
newVar :: TI Type
newVar = do
    i <- getCount
    putCount (i + 1)
    return . TyVar . nameTyVar $ i

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
    return $ inst m t

-- | Extend a global substitution with given one.
extend :: Subst -> TI ()
extend ns = do
    os <- getSubst
    putSubst (ns @@ os)

-- | Try to unify two types and add the resulting substitution to the
--   global one.
unifyE :: Type -> Type -> TI ()
unifyE t u = do
    s  <- getSubst
    s' <- case unify (apply s t) (apply s u) of
        Right s' -> return s'
        Left  e  -> throwTCError $ UError (FullError t u e)
    extend s'

-- | A type alias for all type inference operations.
type Infer e t = e -> TI t

-- | Find a type scheme corresponding to a given variable.
--
--   If the name is not present in the context, the variable is unbound
--   and error of appropriate type is produced.
findCtx :: Infer Name Scheme
findCtx n = do
    tc <- getTc
    case Map.lookup n tc of
        Just t -> return t
        _      -> throwTCError $ SError (UnboundVariable n)

-- | Kind check a given scheme.
--
--   If the kind cannot be checked (for example because the type has not been
--   defined yet) or the scheme contains a kind error (such as @Int a@), an
--   error is produced.
checkKind :: Infer Scheme ()
checkKind (Scheme _ ts) = do
    i <- go ts
    when (i /= 0) . throwTCError $ KError (KindMismatch ts i 0)
  where
    go (TyData n) = do
        kc <- getKc
        case Map.lookup n kc of
            Just i -> return i
            _      -> throwTCError $ SError (UndefinedType n)

    -- All variables (quantified or not) are assumed to be of kind @*@.
    go (TyGen _)  = return 0
    go (TyVar _)  = return 0

    -- The kind of @t@ shouldn't be @*@ and the kind of @u@ should be
    -- @*@.
    go (TyApp t u) = do
        ti <- go t
        when (ti <  1) . throwTCError $ KError (KindMismatch t ti 1)
        ui <- go u
        when (ui /= 0) . throwTCError $ KError (KindMismatch u ui 0)
        return $ ti - 1
    go (TyArr t u) = do
        ti <- go t
        ui <- go u
        when (ti /= 0) . throwTCError $ KError (KindMismatch t ti 0)
        when (ui /= 0) . throwTCError $ KError (KindMismatch u ui 0)
        return 0

-- | Quantifiy all variables that are not free in the given typing context.
quantifyCtx :: Infer Type Scheme
quantifyCtx t = do
    s  <- getSubst
    tc <- getTc
    let t' = apply s t
        q  = free t' \\ free (apply s tc)
    return $ quantify q t'

-- | Check whether the inferred type matches the declared type (or that
--   the inferred type is more generic). If it doesn't, the declared type is
--   too general and an error is produced.
setType :: Type  -- ^ Inferred type.
        -> Infer Scheme ()
setType t ts = do
    tf <- freshInst ts
    unifyE tf t
    nts <- quantifyCtx tf
    when (nts /= ts) . throwTCError $ TError (TypeTooGeneral nts ts)

-- | Infer the type of a given expression.
inferExpr :: Infer Expr Type
inferExpr (Var v)   =
    findCtx v >>= freshInst
inferExpr (Lam x e) = do
    t  <- newVar
    te <- localT (Map.insert x (Scheme 0 t)) $ inferExpr e
    return $ TyArr t te
inferExpr e@(App e1 e2) = do
    te1 <- inferExpr e1
    te2 <- inferExpr e2
    t   <- newVar
    localE (InExpr e:) $ unifyE (TyArr te2 t) te1
    return t
inferExpr (Let [] e) =
    inferExpr e
inferExpr (Let (d:ds) e) = do
    ctx' <- localE (InLocalDef d:) $ inferValueDef d
    localCtx ctx' $ inferExpr (Let ds e)
inferExpr ex@(SetType e ts@(Scheme _ t)) = do
    localE (InType t:) $ checkKind ts
    te <- inferExpr e
    localE (InExpr ex:) $ setType te ts
    return te
inferExpr (NumLit _) =
    return $ TyData "Int"
inferExpr (BoolLit _) =
    return $ TyData "Bool"
inferExpr ex@(Fix x e) = do
    t  <- newVar
    te <- localT (Map.insert x (Scheme 0 t)) $ inferExpr e
    localE (InExpr ex:) (unifyE t te)
    return t

-- | Infer the type of a given value definition.
--
--   If the inference succeeds, corresponding pair of value and its type
--   scheme is added to the context which is then returned.
inferValueDef :: Infer ValueDef TICtx
inferValueDef (ValueDef n e) = do
    te  <- inferExpr e
    sc  <- getSc
    tes <- case Map.lookup n sc of
        Just ts -> ts <$ setType te ts
        Nothing -> quantifyCtx te
    localT (Map.insert n tes) getCtx

-- | Infer the type of a single data constructor given the type constructor,
--   list of variables bound in the type constructor and the actual data
--   constructor.
--
--   If the inference is successful, the type scheme of the constructor is
--   added to the context, which is then returned.
--
--   If a duplicate data constructor is found, an error is produced.
inferVariant :: Type       -- ^ Type constructor.
             -> Set TyVar  -- ^ Bound type variables.
             -> Infer Variant TICtx
inferVariant dt bound (DataCon n ts) = do
    let ty  = foldr1 TyArr (ts ++ [dt])
        tyq = quantify bound ty
        fr  = Set.toList (free tyq)

    checkKind (Scheme 0 ty)

    -- The type of data constructor should be self-contained, it should
    -- not contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwTCError $ SError (UndefinedType u)
    tc <- getTc
    when (n `Map.member` tc) . throwTCError $ SError (ValueRedefined n)
    localT (Map.insert n tyq) getCtx

-- | Infer the type of an eliminator for a data type given the type constructor,
--   list of variables bound in the type constructor, name of the type
--   constructor and the actual list of data constructors.
--
--   If the inference is successful, the type scheme of the eliminator is added
--   to the context, which is then returned.
--
--   Note that 'inferElim' automatically picks the name for the eliminator:
--   it is the uncapitalsed name of the data type. If a value with this name
--   already exists, an error is produced.
inferElim :: Type       -- ^ Type constructor.
          -> Set TyVar  -- ^ Bound type variables.
          -> TyName     -- ^ Name of the type constructor.
          -> Infer [Variant] TICtx
inferElim dt bound n vars = do
    z <- newVar
    let TyVar z' = z

        -- Type construction.
        tyV (DataCon _ ts) = foldr1 TyArr (ts ++ [z])
        tyVs vs            = foldr1 TyArr (map tyV vs ++ [dt, z])

        bound' = Set.insert z' bound
        ty     = tyVs vars
        tyq    = quantify bound' ty
        fr     = Set.toList (free tyq)

    checkKind (Scheme 0 ty)

    -- The type of the eliminator should be self-contained, it should not
    -- contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwTCError $ SError (UndefinedType u)

    -- Name of the eliminator.
    let n' = uncap n
    tc <- getTc
    when (n' `Map.member` tc) . throwTCError $ SError (ValueRedefined n')
    localT (Map.insert n' tyq) getCtx

-- | Kind check data type definition and add all constructors and the
--   eliminator into type inference context.
--
--   Also make sure that all bound type variables are distinct.
inferDataDef :: Infer DataDef TICtx
inferDataDef (DataDef tyc@(TyCon n tvs) vs) = do
    let tvs' = Set.fromList tvs
        tvsc = Set.size tvs'

    -- Declared type variables should be distinct.
    when (length tvs /= tvsc) . localE (InTyCon tyc:) . throwTCError $
        SError VarsNotUnique

    let dt = foldl1 TyApp (TyData n:map TyVar tvs)
    ctx1 <- localK (Map.insert n tvsc) getCtx
    -- Infer the types of all constructors.

    let step ctx v = localE (InVariant v:) . localCtx ctx $
            inferVariant dt tvs' v
    ctx2 <- foldM step ctx1 vs

    -- Infer the type of the eliminator.
    localE (InElim:) . localCtx ctx2 $ inferElim dt tvs' n vs

-- | Infer all relevant types of a single top level declaration/definition
--   and add those types to the type inference context.
inferTopLevel :: Infer TopLevel TICtx
inferTopLevel (Data dd@(DataDef (TyCon n _) _)) = do
    -- Do not allow multiple definitions of one type.
    kc <- getKc
    when (n `Map.member` kc) . throwTCError $ SError (TypeRedefined n)
    localE (InDataDef dd:) $ inferDataDef dd
inferTopLevel (Value vd@(ValueDef n _)) = do
    -- Do not allow multiple definitions of one value.
    tc <- getTc
    when (n `Map.member` tc) . throwTCError $ SError (ValueRedefined n)
    localE (InDef vd:) $ inferValueDef vd
inferTopLevel (Type t@(Sig n ts)) = do
    -- Do not allow multiple type signatures for one value. Also make sure
    -- that the code does not specify signature AFTER the actual definition.
    tc <- getTc
    sc <- getSc
    when (n `Map.member` tc) . throwTCError $ SError (TypeSigTooLate n)
    when (n `Map.member` sc) . throwTCError $ SError (TypeSigRedefined n)
    localE (InTypeSig t:) $ checkKind ts
    localS (Map.insert n ts) getCtx

-- | Infer all revelant types in the module. Returns final type inference
--   context.
inferModule :: Infer Module TICtx
inferModule (Module tls) = do
    ctx <- getCtx
    foldM step ctx tls
  where
    step ctx = localCtx ctx . inferTopLevel
