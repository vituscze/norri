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
type TI a = StateT Subst (StateT Int (Either TCError)) a

-- | Run a type inference with empty substitution and starting counter
--   for name generation.
runTI :: TI a -> Either TCError a
runTI m = fst . fst <$> runStateT (runStateT m emptyS) 0

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
unifyE :: ErrCtx -> Type -> Type -> TI ()
unifyE ec t u = do
    s  <- getSubst
    s' <- case unify (apply s t) (apply s u) of
        Right s' -> return s'
        Left  e  -> throwError $ TCError (UError (FullError t u e)) ec
    extend s'

-- | A type alias for all type inference operations.
type Infer e t = ErrCtx -> TICtx -> e -> TI t

-- | Find a type scheme corresponding to a given variable.
--
--   If the name is not present in the context, the variable is unbound
--   and error of appropriate type is produced.
findCtx :: Infer Name Scheme
findCtx ec ctx n = case Map.lookup n (typeCtx ctx) of
    Just t -> return t
    _      -> throwError $ TCError (SError (UnboundVariable n)) ec

-- | Add a variable with a given type scheme into the context.
addCtx :: Name -> Scheme -> TICtx -> TICtx
addCtx n s = modifyT (Map.insert n s)

-- | Kind check a given scheme.
--
--   If the kind cannot be checked (for example because the type has not been
--   defined yet) or the scheme contains a kind error (such as @Int a@), an
--   error is produced.
checkKind :: Infer Scheme ()
checkKind ec ctx (Scheme _ ts) = do
    i <- go ts
    when (i /= 0) . throwError $
        TCError (KError (KindMismatch ts i 0)) ec
  where
    go (TyData n) = case Map.lookup n (kindCtx ctx) of
        Just i -> return i
        _      -> throwError $
            TCError (SError (UndefinedType n)) ec

    -- All variables (quantified or not) are assumed to be of kind @*@.
    go (TyGen _)  = return 0
    go (TyVar _)  = return 0

    -- The kind of @t@ shouldn't be @*@ and the kind of @u@ should be
    -- @*@.
    go (TyApp t u) = do
        ti <- go t
        when (ti <  1) . throwError $
            TCError (KError (KindMismatch t ti 1)) ec
        ui <- go u
        when (ui /= 0) . throwError $
            TCError (KError (KindMismatch u ui 0)) ec
        return $ ti - 1
    go (TyArr t u) = do
        ti <- go t
        ui <- go u
        when (ti /= 0) . throwError $
            TCError (KError (KindMismatch t ti 0)) ec
        when (ui /= 0) . throwError $
            TCError (KError (KindMismatch u ui 0)) ec
        return 0

-- | Quantifiy all variables that are not free in the given typing context.
quantifyCtx :: Infer Type Scheme
quantifyCtx _ ctx t = do
    s <- getSubst
    let t' = apply s t
        q  = free t' \\ free (apply s ctx)
    return $ quantify q t'

-- | Check whether the inferred type matches the declared type (or that
--   the inferred type is more generic). If it doesn't, the declared type is
--   too general and an error is produced.
setType :: Type  -- ^ Inferred type.
        -> Infer Scheme ()
setType t ec ctx ts = do
    tf <- freshInst ts
    unifyE ec tf t
    nts <- quantifyCtx ec ctx tf
    when (nts /= ts) . throwError $
        TCError (TError (TypeTooGeneral nts ts)) ec

-- | Infer the type of a given expression.
inferExpr :: Infer Expr Type
inferExpr ec ctx (Var v)   =
    findCtx ec ctx v >>= freshInst
inferExpr ec ctx (Lam x e) = do
    t  <- newVar
    te <- inferExpr ec (addCtx x (Scheme 0 t) ctx) e
    return $ TyArr t te
inferExpr ec ctx e@(App e1 e2) = do
    te1 <- inferExpr ec ctx e1
    te2 <- inferExpr ec ctx e2
    t   <- newVar
    unifyE (InExpr e:ec) (TyArr te2 t) te1
    return t
inferExpr ec ctx (Let [] e) =
    inferExpr ec ctx e
inferExpr ec ctx (Let (d:ds) e) = do
    ctx' <- inferValueDef (InLocalDef d:ec) ctx d
    inferExpr ec ctx' (Let ds e)
inferExpr ec ctx ex@(SetType e ts@(Scheme _ t)) = do
    checkKind (InType t:ec) ctx ts
    te <- inferExpr ec ctx e
    setType te (InExpr ex:ec) ctx ts
    return te
inferExpr _  _   (NumLit _) =
    return $ TyData "Int"
inferExpr _  _   (BoolLit _) =
    return $ TyData "Bool"
inferExpr ec ctx ex@(Fix x e) = do
    t  <- newVar
    te <- inferExpr ec (addCtx x (Scheme 0 t) ctx) e
    unifyE (InExpr ex:ec) t te
    return t

-- | Infer the type of a given value definition.
--
--   If the inference succeeds, corresponding pair of value and its type
--   scheme is added to the context which is then returned.
inferValueDef :: Infer ValueDef TICtx
inferValueDef ec ctx (ValueDef n e) = do
    te  <- inferExpr ec ctx e
    tes <- case Map.lookup n (sigCtx ctx) of
        Just ts -> ts <$ setType te ec ctx ts
        Nothing -> quantifyCtx ec ctx te
    return $ addCtx n tes ctx

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
inferVariant dt bound ec ctx (DataCon n ts) = do
    let ty  = foldr1 TyArr (ts ++ [dt])
        tyq = quantify bound ty
        fr  = Set.toList (free tyq)

    checkKind ec ctx (Scheme 0 ty)

    -- The type of data constructor should be self-contained, it should
    -- not contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwError $ TCError (SError (UndefinedType u)) ec
    when (n `Map.member` typeCtx ctx) . throwError $
        TCError (SError (ValueRedefined n)) ec
    return $ addCtx n tyq ctx

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
inferElim dt bound n ec ctx vars = do
    z <- newVar
    let TyVar z' = z

        -- Type construction.
        tyV (DataCon _ ts) = foldr1 TyArr (ts ++ [z])
        tyVs vs            = foldr1 TyArr (map tyV vs ++ [dt, z])

        bound' = Set.insert z' bound
        ty     = tyVs vars
        tyq    = quantify bound' ty
        fr     = Set.toList (free tyq)

    checkKind ec ctx (Scheme 0 ty)

    -- The type of the eliminator should be self-contained, it should not
    -- contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwError $ TCError (SError (UndefinedType u)) ec

    -- Name of the eliminator.
    let n' = uncap n
    when (n' `Map.member` typeCtx ctx) . throwError $
        TCError (SError (ValueRedefined n')) ec
    return $ addCtx n' tyq ctx

-- | Kind check data type definition and add all constructors and the
--   eliminator into type inference context.
--
--   Also make sure that all bound type variables are distinct.
inferDataDef :: Infer DataDef TICtx
inferDataDef ec ctx (DataDef tyc@(TyCon n tvs) vs) = do
    let tvs' = Set.fromList tvs
        tvsc = Set.size tvs'

    -- Declared type variables should be distinct.
    when (length tvs /= tvsc) . throwError $
        TCError (SError VarsNotUnique) (InTyCon tyc:ec)

    let dt   = foldl1 TyApp (TyData n:map TyVar tvs)
        ctx1 = modifyK (Map.insert n tvsc) ctx

    -- Infer the types of all constructors.
    ctx2 <-foldM (\c v -> inferVariant dt tvs' (InVariant v:ec) c v) ctx1 vs

    -- Infer the type of the eliminator.
    inferElim dt tvs' n (InElim:ec) ctx2 vs

-- | Infer all relevant types of a single top level declaration/definition
--   and add those types to the type inference context.
inferTopLevel :: Infer TopLevel TICtx
inferTopLevel ec ctx (Data dd@(DataDef (TyCon n _) _)) = do
    -- Do not allow multiple definitions of one type.
    when (n `Map.member` kindCtx ctx) . throwError $
        TCError (SError (TypeRedefined n)) ec
    inferDataDef (InDataDef dd:ec) ctx dd
inferTopLevel ec ctx (Value vd@(ValueDef n _)) = do
    -- Do not allow multiple definitions of one value.
    when (n `Map.member` typeCtx ctx) . throwError $
        TCError (SError (ValueRedefined n)) ec
    inferValueDef (InDef vd:ec) ctx vd
inferTopLevel ec ctx (Type t@(Sig n ts)) = do
    -- Do not allow multiple type signatures for one value. Also make sure
    -- that the code does not specify signature AFTER the actual definition.
    when (n `Map.member` typeCtx ctx) . throwError $
        TCError (SError (TypeSigTooLate n)) ec
    when (n `Map.member` sigCtx ctx) . throwError $
        TCError (SError (TypeSigRedefined n)) ec
    checkKind (InTypeSig t:ec) ctx ts
    return $ modifyS (Map.insert n ts) ctx

-- | Infer all revelant types in the module. Returns final type inference
--   context.
inferModule :: Infer Module TICtx
inferModule ec ctx (Module tls) = foldM (inferTopLevel ec) ctx tls
