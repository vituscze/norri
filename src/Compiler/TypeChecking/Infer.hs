{-# LANGUAGE FlexibleContexts #-}
-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    (
    -- * Type inference monad
      module Compiler.TypeChecking.Infer.Monad
    , Infer

    -- * Fresh instantiation of type schemes
    , freshInst

    -- * Unification in the 'TI' monad
    , unifyE

    -- * Kind checking
    , checkKind

    -- * Type inference
    , inferExpr
    , inferValueDef
    , inferLetValueDef
    , inferVariant
    , inferElim
    , inferDataDef
    , inferTopLevel
    , inferModule
    )
    where

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Free
import Compiler.TypeChecking.Infer.Monad
import Compiler.TypeChecking.Subst
import Compiler.TypeChecking.Unify
import Utility


-- | Create a 'Scheme' for an unqualified 'Type'.
scheme :: Type -> Scheme
scheme = Scheme 0

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

-- | Create an instantiation of a type scheme by replacing all bound variables
--   with fresh ones.
freshInst :: Scheme -> TI Type
freshInst (Scheme i t) = do
    new <- replicateM i newVar
    let m = Map.fromList $ zip [0 ..] new
    return $ inst m t

-- | Try to unify two types and add the resulting substitution to the
--   global one.
unifyE :: Type -> Type -> TI ()
unifyE t u = do
    s  <- getSubst
    let t' = apply s t
        u' = apply s u
    s' <- case unify t' u' of
        Right s' -> return s'
        Left  e  -> throwTCError $ UError (FullError t' u' e)
    putSubst (s' @@ s)

-- | A type alias for all type inference operations.
type Infer e t = e -> TI t

-- | Find a type scheme corresponding to a given variable.
--
--   If the name is not present in the context, the variable is undefined
--   and an error is produced.
findCtx :: Infer Name Scheme
findCtx n = do
    tc <- askTc
    case Map.lookup n tc of
        Just t -> return t
        _      -> throwTCError $ SError (UnboundVariable n)

-- | Kind check a given scheme.
--
--   If the kind cannot be checked (for example because the type has not been
--   defined yet) or the scheme contains a kind error (such as @Int a@), an
--   error is produced.
checkKind :: Infer Scheme ()
checkKind (Scheme _ ts) = go 0 ts
  where
    -- Throw an error @KError (KindMismatch t k1 k2)@ only when @c@ holds.
    throw c t k1 k2 = when c . throwTCError $ KError (KindMismatch t k1 k2)

    -- 'go' is given an expected kind and the type that should be checked.
    go k t@(TyData n) = do
        kc <- askKc
        case Map.lookup n kc of
            Just i -> throw (k /= i) t i k
            _      -> throwTCError $ SError (UndefinedType n)

    -- All variables (quantified or not) are assumed to be of kind @*@.
    go k t@(TyGen _) = throw (k /= 0) t 0 k
    go k t@(TyVar _) = throw (k /= 0) t 0 k

    go k (TyApp t u) = do
        go (k + 1) t
        go 0 u
    go k ar@(TyArr t u) = do
        throw (k /= 0) ar 0 k
        go 0 t
        go 0 u

-- | Quantifiy all variables that are not among the free variables of
--   the typing context.
quantifyCtx :: Infer Type Scheme
quantifyCtx t = do
    s  <- getSubst
    tc <- askTc
    let t' = apply s t
        q  = free t' \\ free (apply s tc)
    return $ quantify q t'

-- | Check whether the inferred type matches the declared type (or that
--   the inferred type is more generic). If it doesn't, the declared type is
--   too general and an error is produced.
setType :: Type  -- ^ Inferred type.
        -> Infer Scheme ()
setType t ts = do
    tf  <- freshInst ts
    -- The would-be inferred type. This is only done so that we have
    -- better error messages.
    ts' <- quantifyCtx t
    unifyE tf t
    nts <- quantifyCtx tf
    when (nts /= ts) . throwTCError $ TError (TypeTooGeneral ts' ts)

-- | Infer the type of a given expression.
inferExpr :: Infer Expr Type
inferExpr (Var v) =
    findCtx v >>= freshInst
inferExpr (Lam x e) = do
    t  <- newVar
    te <- localT (Map.insert x (scheme t)) $ inferExpr e
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
    ctx' <- localE (InDef Local d:) $ inferLetValueDef d
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
    te <- localT (Map.insert x (scheme t)) $ inferExpr e
    localE (InExpr ex:) (unifyE t te)
    return t

-- | Infer the type of a given value definition.
--
--   If the inference succeeds, return the type inference context extended with
--   the new name and its type scheme.
inferValueDef :: Infer ValueDef TICtx
inferValueDef (ValueDef n e) = do
    te  <- inferExpr e
    sc  <- askSc
    tes <- case Map.lookup n sc of
        Just ts -> ts <$ setType te ts
        Nothing -> quantifyCtx te
    localT (Map.insert n tes) askCtx

-- | Infer the type of a given value definition inside a let expression.
--
--   If the inference succeeds, return the type inference context extended with
--   the new name and its type scheme.
--
--   Since let definitions cannot have type signatures attached to them,
--   type signatures can be freely ignored.
inferLetValueDef :: Infer ValueDef TICtx
inferLetValueDef (ValueDef n e) = do
    te  <- inferExpr e
    tes <- quantifyCtx te
    localT (Map.insert n tes) askCtx

-- | Infer the type of a single data constructor given the type constructor,
--   list of variables bound in the type constructor and the actual data
--   constructor.
--
--   If the inference succeeds, return the type inference context extended with
--   the constructor and its type scheme.
--
--   If a duplicate data constructor is found, an error is produced.
inferVariant :: Type       -- ^ Type constructor.
             -> Set TyVar  -- ^ Bound type variables.
             -> Infer Variant TICtx
inferVariant dt bound (DataCon n ts) = do
    let ty  = foldr1 TyArr (ts ++ [dt])
        tyq = quantify bound ty
        fr  = Set.toList (free tyq)

    checkKind (scheme ty)

    -- The type of data constructor should be self-contained, it should
    -- not contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwTCError $ SError (UndefinedType u)
    tc <- askTc
    when (n `Map.member` tc) . throwTCError $ SError (ValueRedefined n)
    localT (Map.insert n tyq) askCtx

-- | Infer the type of an eliminator for a data type given the type constructor,
--   list of variables bound in the type constructor, name of the type
--   constructor and the actual list of data constructors.
--
--   If the inference succeeds, return the type inference context extended with
--   the eliminator and its type scheme.
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

    checkKind (scheme ty)

    -- The type of the eliminator should be self-contained, it should not
    -- contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwTCError $ SError (UndefinedType u)

    -- Name of the eliminator.
    let n' = uncap n
    tc <- askTc
    when (n' `Map.member` tc) . throwTCError $ SError (ValueRedefined n')
    localT (Map.insert n' tyq) askCtx

-- | Kind check data type definition and add all constructors and the
--   eliminator into the type inference context.
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
    ctx1 <- localK (Map.insert n tvsc) askCtx
    -- Infer the types of all constructors.

    let step ctx v = localE (InVariant v:) . localCtx ctx $
            inferVariant dt tvs' v
    ctx2 <- foldM step ctx1 vs

    -- Infer the type of the eliminator.
    localE (InElim:) . localCtx ctx2 $ inferElim dt tvs' n vs

-- | Infer all relevant types of a single top level declaration/definition
--   and return the extended type inference context.
inferTopLevel :: Infer TopLevel TICtx
inferTopLevel (Data dd@(DataDef (TyCon n _) _)) = do
    -- Do not allow multiple definitions of one type.
    kc <- askKc
    when (n `Map.member` kc) . throwTCError $ SError (TypeRedefined n)
    localE (InDataDef dd:) $ inferDataDef dd
inferTopLevel (Value vd@(ValueDef n _)) = do
    -- Do not allow multiple definitions of one value.
    tc <- askTc
    when (n `Map.member` tc) . throwTCError $ SError (ValueRedefined n)
    localE (InDef TopLevel vd:) $ inferValueDef vd
inferTopLevel (Type t@(Sig n ts)) = do
    -- Do not allow multiple type signatures for one value. Also make sure
    -- that the code does not specify signature AFTER the actual definition.
    tc <- askTc
    sc <- askSc
    when (n `Map.member` tc) . throwTCError $ SError (TypeSigTooLate n)
    when (n `Map.member` sc) . throwTCError $ SError (TypeSigRedefined n)
    localE (InTypeSig t:) $ checkKind ts
    localS (Map.insert n ts) askCtx
inferTopLevel (Assume t@(Sig n ts)) = do
    -- Make sure that we are not trying to overwrite already defined value.
    -- Also type signatures make no sense with assumptions.
    tc <- askTc
    sc <- askSc
    when (n `Map.member` tc) . throwTCError $ SError (ValueRedefined n)
    when (n `Map.member` sc) . throwTCError $ SError (TypeSigRedefined n)
    localE (InAssume t:) $ checkKind ts
    localT (Map.insert n ts) askCtx

-- | Infer all revelant types in the module and return the final type inference
--   context.
inferModule :: Infer Module TICtx
inferModule (Module tls) = do
    ctx <- askCtx
    foldM step ctx tls
  where
    step ctx = localCtx ctx . inferTopLevel
