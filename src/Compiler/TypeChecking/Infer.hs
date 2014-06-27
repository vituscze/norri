{-# LANGUAGE FlexibleContexts #-}
-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Char
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

quantifyCtx :: Infer Type Scheme
quantifyCtx ctx t = do
    s <- getSubst
    let t'   = apply s t
        qvar = free t' \\ free (apply s ctx)
    return (quantify qvar t')

setType :: Type -> Infer Scheme ()
setType t ctx ts = do
    tf <- freshInst ts
    unifyE tf t
    nts <- quantifyCtx ctx tf
    when (nts /= ts) . throwError . TError $ TypeTooGeneral

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
inferExpr ctx (Let (d:ds) e) = do
    ctx' <- inferValueDef ctx d
    inferExpr ctx' (Let ds e)
inferExpr ctx (SetType e ts) = do
    checkKind ctx ts
    te <- inferExpr ctx e
    setType te ctx ts
    return te
inferExpr ctx (NumLit _) = return (TyData "Int")
inferExpr ctx (Fix x e) = do
    t  <- newVar
    te <- inferExpr (addCtx x (Scheme 0 t) ctx) e
    unifyE t te
    return t

inferValueDef :: Infer ValueDef TICtx
inferValueDef ctx@(_, _, sc) (ValueDef n e) = do
    te  <- inferExpr ctx e
    tes <- case Map.lookup n sc of
        Just ts -> setType te ctx ts >> return ts
        Nothing -> quantifyCtx ctx te
    return $ addCtx n tes ctx

inferVariant :: Type -> Set TyVar -> Infer Variant TICtx
inferVariant dt bound ctx@(_, tc, _) (DataCon n ts) = do
    let ty = quantify bound (foldr1 TyArr (ts ++ [dt]))
        fr = Set.toList (free ty)
    -- The type of data constructor should be self-contained, it should
    -- not contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwError . SError $ UndefinedType u
    when (n `Map.member` tc) . throwError . SError $ ValueRedefined n
    return (addCtx n ty ctx)

inferElim :: Type -> Set TyVar -> TyName -> Infer [Variant] TICtx
inferElim dt bound n ctx@(_, tc, _) vars = do
    z <- newVar
    let TyVar z'  = z
        tyV (DataCon _ ts) = foldr1 TyArr (ts ++ [z])
        tyVs vars = foldr1 TyArr (map tyV vars ++ [dt, z])
        bound'    = Set.insert z' bound
        ty        = quantify bound' (tyVs vars)
        fr        = Set.toList (free ty)
    case fr of
        []  -> return ()
        u:_ -> throwError . SError $ UndefinedType u
    let fToL (x:xs) = toLower x:xs
        n'          = fToL n
    when (n' `Map.member` tc) . throwError . SError $ ValueRedefined n
    return (addCtx n' ty ctx)

-- | Kind check data type definition and add all constructors and the
--   eliminator into type inference context.
inferDataDef :: Infer DataDef TICtx
inferDataDef ctx0@(kc, tc, sc) (DataDef (TyCon n tvs) vs) = do
    let tvs' = Set.fromList tvs
        tvsc = Set.size tvs'
    -- Declared type variables should be distinct.
    when (length tvs /= tvsc) . throwError . SError $ TypeRedefined ""
    -- TODO: Find the offending type variable

    let dt   = foldl1 TyApp (TyData n:map TyVar tvs)
        ctx1 = (Map.insert n tvsc kc, tc, sc)
    -- Create type for constructors.
    ctx2 <- inferElim dt tvs' n ctx1 vs
    foldM (inferVariant dt tvs') ctx2 vs


inferTopLevel :: Infer TopLevel TICtx
inferTopLevel ctx@(kc, _, _) (Data dd@(DataDef (TyCon n _) _)) = do
    -- Do not allow multiple definitions of one type.
    when (n `Map.member` kc) . throwError . SError $ TypeRedefined n
    inferDataDef ctx dd
inferTopLevel ctx@(_, tc, _) (Value vd@(ValueDef n _)) = do
    -- Do not allow multiple definitions of one value.
    when (n `Map.member` tc) . throwError . SError $ ValueRedefined n
    inferValueDef ctx vd
inferTopLevel ctx@(kc, tc,sc) (Type (Sig n ts)) = do
    -- Do not allow multiple type signatures for one value. Also make sure
    -- that the code does not specify signature AFTER the actual definition.
    -- TODO: Perhaps use separate errors?
    when (n `Map.member` sc || n `Map.member` tc) . throwError . SError $
        TypeSigRedefined n
    checkKind ctx ts
    return (kc, tc, Map.insert n ts sc)

inferModule :: Infer Module TICtx
inferModule ctx (Module tls) = foldM inferTopLevel ctx tls
