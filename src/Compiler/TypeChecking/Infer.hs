{-# LANGUAGE FlexibleContexts #-}
-- | Type inference monad and type inference operations.
module Compiler.TypeChecking.Infer
    (
    -- * Type inference monad.
      TI

    -- * Operations on type inference monad.
    , runTI

    -- * Fresh variables.
    , newVar

    -- * Fresh instantiations of type schemes.
    , freshInst

    -- * Unification in TI monad context.
    , unifyE

    -- * Kind checking.
    , checkKind

    -- * Type inference.
    , inferExpr
    , inferValueDef
    , inferVariant
    , inferElim
    , inferDataDef
    , inferTopLevel
    , inferModule
    )
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

-- | Run a type inference with empty substitution and starting counter
--   for name generation.
runTI :: TI a -> Either TCError a
runTI m = fst . fst <$> runStateT (runStateT m empty) 0

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

-- | Type for all operations doing type inference.
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

-- | Kind check a given scheme.
--
--   If the kind cannot be checked (for example because the type has not been
--   defined yet) or the scheme contains a kind error (such as @Int a@), an
--   error is produced.
checkKind :: Infer Scheme ()
checkKind (kc, _, _) (Scheme _ t) = do
    i <- go t
    when (i /= 0) . throwError . KError $ KindMismatch
  where
    go (TyData n) = case Map.lookup n kc of
        Just i -> return i
        _      -> throwError . SError $ UndefinedType n

    -- All variables (quantified or not) are assumed to be of kind @*@.
    go (TyGen _)  = return 0
    go (TyVar _)  = return 0

    -- The kind of @t@ shouldn't be @*@ and the kind of @u@ should be
    -- @*@.
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

-- | Quantifiy all variables that are not free in the given typing context.
quantifyCtx :: Infer Type Scheme
quantifyCtx ctx t = do
    s <- getSubst
    let t'   = apply s t
        qvar = free t' \\ free (apply s ctx)
    return (quantify qvar t')

-- | Check whether the inferred type matches the declared type (or that
--   the inferred type is more generic). If it doesn't, the declared type is
--   too general and an error is produced.
setType :: Type -> Infer Scheme ()
setType t ctx ts = do
    tf <- freshInst ts
    unifyE tf t
    nts <- quantifyCtx ctx tf
    when (nts /= ts) . throwError . TError $ TypeTooGeneral

-- | Infer the type of a given expression.
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

-- | Infer the type of a given value definition.
--
--   If the inference succeeds, corresponding pair of value and its type
--   scheme is added to the context which is then returned.
inferValueDef :: Infer ValueDef TICtx
inferValueDef ctx@(_, _, sc) (ValueDef n e) = do
    te  <- inferExpr ctx e
    tes <- case Map.lookup n sc of
        Just ts -> setType te ctx ts >> return ts
        Nothing -> quantifyCtx ctx te
    return $ addCtx n tes ctx

-- | Infer the type of a single data constructor given the type constructor,
--   list of variables bound in the type constructor and the actual data
--   constructor.
--
--   If the inference is successful, the type scheme of the constructor is
--   added to the context, which is then returned.
--
--   If a duplicate data constructor is found, an error is produced.
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
inferElim :: Type -> Set TyVar -> TyName -> Infer [Variant] TICtx
inferElim dt bound n ctx@(_, tc, _) vars = do
    z <- newVar
    let TyVar z'  = z
        tyV (DataCon _ ts) = foldr1 TyArr (ts ++ [z])
        tyVs vars = foldr1 TyArr (map tyV vars ++ [dt, z])
        bound'    = Set.insert z' bound
        ty        = quantify bound' (tyVs vars)
        fr        = Set.toList (free ty)
    -- The type of the eliminator should be self-contained, it should not
    -- contain any free type variables.
    case fr of
        []  -> return ()
        u:_ -> throwError . SError $ UndefinedType u

    -- Name of the eliminator.
    let fToL (x:xs) = toLower x:xs
        n'          = fToL n
    when (n' `Map.member` tc) . throwError . SError $ ValueRedefined n
    return (addCtx n' ty ctx)

-- | Kind check data type definition and add all constructors and the
--   eliminator into type inference context.
--
--   Also make sure that all bound variables are distinct.
inferDataDef :: Infer DataDef TICtx
inferDataDef ctx0@(kc, tc, sc) (DataDef (TyCon n tvs) vs) = do
    let tvs' = Set.fromList tvs
        tvsc = Set.size tvs'
    -- Declared type variables should be distinct.
    when (length tvs /= tvsc) . throwError . SError $ TypeRedefined ""
    -- TODO: Find the offending type variable

    let dt   = foldl1 TyApp (TyData n:map TyVar tvs)
        ctx1 = (Map.insert n tvsc kc, tc, sc)
    -- Infer the type of the eliminator.
    ctx2 <- inferElim dt tvs' n ctx1 vs

    -- Infer the types of all constructors.
    foldM (inferVariant dt tvs') ctx2 vs

-- | Infer all relevant types of a single top level declaration/definition
--   and add those types to the type inference context.
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

-- | Infer all revelant types in the module. Returns final type inference
--   context.
inferModule :: Infer Module TICtx
inferModule ctx (Module tls) = foldM inferTopLevel ctx tls
