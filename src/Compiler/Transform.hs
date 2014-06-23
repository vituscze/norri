-- | Module for basic AST transformations.
module Compiler.Transform
    (
    -- * Free variables.
      free

    -- * Recursion removal.
    , fixify

    -- * Variables renaming.
    , rename
    , fresh
    )
    where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST

-- | Returns set of all free variables in a given expression.
--
--   Local definitions inside @Let@ also bind variables.
free :: Expr -> Set Name
free (Var v)       = Set.singleton v
free (Lam x e)     = free e \\ Set.singleton x
free (App e1 e2)   = free e1 `Set.union` free e2
free (SetType e _) = free e
free (NumLit _)    = Set.empty
free (Fix x e)     = free e \\ Set.singleton x
free (Let decls e) = (free e \\ names) `Set.union` vars
  where
    (names, vars) = foldr freeStep (Set.empty, Set.empty) (reverse decls)
    freeStep (ValueDef n e) (names, vars) =
        (names', vars `Set.union` (free e \\ names'))
      where
        names' = Set.insert n names

-- | Modifies a recursive definition into a non-recursive one by
--   adding the fix point operator. If the definition is not recursive,
--   no changes are made.
--
--   A definition @ValueDef n e@ is recursive, if @n@ is a free variable
--   inside the expression @e@.
--
--   Note that @fix@ should not be bound in a @Let@ or @Lam@.
fixify :: ValueDef -> ValueDef
fixify v@(ValueDef n e)
  | n `Set.member` vars = ValueDef n (Fix n (fixify' e))
  | otherwise           = v
  where
    vars = free e

    -- Make sure we also cover (possibly recursive) @Let@ definitions.
    fixify' (Lam x e)     = Lam x (fixify' e)
    fixify' (App e1 e2)   = App (fixify' e1) (fixify' e2)
    fixify' (SetType e t) = SetType (fixify' e) t
    fixify' (Let decls e) = Let (map fixify decls) e
    fixify' (Fix x e)     = Fix x (fixify' e)
    fixify' e             = e

-- | Internal monad transformer used for variable renaming. Maps variables of
--   type @v@ to variables of type @v'@.
type RenameM a = ReaderT (Map Name Name) (State Int) a

-- | Extracts a new name @s'@ from state and runs @m@ with environment mapping
--   @s@ to @s'@.
--
--   The crated name is @_Ts'@, where @s'@ is the extracted number.
localInsert :: Name -> RenameM a -> RenameM (Name, a)
localInsert s m = do
    s' <- state (\n -> (n, n + 1))
    let name = "_T" ++ show s'
    a  <- local (Map.insert s name) m
    return (name, a)

-- | Renames all bound variables in an expression so that no two distinct
--   variables share the same name.
--
--   The variables are renamed to @_Tn@, where @n@ are number starting with
--   the argument @s@.
rename :: Int  -- ^ Starting number.
       -> Expr -- ^ Expression to rename.
       -> Expr
rename s e = evalState (runReaderT (go e) Map.empty) s
  where
    -- Apply the renaming inside expression.
    go (Var v)       = liftM (Var . fromMaybe v) (asks (Map.lookup v))
    go (Lam x e)     = liftM (uncurry Lam) (localInsert x (go e))
    go (App e1 e2)   = liftM2 App (go e1) (go e2)
    go (SetType e t) = liftM (\e -> SetType e t) (go e)
    go (NumLit n)    = return (NumLit n)
    go (Fix x e)     = liftM (uncurry Fix) (localInsert x (go e))
    go (Let decls e) = go' decls
      where
        go' []                = liftM (Let []) (go e)
        go' (ValueDef n e:ds) = do
            let renamed = liftM2 (,) (go e) (go' ds)
            (n', (e', Let decls' outer)) <- localInsert n renamed
            return (Let (ValueDef n' e':decls') outer)

-- | Replaces all variables with fresh names to prevent any problems
--   in the generated C++ code.
--
--   Note that this function should be only called after the removal of
--   direct recursion. Free occurences of the name of the defined value
--   will be replaced.
fresh :: ValueDef -> ValueDef
fresh (ValueDef n e) = ValueDef n (rename 0 e)
