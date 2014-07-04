-- | Module for basic AST transformations.
module Compiler.Transform
    (
    -- * Free variables
      free

    -- * Recursion removal
    , fixify
    , fixifyModule

    -- * Variables renaming
    , rename
    , fresh
    , freshModule
    )
    where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import Compiler.AST
import Utility

-- | Return set of all free variables in a given expression.
--
--   Local definitions inside @Let@ also bind variables.
--
-- >>> free $ Lam "x" $ App (Var "x") (Var "y")
-- Set.fromList ["y"]
free :: Expr -> Set Name
free (Var v)       = Set.singleton v
free (Lam x e)     = free e \\ Set.singleton x
free (App e1 e2)   = free e1 `Set.union` free e2
free (SetType e _) = free e
free (NumLit _)    = Set.empty
free (BoolLit _)   = Set.empty
free (Fix x e)     = free e \\ Set.singleton x
free (Let decls e) = (free e \\ names) `Set.union` vars
  where
    (names, vars) = foldr step (Set.empty, Set.empty) (reverse decls)

    step (ValueDef n e') (bound, freev) =
        (bound', freev `Set.union` (free e' \\ bound'))
      where
        bound' = Set.insert n bound

-- | Modifiy a recursive definition into a non-recursive one by
--   adding the fix point operator. If the definition and any possible
--   local definitions) are not recursive, no changes are made.
--
--   A definition @ValueDef n e@ is recursive, if @n@ is a free variable
--   inside the expression @e@.
fixify :: ValueDef -> ValueDef
fixify (ValueDef n ex)
  | n `Set.member` vars = ValueDef n (Fix n (fixify' ex))
  | otherwise           = ValueDef n (fixify' ex)
  where
    vars = free ex

    -- Make sure we also cover (possibly recursive) @Let@ definitions.
    fixify' (Lam x e)     = Lam x (fixify' e)
    fixify' (App e1 e2)   = App (fixify' e1) (fixify' e2)
    fixify' (SetType e t) = SetType (fixify' e) t
    fixify' (Let decls e) = Let (map fixify decls) e
    fixify' (Fix x e)     = Fix x (fixify' e)
    fixify' e             = e

-- | Remove direct recursion in the whole module by applying 'fixify'
--   to all 'ValueDef'initions.
fixifyModule :: Module -> Module
fixifyModule (Module tls) = Module (map go tls)
  where
    go (Value vd) = Value (fixify vd)
    go other      = other

-- | Internal monad transformer used for variable renaming. Maps variables of
--   type @v@ to variables of type @v'@.
type RenameM a = ReaderT (Map Name Name) (State Int) a

-- | Extract a new name @s'@ from state and runs @m@ with environment mapping
--   @s@ to @s'@.
--
--   For the scheme by which names are created, see 'nameVar'.
localInsert :: Name -> RenameM a -> RenameM (Name, a)
localInsert s m = do
    s' <- state $ \n -> (n, n + 1)
    let name = nameVar s'
    a  <- local (Map.insert s name) m
    return (name, a)

-- | Rename all bound variables in an expression so that no two distinct
--   variables share the same name. Free variables retain their original
--   name.
--
--   The variables are renamed according to the 'nameVar' function.
rename :: Int  -- ^ Starting number.
       -> Expr -- ^ Expression to rename.
       -> Expr
rename s ex = evalState (runReaderT (go ex) Map.empty) s
  where
    -- Apply the renaming inside expression.
    go (Var v)       = Var . fromMaybe v <$> asks (Map.lookup v)
    go (Lam x e)     = uncurry Lam <$> localInsert x (go e)
    go (App e1 e2)   = App <$> go e1 <*> go e2
    go (SetType e t) = flip SetType t <$> go e
    go (NumLit n)    = return $ NumLit n
    go (BoolLit b)   = return $ BoolLit b
    go (Fix x e)     = uncurry Fix <$> localInsert x (go e)
    go (Let decls e) = go' decls
      where
        go' []                 = Let [] <$> go e
        go' (ValueDef n e':ds) = do
            let renamed = (,) <$> go e' <*> go' ds
            (n', (e'', Let decls' outer)) <- localInsert n renamed
            return $ Let (ValueDef n' e'':decls') outer

-- | Replace all variables with fresh names to prevent any problems
--   in the generated C++ code.
fresh :: ValueDef -> ValueDef
fresh (ValueDef n e) = ValueDef n (rename 0 e)

-- | Replace all variables in a module with fresh names.
--
--   This is done by using 'fresh' on all 'ValueDef'initions. Note that two
--   different definitions may share same variables, but this is not an
--   issue in the generated C++ code.
freshModule :: Module -> Module
freshModule (Module tls) = Module (map go tls)
  where
    go (Value vd) = Value (fresh vd)
    go other      = other
