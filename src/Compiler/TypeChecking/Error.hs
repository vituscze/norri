module Compiler.TypeChecking.Error
    where

import Control.Monad.Error

import Compiler.AST

data UnificationError
    = OccursCheck TyVar Type
    | TyConMismatch Type Type
    deriving (Eq, Show)

data KindError
    = KindMismatch
    deriving (Eq, Show)

data ScopeError
    = UnboundVariable Name
    | UndefinedType Name
    deriving (Eq, Show)

data TCError
    = UError UnificationError
    | KError KindError
    | SError ScopeError
    | UnknownError String
    deriving (Eq, Show)

instance Error TCError where
    noMsg  = UnknownError ""
    strMsg = UnknownError
