-- | Default values for file names, contexts, etc.
module Default
    (
    -- * Default contexts
      defaultErrCtx
    , defaultTICtx

    -- * Module inference
    , runDefault

    -- * Default file paths
    , defaultOutput
    , defaultInclude
    )
    where

import qualified Data.Map as Map
import Text.Parsec

import Compiler.AST
import Compiler.Parser
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Error
import Compiler.TypeChecking.Infer

-- | Default error context.
defaultErrCtx :: ErrCtx
defaultErrCtx = []

-- | Default type inference context.
defaultTICtx :: TICtx
defaultTICtx = TICtx
    -- Type constructors.
    (Map.fromList
        [ ("Bool", 0)
        , ("Int",  0)
        , ("Type", 0)
        ])

    -- Values.
    (Map.fromList
        [ ("fix",   ty "(a -> a) -> a")
        , ("neg",   ty "Int -> Int")
        , ("plus",  ty "Int -> Int -> Int")
        , ("minus", ty "Int -> Int -> Int")
        , ("mul",   ty "Int -> Int -> Int")
        , ("div",   ty "Int -> Int -> Int")
        , ("rem",   ty "Int -> Int -> Int")
        , ("eq",    ty "Int -> Int -> Bool")
        , ("neq",   ty "Int -> Int -> Bool")
        , ("lt",    ty "Int -> Int -> Bool")
        , ("le",    ty "Int -> Int -> Bool")
        , ("gt",    ty "Int -> Int -> Bool")
        , ("ge",    ty "Int -> Int -> Bool")
        , ("not_",  ty "Bool -> Bool")
        , ("and_",  ty "Bool -> Bool -> Bool")
        , ("or_",   ty "Bool -> Bool -> Bool")
        , ("xor_",  ty "Bool -> Bool -> Bool")
        , ("if_",   ty "Bool -> a -> a -> a")
        ])

    -- Type signatures.
    (Map.fromList [])
  where
    ty t = case parse scheme "" t of
        Right ts -> ts
        Left  _  -> error "Fatal error: failed to parse the type of\
                          \ built-in function."

-- | Run the type inference for a given module with default contexts
--   and return the result.
runDefault :: Module -> Either TCError TICtx
runDefault ast = runTI (inferModule ast) defaultErrCtx defaultTICtx

-- | Default output file for the resulting C++ code.
defaultOutput :: FilePath
defaultOutput = "a.cpp"

-- | Default include directory.
defaultInclude :: FilePath
defaultInclude = "runtime"
