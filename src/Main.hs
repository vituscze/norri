-- Temporary main.
module Main
    (
      main
    )
    where

import qualified Data.Map as Map
import Data.Maybe
import Options.Applicative
import System.Exit
import Text.Parsec

import Compiler.Compile
import Compiler.Parser
import Compiler.Transform
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Infer
import Options
import Report

defaultCtx :: TICtx
defaultCtx = TICtx
    (Map.fromList [("Bool", 0), ("Int", 0), ("Type", 0)])
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
    (Map.fromList [])
  where
    ty t = case parse scheme "" t of
        Right ts -> ts
        Left  _  -> error "Fatal error: failed to parse the type of\
                          \ built-in function."

defaultOutput :: String
defaultOutput = "a.cpp"

main :: IO ()
main = do
    opts <- execParser options

    let input  = inputFile opts
        output = fromMaybe defaultOutput (outputFile opts)

    code <- readFile input
    ast  <- case parse file "" code of
        Right ast -> return ast
        Left  err -> print err >> exitFailure
    let ast' = fixifyModule ast
    case runTI (inferModule ast') [] defaultCtx of
        Right _   -> return ()
        Left  err -> reportTCError err >> exitFailure
    writeFile output (compileModule (freshModule ast'))
