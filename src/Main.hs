-- Temporary main.
module Main
    (
      main
    )
    where

import qualified Data.Map as Map
import Data.Map (Map)
import System.Exit
import System.IO
import Text.Parsec

import Compiler.Parser
import Compiler.Pretty
import Compiler.Transform
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Infer

defaultCtx :: TICtx
defaultCtx =
  ( Map.fromList [("Int", 0)]
  , Map.fromList []
  , Map.fromList []
  )

main :: IO ()
main = do
    input <- readFile "input.tmpc"
    ast <- case parse file "" input of
        Right ast -> return ast
        Left  err -> putStrLn "Parsing error:" >> print err >> exitFailure
    let ast' = fixifyModule ast
    case runTI (inferModule defaultCtx ast') of
        Right _   -> return ()
        Left  err -> putStrLn "Type checking error:" >> print err >> exitFailure
    writeFile "output.cpp" (prettyModule (freshModule ast'))

