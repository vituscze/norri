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

import Compiler.Parser
import Compiler.Pretty
import Compiler.Transform
import Compiler.TypeChecking.Context
import Compiler.TypeChecking.Infer
import Options

defaultCtx :: TICtx
defaultCtx =
  ( Map.fromList [("Bool", 0), ("Int", 0)]
  , Map.fromList []
  , Map.fromList []
  )

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
        Left  err -> putStrLn "Parsing error:" >> print err >> exitFailure
    let ast' = fixifyModule ast
    case runTI (inferModule defaultCtx ast') of
        Right _   -> return ()
        Left  err -> putStrLn "Type checking error:" >> print err >> exitFailure
    writeFile output (prettyModule (freshModule ast'))
