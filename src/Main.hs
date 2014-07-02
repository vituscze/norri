-- Temporary main.
module Main
    (
      main
    )
    where

import Data.Maybe
import Options.Applicative
import Text.Parsec

import Compiler.Compile
import Compiler.Parser
import Compiler.Transform
import Default
import Options
import Report

main :: IO ()
main = do
    opts <- execParser options

    let input  = inputFile opts
        output = fromMaybe defaultOutput (outputFile opts)

    code <- readFile input
    ast  <- either reportParseError return (parse file input code)

    let ast' = fixifyModule ast

    _    <- either reportTCError return (runDefault ast')
    writeFile output (compileModule (freshModule ast'))
