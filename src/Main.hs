-- The main module.
module Main
    (
    -- * Main
      main

    -- * Compilation
    , compile
    )
    where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Options.Applicative
import Text.Parsec
import System.FilePath
import System.IO

import Compiler.AST
import Compiler.Compile
import Compiler.Parser
import Compiler.Transform
import Default
import Options
import Report

-- | Compile the module and write it into specified file.
compile :: Module
        -> Bool      -- ^ Whether to add the content directly.
        -> Bool      -- ^ Whether to add @#include@ guards.
        -> FilePath  -- ^ Include directory.
        -> FilePath  -- ^ Destination.
        -> IO ()
compile m direct guards include dest = withFile dest WriteMode $ \h -> do
    let guardName = "TMPCOMPILER_OUTPUT_" ++ map toUpper (takeBaseName dest)
    when guards $ do
        hPutStrLn h $ "#ifndef " ++ guardName
        hPutStrLn h $ "#define " ++ guardName
    addHeader h
    hPutStrLn h $ compileModule (freshModule m)
    when guards $
        hPutStrLn h $ "#endif // " ++ guardName
  where
    addHeader h = forM_ defaultRuntime $ \rt ->
        let path = include </> rt
        in if direct
            then readFile path >>= hPutStrLn h
            else hPutStrLn h . concat $
                [ "#include \""
                , intercalate "/" $ splitDirectories path
                , "\""
                ]

main :: IO ()
main = do
    opts <- execParser options

    let input   = inputFile opts
        output  = fromMaybe defaultOutput (outputFile opts)
        direct  = addRuntime opts
        include = fromMaybe defaultInclude (includeDir opts)
        guards  = useGuards opts

    code <- readFile input
    ast  <- either reportParseError return (parse file input code)

    let ast' = fixifyModule ast

    _    <- either reportTCError return (runDefault ast')
    compile ast' direct guards include output
