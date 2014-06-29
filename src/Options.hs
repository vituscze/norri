-- | Command line options.
module Options
    (
    -- * Options type
      Options(..)

    -- * Options parsing
    , options
    )
    where

import Control.Applicative
import Data.Monoid
import Options.Applicative
import System.IO

-- | Data type for command options.
data Options
    = Options
    { inputFile  :: FilePath        -- ^ File to be compiled.
    , outputFile :: Maybe FilePath  -- ^ Where to put the resulting C++ code.
    , include    :: Bool            -- ^ Whether to add the C++ runtime
                                    --   via @#include@.
    , includeDir :: Maybe FilePath  -- ^ Location of  the C++ runtime.
    }
    deriving (Eq, Show)

-- | The actual command line parser. All options are aggregated into
--   a single value of type 'Options'.
options :: ParserInfo Options
options = info (helper <*> opts)
    ( fullDesc
   <> progDesc "Compile source code in INPUT into C++ template metaprogram."
    )
  where
    opts = Options
        <$> argument str
            ( metavar "INPUT"
           <> help "Location of the source code"
            )
        <*> (optional . strOption)
            ( long "output"
           <> short 'o'
           <> metavar "OUTPUT"
           <> help "Location of the compiled code"
            )
        <*> switch
            ( long "addinclude"
           <> short 'a'
           <> help "Whether to add #include to the resulting code"
            )
        <*> (optional . strOption)
            ( long "includedir"
           <> short 'i'
           <> metavar "DIR"
           <> help "Location of the C++ \"runtime\""
            )
