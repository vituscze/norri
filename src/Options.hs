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
import Options.Applicative

-- | Data type for command options.
data Options
    = Options
    { inputFile  :: FilePath        -- ^ File to be compiled.
    , outputFile :: Maybe FilePath  -- ^ Where to put the resulting C++ code.
    , addRuntime :: Bool            -- ^ Whether to add the C++ runtime
                                    --   directly.
    , includeDir :: Maybe FilePath  -- ^ Location of  the C++ runtime.
    , useGuards  :: Bool            -- ^ Whether to use @#include@ guards.
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
            ( long "addruntime"
           <> short 'a'
           <> help "Whether to directly include the runtime"
            )
        <*> (optional . strOption)
            ( long "includedir"
           <> short 'i'
           <> metavar "DIR"
           <> help "Location of the C++ \"runtime\""
            )
        <*> (fmap not . switch)
            ( long "noguards"
           <> short 'n'
           <> help "Do not add #include guards"
            )
