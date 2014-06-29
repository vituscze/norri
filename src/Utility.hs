-- | Utility functions and types.
module Utility
    (
    -- * Name manipulation
      uncap
    )
    where

import Data.Char

-- | Uncapitalise a name.
--
-- >>> uncap "Name"
-- "name"
uncap :: String -> String
uncap ""     = ""
uncap (c:cs) = toLower c:cs
