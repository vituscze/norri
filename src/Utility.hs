-- | Utility functions and types.
module Utility
    (
    -- * Name manipulation
      uncap
    , nameTyVar
    , nameVar
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

-- | Create a name for type variable given a unique number.
--
-- >>> nameTyVar 1
-- "t1"
nameTyVar :: Int -> String
nameTyVar n = "t" ++ show n

-- | Create a name for a bound name given a unique number.
--
-- >>> nameVar 1
-- "_T1"
nameVar :: Int -> String
nameVar n = "_T" ++ show n
