{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Tabulate.Example
  (
    -- * Quick start
    -- $quick_start

    -- * Imports and language extensions
    -- $imports_and_extensions

    -- * Deriving tabulate automatically
    -- $deriving

    module Tabulate.Example

  ) where


-- $quick_start
--
-- You can play with this interactively by loading it into ghci like this:
--

-- $imports_and_extensions
--
-- The following extensions are required to use tabulate: 
--
-- @
--     {-# LANGUAGE FlexibleInstances #-}
--     {-# LANGUAGE MultiParamTypeClasses #-}
--     {-# LANGUAGE DeriveGeneric #-}
-- @

import Tabulate
import GHC.Generics (Generic)
import Data.List (intercalate)


-- $deriving
-- Tabulate uses GHC's 'Generic' along with @DeriveGeneric@ to automatically derive a 'Tabulate' instance for you algebraic data types.

data Example1 = A1 deriving Generic
data Example2 = A2 | B2 deriving Generic
data Example3 = A3 Int deriving Generic
data Example4 = A4 Int | B4 | C4 Int Int Int deriving Generic
data Example5 = A5 { ax5 :: Int } | B5 Int | C5 { cx5 :: Int, cy5 :: Int, cz5 :: Int } deriving Generic

instance Tabulate Example1 String
instance Tabulate Example2 String
instance Tabulate Example3 String
-- instance Tabulate Example4 String
-- instance Tabulate Example5 String

a1 = A1
a2 = A2
b2 = B2
a3 = A3 99
a4 = A4 99
b4 = B4
c4 = C4 99 88 77
a5 = A5 99
b5 = B5 99
c5 = C5 99 88 77

-- | Print a comma-Separated table of values
printTable :: (Tabulate a String) => [a] -> IO ()
printTable xs =
  let rows = map (intercalate ",") (tabulate xs)
  in mapM_ putStrLn rows

