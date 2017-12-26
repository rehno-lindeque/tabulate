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
data Example4 = A4 Int Int deriving Generic
data Example5 = A5 Int | B5 | C5 Int Int Int deriving Generic
data Example6 = A6 { ax6 :: Int } | B6 Int | C6 { cx6 :: Int, cy6 :: Int, cz6 :: Int } deriving Generic

instance Tabulate Example1 String
instance Tabulate Example2 String
instance Tabulate Example3 String
instance Tabulate Example4 String
instance Tabulate Example5 String
instance Tabulate Example6 String

a1 = A1
a2 = A2
b2 = B2
a3 = A3 99
a4 = A4 99 88
a5 = A5 99
b5 = B5
c5 = C5 99 88 77
a6 = A6 99
b6 = B6 99
c6 = C6 99 88 77

-- | Print a comma-Separated table of values
printTable :: (Tabulate a String) => [a] -> IO ()
printTable xs =
  let headings = intercalate "," (tabulateRowLabels xs)
      rows = map (intercalate ",") (tabulate xs)
  in putStrLn headings >> mapM_ putStrLn rows

