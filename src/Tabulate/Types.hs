{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tabulate.Types
  ( Tabulate
  , tabulateRow
  , tabulateRowLabels
  , FormatCell
  , formatCell
  , EmptyCell(..)
  )
  where

import Tabulate.Internal
import GHC.Generics (Generic, Rep, from)
import Data.Proxy (Proxy(..))

-- * Type classes

-- | Top-level class to derive. The row type is given by 'a' and the resulting cell (after formatting) is given by 'rep'.
--   A typical instance will look as follows:
--
-- @
--     data Foo = Foo { fooField :: Int } deriving (Generic)
--     instance Tabulate Foo String
-- @
class Tabulate a rep where
  -- | Format a value into a single row of cells
  tabulateRow :: a -> [rep]
  default tabulateRow :: (Generic a, GTabulate (Rep a) rep) => a -> [rep]
  tabulateRow x = gtabulateRow (from x)

  tabulateRowLabels :: proxy a -> [rep]
  default tabulateRowLabels :: (GTabulate (Rep a) rep) => proxy a -> [rep]
  tabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy (Rep a))

-- | Supply formatting for the final representation of each cell
class FormatCell a rep where

  -- | Convert from an arbitrary type to the table representation of a cell
  formatCell :: a -> rep

  -- Our default instance uses show
  default formatCell :: (Show a, rep ~ String) => a -> rep
  formatCell x = show x

-- * Formatting helpers
data EmptyCell = EmptyCell

