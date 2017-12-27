{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Tabulate.Types
  ( Tabulate
  , tabulateRow
  , tabulateRowLabels
  , tabulateInlineRow
  , tabulateInlineRowLabels
  , FormatMeta
  , formatMeta
  , EmptyCell(..)
  )
  where

import Tabulate.Internal
import GHC.Generics (Generic)
import qualified GHC.Generics as Generics (Rep, Meta, from)
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
  default tabulateRow :: (Generic a, GTabulate (Generics.Rep a) rep) => a -> [rep]
  tabulateRow x = gtabulateRow (Generics.from x)

  -- | Format a value into a single row of labels
  tabulateRowLabels :: proxy a -> [rep]
  default tabulateRowLabels :: (GTabulate (Generics.Rep a) rep) => proxy a -> [rep]
  tabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy (Generics.Rep a))

  -- | Format a value into cells forming part of a larger row
  tabulateInlineRow :: a -> [rep]
  tabulateInlineRow = tabulateRow

  -- | Format a value into a single row of labels forming part of a larger row of labels
  tabulateInlineRowLabels :: proxy a -> [rep]
  tabulateInlineRowLabels = tabulateRowLabels

-- * Formatting helpers

-- | Format metadata such as data type names, constructor names and selector names
class FormatMeta (meta :: Generics.Meta) rep where
  formatMeta :: proxy meta -> rep

data EmptyCell = EmptyCell

