{-# LANGUAGE MultiParamTypeClasses #-}
module Tabulate.Internal where

-- | Internal class used with the 'Generic' instance to tabulate data types automatically
class GTabulate f rep where
  gtabulateRow :: f a -> [rep]
  gtabulateRowLabels :: proxy f -> [rep]

-- | Find the constructor name in a sum type and format it
class FormatConstructor f rep where
  formatConstructor :: f a -> rep
