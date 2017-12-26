{-# LANGUAGE MultiParamTypeClasses #-}
module Tabulate.Internal where

-- | Internal class used with the 'Generic' instance to tabulate data types automatically
class GTabulate f rep where
  gtabulateRow :: f a -> [rep]
  gtabulateRowLabels :: proxy f -> [rep]
