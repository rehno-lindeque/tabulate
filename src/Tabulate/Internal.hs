{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Tabulate.Internal where

-- import GHC.Generics

-- | Internal class used with the 'Generic' instance to tabulate data types automatically
class GTabulate f rep where
  gtabulateRow :: f a -> [rep]

