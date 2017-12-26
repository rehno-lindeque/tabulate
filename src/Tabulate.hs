{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Tabulate
  ( module Tabulate.Types
  , module Tabulate.DefaultInstances
  , tabulate
  ) where

import Tabulate.Types
import Tabulate.Internal
import Tabulate.DefaultInstances

import GHC.Generics
import Data.Proxy (Proxy(..))

-- | Format a list into a tabular rep (2 dimensional matrix of cells)
tabulate :: (Tabulate a rep) => [a] -> [[rep]]
tabulate = map tabulateRow

-- Helper that counts the number of tabulate cells in a datatype
countCells :: proxy a -> Int
countCells proxy = _

-- | Helper for generating empty tabulate cells for a data type
emptyCells :: (FormatCell EmptyCell rep) => proxy a -> [rep]
emptyCells proxy = map formatCell (take (countCells proxy) (repeat EmptyCell))

-- | Find the constructor name for a given choice (inhabiting some sum type)
class ConstructorChoice f where
  choiceConName :: f a -> String
instance Constructor c => ConstructorChoice (C1 c f) where
  choiceConName = conName
instance (ConstructorChoice fa, ConstructorChoice fb) => ConstructorChoice (fa :+: fb) where
  choiceConName (L1 x) = choiceConName x
  choiceConName (R1 x) = choiceConName x

-- | Tabulate a single nullary data constructor
--
-- e.g.
--
-- @
-- data Foo = Foo
-- @
instance
  ( Constructor c
  , FormatCell String rep
  )
  => GTabulate (D1 d (C1 c U1)) rep where
  gtabulateRow (M1 x) = [formatCell (conName x)]

-- | Tabulate a single unary data constructor
instance
  ( GTabulate f rep
  )
  => GTabulate (D1 d (C1 c (S1 s f))) rep
  where
    gtabulateRow = gtabulateRow . unM1 . unM1

-- | Tabulate a sum data type
--
-- e.g.
--
-- @
-- data Foo = fa | ...
-- @
instance
  ( GTabulate (fa :+: fb) rep
  , ConstructorChoice (fa :+: fb)
  , FormatCell String rep
  )
  => GTabulate (D1 d (fa :+: fb)) rep
  where
    gtabulateRow (M1 x) = [formatCell (choiceConName x)] ++ gtabulateRow x

-- | A leaf node of the data type (containing a new data type)
instance
  (FormatCell a rep
  )
  => GTabulate (K1 i a) rep
  where
    gtabulateRow (K1 x) = [formatCell x]

-- | A nullary data constructor inside of a larger type
--
-- e.g.
--
-- @
-- data Foo = ... | Bar | ...
-- @
instance GTabulate U1 rep where
  gtabulateRow _ = []

-- | A unary data constructor (wrapping a single value) inside of a larger type
--
-- e.g.
--
-- >>> data Foo = ... | Foo f | ...
--
instance
  ( GTabulate f rep
  )
  => GTabulate (S1 s f) rep
  where
    gtabulateRow = gtabulateRow . unM1

-- | A constructor inside of a larger type
--
-- e.g.
--
-- @
-- data Foo = ... | (Foo ...) | ...
-- @
instance
  ( GTabulate f rep
  )
  => GTabulate (C1 c f) rep
  where
    gtabulateRow = gtabulateRow . unM1

-- | Mulitple fields inside a bigger type
--
-- e.g.
--
-- @
-- data Foo = ... | Foo fa ... | ...
-- @
instance
  ( GTabulate fa rep
  , GTabulate fb rep
  )
  => GTabulate (fa :*: fb) rep
  where
    gtabulateRow (x :*: y) = gtabulateRow x ++ gtabulateRow y

-- | A partial sum type within a larger sum type
--
-- e.g.
--
-- @
-- data Foo = ... | fa | ...
-- @
instance
  ( GTabulate fa rep
  , GTabulate fb rep
  , FormatCell EmptyCell rep
  )
  => GTabulate (fa :+: fb) rep
  where
    gtabulateRow (L1 x) = gtabulateRow x ++ emptyCells (Proxy :: Proxy fb)
    gtabulateRow (R1 x) = emptyCells (Proxy :: Proxy fa) ++ gtabulateRow x
