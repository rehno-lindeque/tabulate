{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- TODO: make this more specialized
formatLabel :: (FormatCell a rep) => a -> rep
formatLabel = formatCell

-- | Helper that counts the number of tabulate cells in a datatype
countCells :: (GTabulate f rep) => proxy f -> proxy' rep -> Int
countCells proxyf (proxyrep :: proxy' rep) =
  length (gtabulateRowLabels proxyf :: [rep])

-- | Helper for generating empty tabulate cells for a data type
emptyCells :: (FormatCell EmptyCell rep, GTabulate f rep) => proxy f -> proxy' rep -> [rep]
emptyCells proxyf proxyrep = map formatCell (take ncells (repeat EmptyCell))
  where
    ncells = countCells proxyf proxyrep

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
  ( Datatype d
  , Constructor c
  , FormatCell String rep
  )
  => GTabulate (D1 d (C1 c U1)) rep where
  gtabulateRow (M1 x) = [formatCell (conName x)]
  gtabulateRowLabels _ = [formatLabel (datatypeName dat)]
    where
      dat = (undefined :: t d f a)

-- | Tabulate a single unary data constructor
--
-- e.g.
--
-- @
-- data Foo = Foo fa
-- @
instance
  ( GTabulate f rep
  , Selector s
  , Constructor c
  , FormatCell String rep
  )
  => GTabulate (D1 d (C1 c (S1 s f))) rep
  where
    gtabulateRow = gtabulateRow . unM1 . unM1
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy (C1 c (S1 s f)))

-- | Tabulate a single data constructor with multiple fields
--
-- e.g.
--
-- @
-- data Foo = Foo fa ...
-- @
instance
  ( GTabulate (fa :*: fb) rep
  , Constructor c
  , FormatCell String rep
  )
  => GTabulate (D1 d (C1 c (fa :*: fb))) rep
  where
    gtabulateRow = gtabulateRow . unM1 . unM1
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy (C1 c (fa :*: fb)))

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
  , Datatype d
  -- , ConstructorChoice (fa :+: fb)
  -- , FormatCell String rep
  )
  => GTabulate (D1 d (fa :+: fb)) rep
  where
    gtabulateRow (M1 x) = [formatCell (choiceConName x)] ++ gtabulateRow x
    gtabulateRowLabels _ = [formatLabel (datatypeName dat)] ++ gtabulateRowLabels (Proxy :: Proxy (fa :+: fb))
      where
        dat = (undefined :: t d f a)

-- | A leaf node of the data type (containing a new data type)
instance
  ( FormatCell a rep
  , FormatCell String rep
  )
  => GTabulate (K1 i a) rep
  where
    gtabulateRow (K1 x) = [formatCell x]
    gtabulateRowLabels _ = [formatLabel ""]

-- | A nullary data constructor inside of a larger type
--
-- e.g.
--
-- @
-- data Foo = ... | Bar | ...
-- @
instance GTabulate U1 rep where
  gtabulateRow _ = []
  gtabulateRowLabels _ = []

-- | A unary data constructor (wrapping a single value) inside of a larger type
--
-- e.g.
--
-- >>> data Foo = ... | Foo f | ...
--
instance
  ( GTabulate f rep
  , Selector s
  , FormatCell String rep
  )
  => GTabulate (S1 s f) rep
  where
    gtabulateRow = gtabulateRow . unM1
    gtabulateRowLabels _ = [formatLabel (selName sel)]
      where
        sel = (undefined :: t s f a)

-- | A constructor inside of a larger type
--
-- e.g.
--
-- @
-- data Foo = ... | (Foo ...) | ...
-- @
instance
  ( GTabulate f rep
  , Constructor c
  , FormatCell String rep
  )
  => GTabulate (C1 c f) rep
  where
    gtabulateRow = gtabulateRow . unM1
    gtabulateRowLabels _ =
      if conIsRecord con
      then gtabulateRowLabels (Proxy :: Proxy f)
      else
        map (formatLabel . (conName con ++) . ("._" ++) . show) (take ncells [1 :: Int ..])
      where
        con = (undefined :: t c f p)
        ncells = countCells (Proxy :: Proxy f) (Proxy :: Proxy rep)

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
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy fa) ++ gtabulateRowLabels (Proxy :: Proxy fb)

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
    gtabulateRow (L1 x) = gtabulateRow x ++ emptyCells (Proxy :: Proxy fb) (Proxy :: Proxy rep)
    gtabulateRow (R1 x) = emptyCells (Proxy :: Proxy fa) (Proxy :: Proxy rep) ++ gtabulateRow x
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy fa) ++ gtabulateRowLabels (Proxy :: Proxy fb)
