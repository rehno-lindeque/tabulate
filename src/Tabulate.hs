{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Tabulate
  ( module Tabulate.Types
  , module Tabulate.DefaultInstances
  , tabulate
  ) where

import Tabulate.Types
import Tabulate.Internal
import Tabulate.DefaultInstances
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

-- | Format a list into a tabular rep (2 dimensional matrix of cells)
tabulate :: (Tabulate a rep) => [a] -> [[rep]]
tabulate = map tabulateRow

-- * Helpers

-- | Helper for generating a cell
formatCell :: (Tabulate a rep) => a -> [rep]
formatCell = tabulateInlineRow

-- | Helper for generating a label cell
formatLabel :: forall proxy (meta :: Meta) rep. (Tabulate (proxy meta) rep) => proxy meta -> [rep]
formatLabel = formatCell

-- | Helper that counts the number of tabulate cells in a datatype
countCells :: forall proxy proxy' f rep. (GTabulate f rep) => proxy f -> proxy' rep -> Int
countCells proxyf _ =
  length (gtabulateRowLabels proxyf :: [rep])

-- | Helper for generating empty tabulate cells for a data type
emptyCells :: forall proxy f rep. (Tabulate EmptyCell rep, GTabulate f rep) => proxy f -> [rep]
emptyCells proxyf = concatMap formatCell (take ncells (repeat EmptyCell))
  where
    ncells = countCells proxyf (Proxy :: Proxy rep)

-- * Formatting instances

instance (FormatMeta c rep) => FormatConstructor (C1 c f) rep where
  formatConstructor _ = formatMeta (Proxy :: Proxy c)
instance (FormatConstructor fa rep, FormatConstructor fb rep) => FormatConstructor (fa :+: fb) rep where
  formatConstructor (L1 x) = formatConstructor x
  formatConstructor (R1 x) = formatConstructor x

instance (KnownSymbol n) => FormatMeta (MetaData n m p nt) String where
  formatMeta _ = symbolVal (Proxy :: Proxy n)
instance (KnownSymbol n) => FormatMeta (MetaCons n f r) String where
  formatMeta _ = symbolVal (Proxy :: Proxy n)
instance (KnownSymbol n) => FormatMeta (MetaSel (Just n) su ss ds) String where
  formatMeta _ = symbolVal (Proxy :: Proxy n)

-- * Generic instances

-- | Tabulate a single nullary data constructor
--
-- e.g.
--
-- @
-- data Foo = Foo
-- @
instance
  ( FormatMeta c rep
  , FormatMeta d rep
  )
  => GTabulate (D1 d (C1 c U1)) rep where
  gtabulateRow (M1 x) = [formatMeta (Proxy :: Proxy c)]
  gtabulateRowLabels _ = [formatMeta (Proxy :: Proxy d)]

-- | Tabulate a single unary data constructor
--
-- e.g.
--
-- @
-- data Foo = Foo fa
-- @
instance
  ( GTabulate f rep
  , GTabulate (S1 s f) rep
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
  , FormatConstructor (fa :+: fb) rep
  , FormatMeta d rep
  )
  => GTabulate (D1 d (fa :+: fb)) rep
  where
    gtabulateRow (M1 x) = formatConstructor x : gtabulateRow x
    gtabulateRowLabels _ = formatMeta (Proxy :: Proxy d) : gtabulateRowLabels (Proxy :: Proxy (fa :+: fb))

-- | A leaf node of the data type (containing a new data type)
instance
  ( Tabulate a rep
  , Tabulate String rep
  )
  => GTabulate (K1 i a) rep
  where
    gtabulateRow (K1 x) = formatCell x
    gtabulateRowLabels _ = tabulateInlineRowLabels (Proxy :: Proxy a)

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
  )
  => GTabulate (S1 (MetaSel Nothing su ss ds) f) rep
  where
    gtabulateRow = gtabulateRow . unM1
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy f)

instance
  ( GTabulate f rep
  , FormatMeta (MetaSel (Just n) su ss ds) rep
  )
  => GTabulate (S1 (MetaSel (Just n) su ss ds) f) rep
  where
    gtabulateRow = gtabulateRow . unM1
    gtabulateRowLabels _ = [formatMeta (Proxy :: Proxy (MetaSel (Just n) su ss ds))]

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
    gtabulateRowLabels _ =
      gtabulateRowLabels (Proxy :: Proxy f)
      -- if conIsRecord con
      -- then gtabulateRowLabels (Proxy :: Proxy f)
      -- else
      --   -- concatMap (formatLabel . (conName con ++) . ("._" ++) . show) (take ncells [1 :: Int ..])
      --   if ncells == 1
      --   then formatLabel (conName con)
      --   else gtabulateRowLabels (Proxy :: Proxy f)
      -- where
      --   con = (undefined :: t c f p)
      --   ncells = countCells (Proxy :: Proxy f) (Proxy :: Proxy rep)

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
  , Tabulate EmptyCell rep
  )
  => GTabulate (fa :+: fb) rep
  where
    gtabulateRow (L1 x) = gtabulateRow x ++ emptyCells (Proxy :: Proxy fb)
    gtabulateRow (R1 x) = emptyCells (Proxy :: Proxy fa) ++ gtabulateRow x
    gtabulateRowLabels _ = gtabulateRowLabels (Proxy :: Proxy fa) ++ gtabulateRowLabels (Proxy :: Proxy fb)
