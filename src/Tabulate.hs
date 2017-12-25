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
import Data.Proxy (Proxy)

-- | Format a list into a tabular rep (2 dimensional matrix of cells)
tabulate :: (Tabulate a rep) => [a] -> [[rep]]
tabulate = map tabulateRow

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

-- | A nullary data constructor inside a sum type
--
-- e.g.
--
-- @
-- data Foo = ... | Bar | ...
-- @
instance GTabulate U1 rep where
  gtabulateRow _ = []

-- | A constructor inside a sum type
--
-- e.g.
--
-- @
-- data Foo = ... | (Bar ...) | ...
-- @
instance
  ( GTabulate f rep
  )
  => GTabulate (C1 c f) rep
  where
    gtabulateRow = gtabulateRow . unM1

-- | A partial sum type within a sum type
--
-- e.g.
--
-- @
-- data Foo = ... | fa | ...
-- @
instance
  ( GTabulate fa rep
  , GTabulate fb rep
  )
  => GTabulate (fa :+: fb) rep
  where
    gtabulateRow (L1 x) = gtabulateRow x
    gtabulateRow (R1 x) = gtabulateRow x

