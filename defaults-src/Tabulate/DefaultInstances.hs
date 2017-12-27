{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Default 'String' (and 'ShowS') formatting for basic data types based on 'Show'.
--   Normally this module will be automatically imported along with "Tabulate" (unless the withdefaults flag is turned off).
module Tabulate.DefaultInstances where

import Tabulate.Types
import Text.Printf

instance Tabulate EmptyCell String where
  tabulateRow _ = [""]
  tabulateRowLabels _ = ["EmptyCell"]

instance Tabulate Integer String where
  tabulateRow x = [printf "%d" x]
  tabulateRowLabels _ = ["Integer"]

instance Tabulate Int String where
  tabulateRow x = [printf "%d" x]
  tabulateRowLabels _ = ["Int"]

instance Tabulate Float String where
  tabulateRow x = [printf "%14.9g" x]
  tabulateRowLabels _ = ["Float"]

instance Tabulate String String where
  tabulateRow x = [printf "%s" x]
  tabulateRowLabels _ = ["String"]

instance Tabulate Double String where
  tabulateRow x = [printf "%14.9g" x]
  tabulateRowLabels _ = ["Double"]

instance Tabulate Bool String where
  tabulateRow x = [show x]
  tabulateRowLabels _ = ["Bool"]

instance Tabulate () String where
  tabulateRow x = [show x]
  tabulateRowLabels _ = ["()"]
