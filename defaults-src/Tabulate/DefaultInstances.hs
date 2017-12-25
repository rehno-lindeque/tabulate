{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Default 'String' (and 'ShowS') formatting for basic data types based on 'Show'.
--   Normally this module will be automatically imported along with "Tabulate" (unless the withdefaults flag is turned off).
module Tabulate.DefaultInstances where

import Tabulate.Types
import Text.Printf

instance FormatCell EmptyCell String {- TODO: Text -} where
  formatCell _ = "-"

-- instance FormatCell EmptyLabel String {- TODO: Text -} where
--   formatCell _ = "-"

instance FormatCell Integer String {- TODO: Text -} where
  formatCell x = printf "%d" x

instance FormatCell Int String where
  formatCell x = printf "%d" x

instance FormatCell Float String where
  formatCell x = printf "%14.9g" x

instance FormatCell String String where
  formatCell x = printf "%s" x

instance FormatCell Double String where
  formatCell x = printf "%14.9g" x

instance FormatCell Bool String


