{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Text.Format where

import Text.PrettyPrint.ANSI.Leijen (Doc, text)

class Pretty a where
    pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty = text . show .map pretty
