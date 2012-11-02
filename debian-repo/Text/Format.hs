{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Text.Format where

import Text.PrettyPrint (Doc, text)

class Pretty a where
    pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty = text . show .map pretty
