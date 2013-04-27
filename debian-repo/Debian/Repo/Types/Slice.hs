{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Slice
    ( SliceList(..)
    , NamedSliceList(..)
    ) where

import Debian.Sources ( SliceName(..), DebSource(..), SourceType(..) )
import Text.PrettyPrint.ANSI.Leijen (vcat, Pretty(pretty))

import Debian.Repo.Types.Repository (Repository)

----------------- SLICES (SOURCES.LIST ENTRIES) ---------------

deriving instance Show SourceType
deriving instance Show DebSource

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [(Repository, DebSource)]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = vcat . map (pretty . snd) . slices
