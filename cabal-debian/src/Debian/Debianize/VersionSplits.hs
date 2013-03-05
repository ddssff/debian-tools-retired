module Debian.Debianize.VersionSplits
    ( mapCabal
    , splitCabal
    ) where

import Data.Lens.Lazy (modL)
import Data.Map as Map (alter)
import Data.Version (Version)
import Debian.Debianize.Atoms (Atoms, debianNameMap)
import Debian.Debianize.Types.VersionSplits (VersionSplits, makePackage, insertSplit)
import Distribution.Package (PackageName)

-- | Add a VersionSplits value to 'atoms' that maps the given cabal
-- name 'pname' to the given debian name 'dname'.
mapCabal :: PackageName -> String -> Atoms -> Atoms
mapCabal pname dname atoms =
    modL debianNameMap (Map.alter f pname) atoms
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = Just (makePackage dname)
      f (Just sp) = error $ "mapCabal - already mapped: " ++ show sp

-- | Map the versions of 'pname' less than 'ver' to the given debian
-- name 'ltname'.
splitCabal :: PackageName -> String -> Version -> Atoms -> Atoms
splitCabal pname ltname ver atoms =
    modL debianNameMap (Map.alter f pname) atoms
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = error $ "splitCabal - not mapped: " ++ show pname
      f (Just sp) = Just (insertSplit ver ltname sp)
