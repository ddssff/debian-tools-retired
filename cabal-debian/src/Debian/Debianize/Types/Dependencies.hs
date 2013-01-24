{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Types.Dependencies
    ( debNameFromType
    ) where

import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, listToMaybe)
import Debian.Control
import Debian.Debianize.Types.PackageType (DebType(Dev, Prof, Doc))
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Version (DebianVersion)

-- | Given a control file and a DebType, look for the binary deb with
-- the corresponding suffix and return its name.
debNameFromType :: Control' String -> DebType -> Maybe BinPkgName
debNameFromType control debType =
    case debType of
      Dev -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))
