{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Files2
    ( debianName             -- Used in Debian.Debianize.Files and
    , mkPkgName
    , mkPkgName'
    ) where

import Data.Char (toLower)
import Data.Lens.Lazy (access)
import Data.Map as Map (lookup)
import Data.Version (showVersion)
import Debian.Debianize.Facts.Types as Debian (PackageType(..))
import Debian.Debianize.Facts.Lenses as Lenses (debianNameMap, packageDescription)
import Debian.Debianize.Facts.Monad (DebT)
import Debian.Debianize.VersionSplits (doSplits, VersionSplits)
import Debian.Orphans ()
import Debian.Relation (PkgName(..), Relations)
import qualified Debian.Relation as D (VersionReq(EEQ))
import Debian.Version (parseDebianVersion)
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (unlines)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

-- | Build the Debian package name for a given package type.
debianName :: (Monad m, PkgName name) => PackageType -> DebT m name
debianName typ =
    do Just pkgDesc <- access packageDescription
       let pkgId = Cabal.package pkgDesc
       nameMap <- access Lenses.debianNameMap
       return $ debianName' (Map.lookup (pkgName pkgId) nameMap) typ pkgId

-- | Function that applies the mapping from cabal names to debian
-- names based on version numbers.  If a version split happens at v,
-- this will return the ltName if < v, and the geName if the relation
-- is >= v.
debianName' :: (PkgName name) => Maybe VersionSplits -> PackageType -> PackageIdentifier -> name
debianName' msplits typ pkgId =
    case msplits of
      Nothing -> mkPkgName pname typ
      Just splits -> (\ s -> mkPkgName' s typ) $ doSplits splits version
    where
      -- def = mkPkgName pname typ
      pname@(PackageName _) = pkgName pkgId
      version = (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion pkgId)))))

-- | Build a debian package name from a cabal package name and a
-- debian package type.  Unfortunately, this does not enforce the
-- correspondence between the PackageType value and the name type, so
-- it can return nonsense like (SrcPkgName "libghc-debian-dev").
mkPkgName :: PkgName name => PackageName -> PackageType -> name
mkPkgName pkg typ = mkPkgName' (debianBaseName pkg) typ

mkPkgName' :: PkgName name => String -> PackageType -> name
mkPkgName' base typ =
    pkgNameFromString $
             case typ of
                Documentation -> "libghc-" ++ base ++ "-doc"
                Development -> "libghc-" ++ base ++ "-dev"
                Profiling -> "libghc-" ++ base ++ "-prof"
                Utilities -> "haskell-" ++ base ++ "-utils"
                Exec -> base
                Source' -> "haskell-" ++ base ++ ""
                Cabal -> base

debianBaseName :: PackageName -> String
debianBaseName (PackageName name) =
    map (fixChar . toLower) name
    where
      -- Underscore is prohibited in debian package names.
      fixChar :: Char -> Char
      fixChar '_' = '-'
      fixChar c = toLower c
