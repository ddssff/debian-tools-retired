{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Types.Dependencies
    ( DependencyHints(..)
    , defaultDependencyHints
    , VersionSplits(..)
    , filterMissing
    , PackageInfo(..)
    , debNameFromType
    ) where

import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Debian.Control
import Debian.Debianize.Types.PackageType (DebType(Dev, Prof, Doc), VersionSplits(..), knownVersionSplits)
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import qualified Debian.Relation as D
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)

data DependencyHints
    = DependencyHints
      {
      ---------------------------------------------------------------------------
      -- Build Dependencies - Controling the Build-Depends and
      -- Build-Depends-Indep fields of the debian source package.
      ---------------------------------------------------------------------------
        buildDeps :: [BinPkgName]
      -- ^ Add a debian binary package to the debianization's list of
      -- build dependencies.  These will be in addition to dependencies
      -- derived from the cabal build-depends list.
      , missingDependencies :: [BinPkgName]
      -- ^ Mark a package missing, do not add it to any dependency lists
      -- in the debianization.  If some cabal build dependency foo has
      -- no documentation package (because it was built with the haddock
      -- option set to False) you would need to add libghc-foo-doc to
      -- this list.
      , extraLibMap :: Map.Map String [BinPkgName]
      -- ^ Specify which debian binary packages correspond to the
      -- packages specified in the cabal file Extra-Library field,
      -- e.g. Map.insert extraLibMap "cryptopp" "libcrypto-dev" adds a
      -- build dependency on libcrypto-dev to any package that has
      -- cryptopp in its cabal Extra-Library list.
      , execMap :: Map.Map String BinPkgName
      -- ^ Specify a mapping from the name appearing in the Build-Tool
      -- field of the cabal file to a debian binary package name,
      -- e.g. Map.insert execMap "trhsx" "haskell-hsx-utils" adds a
      -- build dependency on haskell-hsx-utils to any package that has
      -- trhsx in its cabal build-tool list.
      , omitLTDeps :: Bool
      -- ^ Don't generate the << dependency when we see a cabal equals
      -- dependency.

      ---------------------------------------------------------------------------
      -- Install Dependencies - controlling the Depends, Conflicts,
      -- Provides, and Replaces fields of the debian binary packages.
      ---------------------------------------------------------------------------
      , extraDevDeps :: [BinPkgName]
      -- ^ Add a debian binary package to the list of dependencies of
      -- the libghc-foo-dev package produced by the debianization.
      -- E.g., when building the haskell-terminfo package we put
      -- "libncurses5-dev" into this list to make it an install
      -- dependency of libghc-terminfo-dev.
      , binaryPackageDeps :: [(BinPkgName, BinPkgName)]
      -- ^ An entry (a, b) says that debian package a should have a
      -- dependency on b, e.g. ("cabal-debian", "apt-file") says that
      -- cabal-debian program needs apt-file to be installed.
      , binaryPackageConflicts :: [(BinPkgName, BinPkgName)]
      -- ^ An entry (a, b) says that two debian packages should both
      -- have Conflicts entries referring to each other.  For example
      -- [("libghc-quickcheck1-doc", "libghc-quickcheck2-doc")] says
      -- that libghc-quickcheck1-doc conflicts with
      -- libghc-quickcheck2-doc, and vice versa.

      ---------------------------------------------------------------
      -- Debian Binary Packages - Controlling which debian binary ---
      -- packages will be created.                                ---
      ---------------------------------------------------------------
{-
      , debLibProf :: Bool
      -- ^ Should we generate? profiling libraries.  At certain times it
      -- was necessary to turn this off to work around compiler bugs.
      , haddock :: Bool
      -- ^ Should we generate the library documentation package?  Also
      -- used at times to avoid haddock bugs or, as the case may be,
      -- bugs in the haddock markup of some packages.
-}

      -------------------------
      -- Version Numbers
      -------------------------
      , epochMap :: Map.Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @Map.insert epochMap "HTTP" 1@. 
      , revision :: String
      -- ^ Specify the revision string to use when converting the cabal
      -- version to debian.
      , debVersion :: Maybe DebianVersion
      -- ^ Specify the exact debian version of the resulting package,
      -- including epoch.  One use case is to work around the the
      -- "buildN" versions that are often uploaded to the debian and
      -- ubuntu repositories.  Say the latest cabal version of
      -- transformers is 0.3.0.0, but the debian repository contains
      -- version 0.3.0.0-1build3, we need to specify
      -- debVersion="0.3.0.0-1build3" or the version we produce will
      -- look older than the one already available upstream.
      , versionSplits :: [VersionSplits]
      -- ^ Instances where the debian package name is different (for
      -- some range of version numbers) from the default constructed
      -- by mkPkgName.
      } deriving (Eq, Ord, Show)

defaultDependencyHints :: DependencyHints
defaultDependencyHints =
    DependencyHints
    { missingDependencies = []
{-
    , debLibProf = True
    , haddock = True
-}
    , extraDevDeps = []
    , extraLibMap = Map.empty
    , binaryPackageDeps = []
    , binaryPackageConflicts = []
    , execMap = Map.empty
    , omitLTDeps = False
    , buildDeps = []
    , epochMap = Map.empty
    -- Setting revision to @-1~hackage1@ produces a version number
    -- that is slightly older looking than an initial version from
    -- debian, which will have revision @-1@.  However, it really
    -- ought to be a site policy.
    -- , revision = "-1~hackage1"
    , revision = ""
    , debVersion = Nothing
    , versionSplits = knownVersionSplits
    }

filterMissing :: DependencyHints -> [[D.Relation]] -> [[D.Relation]]
filterMissing hints rels =
    filter (/= []) (map (filter (\ (D.Rel name _ _) -> not (elem name (missingDependencies hints)))) rels)

data PackageInfo = PackageInfo { libDir :: FilePath
                               , cabalName :: String
                               , cabalVersion :: String
                               , devDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , profDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , docDeb :: Maybe (D.BinPkgName, DebianVersion) }

-- | Given a control file and a DebType, look for the binary deb with
-- the corresponding suffix and return its name.
debNameFromType :: Control' String -> DebType -> Maybe D.BinPkgName
debNameFromType control debType =
    case debType of
      Dev -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))
