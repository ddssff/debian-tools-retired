-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Debian.Debianize.Types.PackageType
    ( PackageType(..)
    , mkPkgName
    , DebType(..)
    , VersionSplits(..)
    , knownVersionSplits
    ) where

import Data.Char (toLower)
import Data.Version (Version(Version))
import Debian.Debianize.Interspersed (Interspersed(leftmost, pairs))
import Debian.Relation (PkgName(pkgNameFromString))
import Distribution.Package (PackageName(PackageName))

-- ^ The different types of binary debs we can produce from a haskell package
data PackageType
    = Development   -- ^ The libghc-foo-dev package.
    | Profiling     -- ^ The libghc-foo-prof package.
    | Documentation -- ^ The libghc-foo-doc package.
    | Exec          -- ^ A package related to a particular executable, perhaps
                    -- but not necessarily a server.
    | Utilities     -- ^ A package that holds the package's data files
                    -- and any executables not assigned to other
                    -- packages.
    | Source'       -- ^ The source package (not a binary deb actually.)
    | Cabal         -- ^ This is used to construct the value for
                    -- DEB_CABAL_PACKAGE in the rules file
    deriving (Eq, Show)

-- | Build a debian package name from a cabal package name and a
-- debian package type.  Unfortunately, this does not enforce the
-- correspondence between the PackageType value and the name type, so
-- it can return nonsense like (SrcPkgName "libghc-debian-dev").
mkPkgName :: PkgName name => PackageName -> PackageType -> name
mkPkgName (PackageName name) typ =
    pkgNameFromString $
             case typ of
                Documentation -> "libghc-" ++ base ++ "-doc"
                Development -> "libghc-" ++ base ++ "-dev"
                Profiling -> "libghc-" ++ base ++ "-prof"
                Utilities -> "haskell-" ++ base ++ "-utils"
                Exec -> base
                Source' -> "haskell-" ++ base ++ ""
                Cabal -> base
    where
      base = map (fixChar . toLower) name
      -- Underscore is prohibited in debian package names.
      fixChar :: Char -> Char
      fixChar '_' = '-'
      fixChar c = toLower c

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Read, Show)

data VersionSplits name
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: name
      , splits :: [(Version, name)] -- Assumed to be in version number order
      }

instance PkgName name => Interspersed (VersionSplits name) name Version where
    leftmost (VersionSplits {splits = []}) = error "Empty Interspersed instance"
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: PkgName name => PackageType -> [VersionSplits name]
knownVersionSplits typ =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = mkPkgName (PackageName "parsec2") typ
      , splits = [(Version [3] [], mkPkgName (PackageName "parsec3") typ)] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = mkPkgName (PackageName "quickcheck1") typ
      , splits = [(Version [2] [], mkPkgName (PackageName "quickcheck2") typ)] }
    ]
