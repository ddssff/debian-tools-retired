{-# OPTIONS_GHC -Wall -Werror #-}
-- | Known instances where you can't infer the debian package name
-- from the cabal package name, including when the debian package name
-- is a function of the cabal package version.
module Distribution.Debian.Splits
    ( versionSplits
    ) where

import Data.Version (Version(Version))
import qualified Debian.Relation as D
import Distribution.Debian.Dependencies (PackageType(..), VersionSplits(..), mkPkgName)
import Distribution.Package (PackageName(..))

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
versionSplits :: D.PkgName name => PackageType -> [VersionSplits name]
versionSplits typ =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = mkPkgName "parsec2" typ
      , splits = [(Version [3] [], mkPkgName "parsec3" typ)] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = mkPkgName "quickcheck1" typ
      , splits = [(Version [2] [], mkPkgName "quickcheck2" typ)] }
    ]
