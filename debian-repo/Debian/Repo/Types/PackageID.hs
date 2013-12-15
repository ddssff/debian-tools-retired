{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.PackageID
    ( PackageID(packageVersion, packageName)
    , prettyPackageID
    , makeBinaryPackageID
    , makeSourcePackageID
    ) where

import Control.Arrow (second)
import Data.Text (Text)
import Debian.Arch (Arch(..))
import qualified Debian.Control.Text as T
import qualified Debian.Relation as B -- ( PkgName, prettyPkgName, Relations, BinPkgName(..), SrcPkgName(..) )
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Release (Section(..))
import Debian.Repo.Types.PackageVersion (PackageVersion(..))
import Debian.Version (DebianVersion, prettyDebianVersion, parseDebianVersion)
import System.Posix.Types ( FileOffset )
import Text.PrettyPrint.ANSI.Leijen (Doc, text, (<>), Pretty(pretty))

-- | The 'PackageID' type fully identifies a package by name, version,
-- and a 'PackageIndex' which identifies the package's release,
-- component and architecture.

data PackageID n
    = PackageID
      { packageName :: n
      , packageVersion :: DebianVersion
      } deriving (Eq, Ord, Show)

prettyPackageID :: B.PkgName n => PackageID n -> Doc
prettyPackageID p = pretty (packageName p) <> text "-" <> prettyDebianVersion (packageVersion p)

makeBinaryPackageID :: String -> DebianVersion -> PackageID BinPkgName
makeBinaryPackageID n v = PackageID (BinPkgName n) v

makeSourcePackageID :: String -> DebianVersion -> PackageID SrcPkgName
makeSourcePackageID n v = PackageID (SrcPkgName n) v

instance PackageVersion (PackageID BinPkgName) where
    pkgName = packageName
    pkgVersion = packageVersion
