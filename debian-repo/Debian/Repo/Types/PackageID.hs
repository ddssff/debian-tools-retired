{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.PackageID
    ( PackageID(PackageID, packageVersion, packageName)
    , prettyPackageID
    , makeBinaryPackageID
    , makeSourcePackageID
    ) where

import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import qualified Debian.Relation as B (PkgName)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)

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
