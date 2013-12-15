{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.PackageIndex
    ( PackageIndex(..)

    , BinaryPackage(..)
    , prettyBinaryPackage

    , SourcePackage(..)
    , SourceControl(..)
    , SourceFileSpec(..)
    ) where

import Control.Arrow (second)
import Data.Text (Text)
import Debian.Arch (Arch(..))
import qualified Debian.Control.Text as T
import qualified Debian.Relation as B -- ( PkgName, prettyPkgName, Relations, BinPkgName(..), SrcPkgName(..) )
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Release (Section(..))
import Debian.Repo.Types.PackageID (PackageID(packageVersion, packageName), prettyPackageID)
import Debian.Repo.Types.PackageVersion (PackageVersion(pkgName, pkgVersion))
import Debian.Version (DebianVersion, prettyDebianVersion, parseDebianVersion)
import System.Posix.Types ( FileOffset )
import Text.PrettyPrint.ANSI.Leijen (Doc, text, (<>), Pretty(pretty))

deriving instance Show (T.Field' Text)
deriving instance Ord (T.Field' Text)
deriving instance Show T.Paragraph
deriving instance Ord T.Paragraph

---------------- PACKAGES AND PACKAGE INDEXES -------------

-- |The PackageIndex type is the meta-information of a file containing
-- control information about debian packages, either source or binary.
-- Though the control information for a binary package does not
-- specify an architecture, the architecture here is that of the
-- environment where the package information is cached.
data PackageIndex
    = PackageIndex { packageIndexComponent :: Section
                   , packageIndexArch :: Arch
                   } deriving (Eq, Ord, Show)

{-
instance PackageVersion BinaryPackage where
    pkgName = binaryPackageName
    pkgVersion = packageVersion . packageID
-}

-- | The 'BinaryPackage' type adds to the 'PackageID' type the control
-- information obtained from the package index.
data BinaryPackage
    = BinaryPackage
      { packageID :: PackageID BinPkgName
      , packageInfo :: T.Paragraph
      , pDepends :: B.Relations
      , pPreDepends :: B.Relations
      , pConflicts ::B.Relations
      , pReplaces :: B.Relations
      , pProvides :: B.Relations
      }

instance Ord BinaryPackage where
    compare a b = compare (packageID a) (packageID b)

instance Eq BinaryPackage where
    a == b = (packageID a) == (packageID b)

instance PackageVersion BinaryPackage where
    pkgName = pkgName . packageID
    pkgVersion = pkgVersion . packageID

prettyBinaryPackage :: BinaryPackage -> Doc
prettyBinaryPackage = prettyPackageID . packageID

data SourcePackage
    = SourcePackage
      { sourcePackageID :: PackageID SrcPkgName
      , sourceParagraph :: T.Paragraph
      , sourceControl :: SourceControl
      , sourceDirectory :: String
      , sourcePackageFiles :: [SourceFileSpec]
      } deriving (Show, Eq, Ord)

-- |Source package information derived from the control paragraph.
data SourceControl
    = SourceControl
      { source :: Text
      , maintainer :: NameAddr
      , uploaders :: [NameAddr]
      , packageSection :: Maybe Section' -- Should this be the same type as the Section field in a .changes file?
      , packagePriority :: Maybe Priority
      , buildDepends :: [Package]
      , buildDependsIndep :: [Package]
      , buildConflicts :: [Package]
      , buildConflictsIndep :: [Package]
      , standardsVersion :: Maybe StandardsVersion -- There are packages that don't have this
      , homepage :: Maybe Text -- There are packages that don't have this
      } deriving (Show, Eq, Ord)

type NameAddr = Text
type StandardsVersion = Text
type Section' = Text
type Priority = Text
type Package = Text

data SourceFileSpec
    = SourceFileSpec
      { sourceFileMD5sum :: String
      , sourceFileSize :: FileOffset
      , sourceFileName :: FilePath
      }
    deriving (Show, Eq, Ord)
