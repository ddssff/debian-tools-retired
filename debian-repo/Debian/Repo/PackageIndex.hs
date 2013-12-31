{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module Debian.Repo.PackageIndex
    ( PackageIndex(..)

    , BinaryPackage(..)
    , prettyBinaryPackage

    , SourcePackage(..)
    , SourceControl(..)
    , SourceFileSpec(..)

    , packageIndexName
    , packageIndexPath
    , packageIndexDir
    , packageIndexPathList
    , packageIndexDirList
    , packageIndexList
    , sourceIndexList
    , binaryIndexList
    , releaseDir

    , sortSourcePackages
    , sortBinaryPackages
    ) where

import Data.List (sortBy)
import Data.Text (Text)
import Debian.Arch (Arch(..), prettyArch)
import qualified Debian.Control.Text as T
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import qualified Debian.Relation as B (Relations)
import Debian.Release (releaseName', Section(..), sectionName')
import Debian.Repo.PackageID (PackageID(packageName, packageVersion), prettyPackageID)
import Debian.Repo.Release (Release(..))
import System.Posix.Types (FileOffset)
import Text.PrettyPrint.ANSI.Leijen (Doc)

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

packageIndexName :: PackageIndex -> FilePath
packageIndexName index =
    case packageIndexArch index of
      Source -> "Sources"
      _ -> "Packages"

packageIndexPath :: Release -> PackageIndex -> FilePath
packageIndexPath release index = packageIndexDir release index ++ "/" ++ packageIndexName index

packageIndexDir :: Release -> PackageIndex -> FilePath
packageIndexDir release index =
    case packageIndexArch index of
      Source -> releaseDir release ++ "/" ++ sectionName' (packageIndexComponent index) ++ "/source"
      _ -> (releaseDir release ++ "/" ++
            sectionName' (packageIndexComponent index) ++
            -- Will prettyArch give us linux-amd64 when we just want amd64?
            "/binary-" ++ show (prettyArch (packageIndexArch index)))

releaseDir :: Release -> String
releaseDir release = "dists/" ++ (releaseName' . releaseName $ release)

packageIndexPathList :: Release -> [FilePath]
packageIndexPathList release = map (packageIndexPath release) . packageIndexList $ release

packageIndexDirList :: Release -> [FilePath]
packageIndexDirList release = map (packageIndexDir release) . packageIndexList $ release

packageIndexList :: Release -> [PackageIndex]
packageIndexList release = sourceIndexList release ++ binaryIndexList release

sourceIndexList :: Release -> [PackageIndex]
sourceIndexList release =
    map componentIndex (releaseComponents $ release)
    where componentIndex component = PackageIndex { packageIndexComponent = component
                                                  , packageIndexArch = Source }

binaryIndexList :: Release -> [PackageIndex]
binaryIndexList release =
    concat . map componentIndexes $ (releaseComponents release)
    where
      --componentIndexes :: Section -> [PackageIndex]
      componentIndexes component =
          map archIndex (filter (/= Source) (releaseArchitectures release))
          where
            --archIndex :: Arch -> PackageIndex
            archIndex arch = PackageIndex { packageIndexComponent = component
                                          , packageIndexArch = arch }

-- | Return a sorted list of available source packages, newest version first.
sortSourcePackages :: [SrcPkgName] -> [SourcePackage] -> [SourcePackage]
sortSourcePackages names pkgs =
    sortBy cmp . filterNames $ pkgs
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          filter (flip elem names . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

sortBinaryPackages :: [BinPkgName] -> [BinaryPackage] -> [BinaryPackage]
sortBinaryPackages names pkgs =
    sortBy cmp . filterNames $ pkgs
    where
      filterNames :: [BinaryPackage] -> [BinaryPackage]
      filterNames packages =
          filter (flip elem names . packageName . packageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . packageID $ p1
            v2 = packageVersion . packageID $ p2
