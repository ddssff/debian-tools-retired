-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.DebControl
    ( SourceDebDescription(..)
    , newSourceDebDescription
    , VersionControlSpec(..)
    , XField(..)
    , XFieldDest(..)
    , BinaryDebDescription(..)
    , newBinaryDebDescription
    , modifyBinaryDeb
    -- , modifyBinaryDescription
    , PackageRelations(..)
    , packageArch
    ) where

import Data.Generics (Data, Typeable)
import Data.Monoid (mempty)
import Data.Set as Set (Set, empty)
import Data.Text (Text)
import Debian.Debianize.Types.PackageType (PackageType(..))
import Debian.Orphans ()
import Debian.Policy (StandardsVersion, PackagePriority, PackageArchitectures(..), Section)
import Debian.Relation (Relations, SrcPkgName(..), BinPkgName)
import Prelude hiding (init, log)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | This type represents the debian/control file, which is the core
-- of the source package debianization.  It includes the information
-- that goes in the first, or source, section, and then a list of the
-- succeeding binary package sections.
data SourceDebDescription
    = SourceDebDescription
      { source :: SrcPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Source>
      , maintainer :: NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Maintainer>
      , changedBy :: Maybe NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Changed-By>
      , uploaders :: [NameAddr]
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
      , dmUploadAllowed :: Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-DM-Upload-Allowed>
      , priority :: Maybe PackagePriority
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Priority>
      , section :: Maybe Section
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
      , standardsVersion :: StandardsVersion
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
      , homepage :: Maybe Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Homepage>
      , vcsFields :: Set VersionControlSpec
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-VCS-fields>
      , xFields :: Set XField
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.7>
      , buildDepends :: Relations
      , buildConflicts :: Relations
      , buildDependsIndep :: Relations
      , buildConflictsIndep :: Relations
      , binaryPackages :: [BinaryDebDescription] -- This should perhaps be a set, or a map
      } deriving (Eq, Show, Data, Typeable)

newSourceDebDescription :: SrcPkgName -> NameAddr -> StandardsVersion -> SourceDebDescription
newSourceDebDescription src who standards =
    SourceDebDescription
      { source = src
      , maintainer = who
      , changedBy = Nothing
      , uploaders = []
      , dmUploadAllowed = False
      , priority = Nothing
      , section = Nothing
      , buildDepends = []
      , buildConflicts = []
      , buildDependsIndep  = []
      , buildConflictsIndep  = []
      , standardsVersion = standards
      , homepage = Nothing
      , vcsFields = Set.empty
      , xFields = Set.empty
      , binaryPackages = [] }

data VersionControlSpec
    = VCSBrowser Text
    | VCSArch Text
    | VCSBzr Text
    | VCSCvs Text
    | VCSDarcs Text
    | VCSGit Text
    | VCSHg Text
    | VCSMtn Text
    | VCSSvn Text
    deriving (Eq, Ord, Show, Data, Typeable)

-- | User defined fields.  Parse the line "XBS-Comment: I stand
-- between the candle and the star." to get XField (fromList "BS")
-- "Comment" " I stand between the candle and the star."
data XField
    = XField (Set XFieldDest) Text Text
    deriving (Eq, Ord, Show, Data, Typeable)

data XFieldDest
    = B -- ^ Field will be copied to the binary packgae control files
    | S -- ^ Field will be copied to the source packgae control files
    | C -- ^ Field will be copied to the upload control (.changes) file
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | This type represents a section of the control file other than the
-- first, which in turn represent one of the binary packages or debs
-- produced by this debianization.
data BinaryDebDescription
    = BinaryDebDescription
      { package :: BinPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
      , architecture :: PackageArchitectures
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Architecture>
      , binarySection :: Maybe Section
      , binaryPriority :: Maybe PackagePriority
      , essential :: Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Essential>
      , description :: Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Description>
      , relations :: PackageRelations
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
      } deriving (Read, Eq, Show, Data, Typeable)

newBinaryDebDescription :: BinPkgName -> PackageArchitectures -> BinaryDebDescription
newBinaryDebDescription name arch =
    BinaryDebDescription
      { package = name -- mkPkgName base typ
      , architecture = arch -- packageArch typ
      , binarySection = Nothing
      , binaryPriority = Nothing
      , essential = False
      , description = mempty
      , relations = newPackageRelations }

-- | Modify the description of one of the binary debs without changing
-- the package order.
modifyBinaryDeb :: BinPkgName -> (Maybe BinaryDebDescription -> BinaryDebDescription) -> SourceDebDescription -> SourceDebDescription
modifyBinaryDeb bin f deb =
    deb {binaryPackages = bins'}
    where
      bins' = if any (\ x -> package x == bin) bins
             then map g (binaryPackages deb)
             else binaryPackages deb ++ [f Nothing]
      g x = if package x == bin then f (Just x) else x
      bins = binaryPackages deb

-- ^ Package interrelationship information.
data PackageRelations
    = PackageRelations
      { depends :: Relations
      , recommends :: Relations
      , suggests :: Relations
      , preDepends :: Relations
      , breaks :: Relations
      , conflicts :: Relations
      , provides :: Relations
      , replaces :: Relations
      , builtUsing :: Relations
      } deriving (Read, Eq, Show, Data, Typeable)

newPackageRelations :: PackageRelations
newPackageRelations =
    PackageRelations
      { depends = []
      , recommends = []
      , suggests = []
      , preDepends = []
      , breaks = []
      , conflicts = []
      , provides = []
      , replaces = []
      , builtUsing = [] }

packageArch :: PackageType -> PackageArchitectures
packageArch Development = Any
packageArch Profiling = Any
packageArch Documentation = All
packageArch Utilities = All
packageArch Exec = Any
packageArch Cabal = undefined
packageArch Source' = undefined
