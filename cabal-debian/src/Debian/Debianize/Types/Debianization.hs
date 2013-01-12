-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Debianization
    ( Debianization(..)
    , newDebianization
    , SourceDebDescription(..)
    , newSourceDebDescription
    , VersionControlSpec(..)
    , XField(..)
    , XFieldDest(..)
    , BinaryDebDescription(..)
    , newBinaryDebDescription
    , PackageRelations(..)
    , packageArch
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map)
import Data.Monoid (mempty)
import Data.Set as Set (Set, empty)
import Data.Text (Text, pack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Types.Atoms (DebAtomKey, DebAtom, HasAtoms(..))
import Debian.Debianize.Types.PackageType (PackageType(..))
import Debian.Orphans ()
import Debian.Policy (StandardsVersion, PackagePriority, PackageArchitectures(..), Section, parseMaintainer)
import Debian.Relation (Relations, SrcPkgName(..), BinPkgName)
import Distribution.License (License)
import Prelude hiding (init)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | The full debianization.
data Debianization
    = Debianization
      { sourceDebDescription :: SourceDebDescription
      -- ^ Represents the debian/control file -
      -- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-sourcecontrolfiles>
      , changelog :: ChangeLog
      -- ^ The first entry of the changelog determine's the package's
      -- name and version number, along with several other pieces of
      -- information.
      , rulesHead :: Text
      -- ^ The beginning of the debian/rules file.  The remainder is
      -- assembled from DebRulesFragment values in the atom list.
      , compat :: Int
      -- ^ The debhelper compatibility level from debian/compat.
      , copyright :: Either License Text
      -- ^ Copyright information, either as a Cabal License value or
      -- the full text.
      , debAtoms :: Map DebAtomKey (Set DebAtom)
      -- ^ Information about the source and binary packages that will
      -- be transformed into values for the fields that represent the
      -- actual debianization files.  Binary values are associated
      -- with a BinPkgName, the Nothing entries in the map represent
      -- source values.
      -- , atoms :: [DebAtom]
      -- ^ All the additional non-manditory debianization information.
      -- It is possible to construct a set with multiple conflicting
      -- values in this set, for example two different DebSourceFormat
      -- constructors, which makes it unclear what the resulting
      -- debianization should be.  Perhaps a clever Eq instance for
      -- DebAtom would help this situation.
      } deriving (Eq, Show)

instance HasAtoms Debianization where
    getAtoms = debAtoms
    putAtoms ats x = x {debAtoms = ats}

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLogEntry -> Either License Text -> Int -> StandardsVersion -> Debianization
newDebianization (WhiteSpace {}) _ _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization entry@(Entry {}) copy level standards =
    Debianization
      { sourceDebDescription = newSourceDebDescription (SrcPkgName (logPackage entry)) (either error id (parseMaintainer (logWho entry))) standards
      , changelog = ChangeLog [entry]
      , rulesHead = pack $ unlines [ "#!/usr/bin/make -f"
                                   , ""
                                   , "DEB_CABAL_PACKAGE = " ++ logPackage entry
                                   , ""
                                   , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                   , "include /usr/share/cdbs/1/class/hlibrary.mk"
                                   , "" ]
      , compat = level
      , copyright = copy
      , debAtoms = mempty }

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
      , binaryPackages :: [BinaryDebDescription]
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
