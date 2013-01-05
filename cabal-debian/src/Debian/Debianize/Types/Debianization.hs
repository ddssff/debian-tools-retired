-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Debianization
    ( Debianization(..)
    , SourceDebDescription(..)
    , VersionControlSpec(..)
    , XField(..)
    , XFieldDest(..)
    , BinaryDebDescription(..)
    , PackageRelations(..)
    , DebAtom(..)
    , SourceDebAtom(..)
    , BinaryDebAtom(..)
    , DebType(..)
    -- * Atom set lookup functions
    , compiler
    , compilerVersion
    , compilerVersion'
    , noProfilingLibrary
    , noDocumentationLibrary
    ) where

import Data.Generics (Data, Typeable)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Set as Set (Set, maxView, toList, fromList, filter, null)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..))
import Debian.Orphans ()
import Debian.Orphans ()
import Debian.Policy (StandardsVersion, PackagePriority, PackageArchitectures, Section)
import Debian.Relation (Relations, SrcPkgName, BinPkgName)
import Distribution.License (License)
import Distribution.Simple.Compiler (Compiler)
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
      , srcAtoms :: Set SourceDebAtom
      -- ^ Information about the source package that will be transformed
      -- into values for the fields that represent the actual
      -- debianization files.
      , debAtoms :: Map BinPkgName (Set BinaryDebAtom)
      -- ^ Information about the binary packages that will be
      -- transformed into values for the fields that represent the
      -- actual debianization files.
      , atoms :: [DebAtom]
      -- ^ All the additional non-manditory debianization information.
      -- It is possible to construct a set with multiple conflicting
      -- values in this set, for example two different DebSourceFormat
      -- constructors, which makes it unclear what the resulting
      -- debianization should be.  Perhaps a clever Eq instance for
      -- DebAtom would help this situation.
      } deriving (Eq, Show)

data SourceDebAtom
    = NoDocumentationLibrary -- replaces haddock
    | NoProfilingLibrary     -- replaces debLibProf
    | Compiler Compiler
    | CompilerVersion Version
      -- ^ Specify the version number of the GHC compiler in the build
      -- environment.  The default is to assume that version is the same
      -- as the one in the environment where cabal-debian is running.
      -- This is used to look up hard coded lists of packages bundled
      -- with the compiler and their version numbers.  (This could
      -- certainly be done in a more beautiful way.)
    deriving (Eq, Ord, Show)

lookupAtom :: (Show a, Ord a) => (SourceDebAtom -> Maybe a) -> Debianization -> Maybe a
lookupAtom from deb = lookupAtom' from (srcAtoms deb)

lookupAtom' :: (Show a, Ord a) => (SourceDebAtom -> Maybe a) -> Set SourceDebAtom -> Maybe a
lookupAtom' from atoms =
    case maxView (lookupAtoms' from atoms) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

-- lookupAtoms :: (Show a, Ord a) => (SourceDebAtom -> Maybe a) -> Debianization -> Set a
-- lookupAtoms from deb = lookupAtoms' from (srcAtoms deb)

lookupAtoms' :: (Show a, Ord a) => (SourceDebAtom -> Maybe a) -> Set SourceDebAtom -> Set a
lookupAtoms' from atoms = setMapMaybe from atoms

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = fromList . mapMaybe p . toList

compiler :: Debianization -> Maybe Compiler
compiler deb =
    lookupAtom fromCompiler deb
    where fromCompiler (Compiler x) = Just x
          fromCompiler _ = Nothing

compilerVersion :: Debianization -> Maybe Version
compilerVersion deb =
    lookupAtom from deb
    where from (CompilerVersion x) = Just x
          from _ = Nothing

compilerVersion' :: Set SourceDebAtom -> Maybe Version
compilerVersion' atoms =
    lookupAtom' from atoms
    where from (CompilerVersion x) = Just x
          from _ = Nothing

noProfilingLibrary :: Debianization -> Bool
noProfilingLibrary deb =
    not . Set.null . Set.filter isNoProfilingLibrary . srcAtoms $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = True
      isNoProfilingLibrary _ = False

noDocumentationLibrary :: Debianization -> Bool
noDocumentationLibrary deb =
    not . Set.null . Set.filter isNoDocumentationLibrary . srcAtoms $ deb
    where
      isNoDocumentationLibrary NoDocumentationLibrary = True
      isNoDocumentationLibrary _ = False

data BinaryDebAtom
    = Depends BinPkgName     -- replaces extraDevDeps and binaryPackageDeps
    | Conflicts BinPkgName   -- replaces binaryPackageConflicts
    deriving (Eq, Ord, Show)

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
      , section :: Maybe Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
      , standardsVersion :: StandardsVersion
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
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

-- | The smallest pieces of debhelper information.  Some of these are
-- converted directly into files in the debian directory, others
-- become fragments of those files, and others are first converted
-- into different DebAtom values as new information becomes available.
data DebAtom
    = DebRulesFragment Text                       -- ^ A Fragment of debian/rules
    | DebSourceFormat Text                        -- ^ Write debian/source/format
    | DebWatch Text                               -- ^ Write debian/watch
    | DHIntermediate FilePath Text                -- ^ Put this text into a file with the given name in the debianization.

    | DHInstall BinPkgName FilePath FilePath      -- ^ Install a build file into the binary package
    | DHInstallTo BinPkgName FilePath FilePath    -- ^ Install a build file into the binary package at an exact location
    | DHInstallData BinPkgName FilePath FilePath  -- ^ DHInstallTo the package's data directory: /usr/share/package-version/
    | DHFile BinPkgName FilePath Text             -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec BinPkgName String FilePath -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo BinPkgName String FilePath -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir BinPkgName FilePath            -- ^ Create a directory in the binary package
    | DHInstallInit BinPkgName Text               -- ^ Add an init.d file to the binary package
    | DHInstallLogrotate BinPkgName Text          -- ^ Add a logrotate file to the binary package
    | DHLink BinPkgName FilePath FilePath         -- ^ Create a symbolic link in the binary package
    | DHPostInst BinPkgName Text                  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm BinPkgName Text                    -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst BinPkgName Text                   -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm BinPkgName Text                     -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
    | DHApacheSite BinPkgName String FilePath Text  -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    deriving (Eq, Ord, Show, Data, Typeable)

data DebType = Dev | Prof | Doc deriving (Eq, Read, Show)