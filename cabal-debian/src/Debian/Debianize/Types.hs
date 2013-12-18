{-# LANGUAGE DeriveDataTypeable #-}
module Debian.Debianize.Types
    ( Top(..)
    -- * Modes of operation
    , verbosity
    , dryRun
    , debAction
    , cabalFlagAssignments

    -- * Cabal package info
    , packageDescription
    , compiler

    -- * Repository info
    , execMap
    , epochMap
    , missingDependencies
    , extraLibMap
    , debianNameMap

    -- * Source Package Info
    , sourcePackageName
    , revision
    , debVersion
    , maintainer
    , copyright
    , license
    , licenseFile
    , sourceArchitectures
    , sourcePriority
    , sourceSection
    , compat
    , sourceFormat
    , changelog
    , comments
    , standardsVersion
    , rulesHead
    , rulesFragments
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageNames
    , buildDir
    , watch

    -- * Source Package Build Dependencies
    , omitLTDeps
    , compilerVersion

    -- * Binary Package Info
    , binaryArchitectures
    , executable
    , serverInfo -- askServers = serverInfo
    , website
    , backups
    , apacheSite
    , extraDevDeps
    , postInst
    , postRm
    , preInst
    , preRm
    , binaryPriority
    , binarySection
    , installInit
    , packageType
    , debianDescription
    , essential

    , relations
    , depends
    , recommends
    , suggests
    , preDepends
    , breaks
    , conflicts
    , provides
    , replaces
    , builtUsing

{-
    -- * Binary Package Dependencies
    , depends
    , conflicts
    , replaces
    , provides
-}

    -- * Binary package Files
    , link
    , install
    , installTo
    , installData
    , file
    , installDir
    , logrotateStanza
    , installCabalExec
    , installCabalExecTo

    -- * Unknown, obsolete, internal
    , flags
    , validate
    , warning -- no-op?
    , intermediateFiles
    , packageInfo
    , control -- obsolete
    , source
    , changedBy
    , uploaders
    , dmUploadAllowed
    , homepage
    , vcsFields
    , xFields
    , buildDepends
    , buildConflicts
    , buildDependsIndep
    , buildConflictsIndep
    , binaryPackages
    ) where

import Control.Category ((.))
import Data.Generics (Typeable)
import Data.Lens.Lazy (Lens, lens, iso, getL)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog)
import Debian.Debianize.Prelude (maybeLens, listElemLens)
import Debian.Debianize.Types.Atoms
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.VersionSplits (VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat, StandardsVersion)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (CompilerId)
import Prelude hiding (init, init, log, log, unlines, (.))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | This is a special filepath that represents the top of a directory
-- tree.  For a cabal package this directory would contain the .cabal
-- file, for a debian package it would contain the debian directory.
newtype Top = Top {unTop :: FilePath} deriving (Eq, Ord, Show, Typeable)

binaryDebDescription :: BinPkgName -> Lens Atoms B.BinaryDebDescription
binaryDebDescription b = maybeLens (B.newBinaryDebDescription b) (iso id id) . listElemLens ((== b) . getL B.package) . S.binaryPackages . control

packageType :: BinPkgName -> Lens Atoms (Maybe B.PackageType)
packageType b = B.packageType . binaryDebDescription b

-- | Lens into the description field of a BinaryDebDescription.
debianDescription :: BinPkgName -> Lens Atoms (Maybe Text)
debianDescription b = B.description . binaryDebDescription b

essential :: BinPkgName -> Lens Atoms (Maybe Bool)
essential b = B.essential . binaryDebDescription b

relations :: BinPkgName -> Lens Atoms B.PackageRelations
relations b = B.relations . binaryDebDescription b

-- | The Depends: relations for each binary deb.
depends :: BinPkgName -> Lens Atoms Relations
depends b = B.depends . B.relations . binaryDebDescription b

-- | The Recommends: relations for each binary deb.
recommends :: BinPkgName -> Lens Atoms Relations
recommends b = B.recommends . B.relations . binaryDebDescription b

-- | The Suggests: relations for each binary deb.
suggests :: BinPkgName -> Lens Atoms Relations
suggests b = B.suggests . B.relations . binaryDebDescription b

-- | The Pre-Depends: relations for each binary deb.
preDepends :: BinPkgName -> Lens Atoms Relations
preDepends b = B.preDepends . B.relations . binaryDebDescription b

-- | The Breaks: relations for each binary deb.
breaks :: BinPkgName -> Lens Atoms Relations
breaks b = B.breaks . B.relations . binaryDebDescription b

-- | The Conflicts: relations for each binary deb.
conflicts :: BinPkgName -> Lens Atoms Relations
conflicts b = B.conflicts . B.relations . binaryDebDescription b

-- | The Provides: relations for each binary deb.
provides :: BinPkgName -> Lens Atoms Relations
provides b = B.provides . B.relations . binaryDebDescription b

-- | The Replaces: relations for each binary deb.
replaces :: BinPkgName -> Lens Atoms Relations
replaces b = B.replaces . B.relations . binaryDebDescription b

builtUsing :: BinPkgName -> Lens Atoms Relations
builtUsing b = B.builtUsing . B.relations . binaryDebDescription b

-- | Maintainer field.  Overrides any value found in the cabal file, or
-- in the DEBIANMAINTAINER environment variable.
maintainer :: Lens Atoms (Maybe NameAddr)
maintainer = S.maintainer . control

-- | The architectures supported by a binary package
binaryArchitectures :: BinPkgName -> Lens Atoms (Maybe PackageArchitectures)
binaryArchitectures b = B.architecture . binaryDebDescription b

-- | The source package priority
sourcePriority :: Lens Atoms (Maybe PackagePriority)
sourcePriority = S.priority . control

-- | Map of the binary package priorities (FIXME: redundant with BinaryDebDescription)
binaryPriority :: BinPkgName -> Lens Atoms (Maybe PackagePriority)
binaryPriority b = B.binaryPriority . binaryDebDescription b

-- | The source package's section assignment
sourceSection :: Lens Atoms (Maybe Section)
sourceSection = S.section . control

-- | Map of the binary deb section assignments (FIXME: redundant with BinaryDebDescription)
binarySection :: BinPkgName -> Lens Atoms (Maybe Section)
binarySection b = B.binarySection . binaryDebDescription b

-- * Debian dependency info

source :: Lens Atoms (Maybe SrcPkgName)
source = S.source . control

changedBy :: Lens Atoms (Maybe NameAddr)
changedBy = S.changedBy . control

uploaders :: Lens Atoms ([NameAddr])
uploaders = S.uploaders . control

dmUploadAllowed :: Lens Atoms (Bool)
dmUploadAllowed = S.dmUploadAllowed . control

-- | The @Standards-Version@ field of the @debian/control@ file
standardsVersion :: Lens Atoms (Maybe StandardsVersion)
standardsVersion = S.standardsVersion . control

homepage :: Lens Atoms (Maybe Text)
homepage = S.homepage . control

vcsFields :: Lens Atoms (Set S.VersionControlSpec)
vcsFields = S.vcsFields . control

xFields :: Lens Atoms (Set S.XField)
xFields = S.xFields . control

buildDepends :: Lens Atoms Relations
buildDepends = S.buildDepends . control

buildDependsIndep :: Lens Atoms Relations
buildDependsIndep = S.buildDependsIndep . control

buildConflicts :: Lens Atoms Relations
buildConflicts = S.buildConflicts . control

buildConflictsIndep :: Lens Atoms Relations
buildConflictsIndep = S.buildConflictsIndep . control

binaryPackages :: Lens Atoms [B.BinaryDebDescription]
binaryPackages = S.binaryPackages . control
