{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Facts.Lenses
    (
    -- * Modes of operation
      verbosity
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
    , sourceArchitecture
    , sourcePriority
    , sourceSection
    , compat
    , sourceFormat
    , changelog
    , comments
    , standards
    , rulesHead
    , rulesFragments
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageNames
    , buildDir
    , watch

    -- * Source Package Build Dependencies
    , buildDeps
    , buildDepsIndep
    , omitLTDeps
    , compilerVersion

    -- * Binary Package Info
    , binaryArchitectures
    , description
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
    , binaryPriorities
    , binarySections
    , installInit

    -- * Binary Package Dependencies
    , depends
    , conflicts
    , replaces
    , provides

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
    ) where

import Control.Category ((.))
import Data.Lens.Lazy (lens, Lens)
import Data.Map as Map (Map)
import Data.Set as Set (Set)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog)
import Debian.Debianize.Facts.Types hiding (conflicts, depends, description, maintainer)
import Debian.Debianize.VersionSplits (VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat, StandardsVersion)
import Debian.Relation (BinPkgName, Relation(..), Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (CompilerId)
import Prelude hiding (init, log, unlines, (.))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- Lenses to access values in the Atoms type.  This is an old
-- design which I plan to make private and turn into something
-- nicer, so these will remain ugly and repetitive for now.

-- | Set how much progress messages get generated.
verbosity :: Lens Atoms Int
verbosity = lens verbosity_ (\ b a -> a {verbosity_ = b}) . flags

-- | Don't write anything, just output a description of what would have happened
dryRun :: Lens Atoms Bool
dryRun = lens dryRun_ (\ b a -> a {dryRun_ = b}) . flags

-- | Make sure the version number and package names of the supplied
-- and generated debianizations match.
validate :: Lens Atoms Bool
validate = lens validate_ (\ b a -> a {validate_ = b}) . flags

-- | Debianize, SubstVars, or Usage.  I'm no longer sure what SubstVars does, but someone
-- may still be using it.
debAction :: Lens Atoms DebAction
debAction = lens debAction_ (\ b a -> a {debAction_ = b}) . flags

-- | Obsolete record containing verbosity, dryRun, validate, and debAction.
flags :: Lens Atoms Flags
flags = lens flags_ (\ b a -> a {flags_ = b})

-- | Unused
warning :: Lens Atoms (Set Text)
warning = lens warning_ (\ a b -> b {warning_ = a})

-- | Set the compiler version, this is used when loading the cabal file to
compilerVersion :: Lens Atoms (Maybe Version)
compilerVersion = lens compilerVersion_ (\ b a -> a {compilerVersion_ = b})

-- | The build directory.  This can be set by an argument to the @Setup@ script.
-- When @Setup@ is run manually it is just @dist@, when it is run by
-- @dpkg-buildpackage@ the compiler name is appended, so it is typically
-- @dist-ghc@.  Cabal-debian needs the correct value of buildDir to find
-- the build results.
buildDir :: Lens Atoms (Set FilePath)
buildDir = lens buildDir_ (\ b a -> a {buildDir_ = b})

-- | Map from cabal Extra-Lib names to debian binary package names.
extraLibMap :: Lens Atoms (Map String (Set Relations))
extraLibMap = lens extraLibMap_ (\ a b -> b {extraLibMap_ = a})

-- | Map from cabal Build-Tool names to debian binary package names.
execMap :: Lens Atoms (Map String Relations)
execMap = lens execMap_ (\ a b -> b {execMap_ = a})

-- | Cabal flag assignments to use when loading the cabal file.
cabalFlagAssignments :: Lens Atoms (Set (FlagName, Bool))
cabalFlagAssignments = lens cabalFlagAssignments_ (\ a b -> b {cabalFlagAssignments_ = a})

-- | The result of loading a .cabal file
packageDescription :: Lens Atoms (Maybe PackageDescription)
packageDescription = lens packageDescription_ (\ a b -> b {packageDescription_ = a})

-- | Another result of loading a .cabal file
compiler :: Lens Atoms (Maybe CompilerId)
compiler = lens compiler_ (\ a b -> b {compiler_ = a})

-- | Map from cabal version number ranges to debian package names.  This is a
-- result of the fact that only one version of a debian package can be
-- installed at a given time, while multiple versions of a cabal packages can.
debianNameMap :: Lens Atoms (Map PackageName VersionSplits)
debianNameMap = lens debianNameMap_ (\ a b -> b {debianNameMap_ = a})

-- | Map of Debian epoch numbers assigned to cabal packages.
epochMap :: Lens Atoms (Map PackageName Int)
epochMap = lens epochMap_ (\ a b -> b {epochMap_ = a})

-- | Map of binary deb descriptions.
description :: Lens Atoms (Map BinPkgName Text)
description = lens description_ (\ a b -> b {description_ = a})

-- | Create a package to hold a cabal executable
executable :: Lens Atoms (Map BinPkgName InstallFile)
executable = lens executable_ (\ a b -> b {executable_ = a})

-- | Create a package for an operating service using the given executable
serverInfo :: Lens Atoms (Map BinPkgName Server)
serverInfo = lens serverInfo_ (\ a b -> b {serverInfo_ = a})

-- | Create a package for a website using the given executable as the server
website :: Lens Atoms (Map BinPkgName Site)
website = lens website_ (\ a b -> b {website_ = a})

-- | Generate a backups package using the given cabal executable
backups :: Lens Atoms (Map BinPkgName String)
backups = lens backups_ (\ a b -> b {backups_ = a})

-- | Create an apache configuration file with the given
-- (domain, logdir, filetext).  This is called when expanding
-- the result of the website lens above.
apacheSite :: Lens Atoms (Map BinPkgName (String, FilePath, Text))
apacheSite = lens apacheSite_ (\ a b -> b {apacheSite_ = a})

-- * Lower level hints about the debianization


-- | List if packages that should be omitted from any
-- dependency list - e.g. a profiling package missing due
-- to use of noProfilingPackage lens elsewhere.
missingDependencies :: Lens Atoms (Set BinPkgName)
missingDependencies = lens missingDependencies_ (\ a b -> b {missingDependencies_ = a})

-- | Override the package name used to hold left over data files and executables.
-- Usually only one package is specified, but if more then one are they will each
-- receive the same list of files.
utilsPackageNames :: Lens Atoms (Set BinPkgName)
utilsPackageNames = lens utilsPackageNames_ (\ a b -> b {utilsPackageNames_ = a})

-- | Override the debian source package name constructed from the cabal name
sourcePackageName :: Lens Atoms (Maybe SrcPkgName)
sourcePackageName = lens sourcePackageName_ (\ a b -> b {sourcePackageName_ = a})

-- | Revision string used in constructing the debian verison number from the cabal version
revision :: Lens Atoms (Maybe String)
revision = lens revision_ (\ a b -> b {revision_ = a})

-- | Exact debian version number, overrides the version generated from the cabal version
debVersion :: Lens Atoms (Maybe DebianVersion)
debVersion = lens debVersion_ (\ b a -> a {debVersion_ = b})

-- | Maintainer field.  Overrides any value found in the cabal file, or
-- in the DEBIANMAINTAINER environment variable.
maintainer :: Lens Atoms (Maybe NameAddr)
maintainer = lens maintainer_ (\ b a -> a {maintainer_ = b})

-- | No longer sure what the purpose of this lens is.
packageInfo :: Lens Atoms (Map PackageName PackageInfo)
packageInfo = lens packageInfo_ (\ a b -> b {packageInfo_ = a})

-- | Set this to filter any less-than dependencies out of the generated debian
-- dependencies.  (Not sure if this is implemented.)
omitLTDeps :: Lens Atoms (Set Bool)
omitLTDeps = lens omitLTDeps_ (\ b a -> a {omitLTDeps_ = b})

-- | Set this to omit the prof library deb.
noProfilingLibrary :: Lens Atoms (Set Bool)
noProfilingLibrary = lens noProfilingLibrary_ (\ b a -> a {noProfilingLibrary_ = b})

-- | Set this to omit the doc library deb.
noDocumentationLibrary :: Lens Atoms (Set Bool)
noDocumentationLibrary = lens noDocumentationLibrary_ (\ b a -> a {noDocumentationLibrary_ = b})

-- | The copyright information from the cabal file
copyright :: Lens Atoms (Maybe Text)
copyright = lens copyright_ (\ a b -> b {copyright_ = a})

-- | The license information from the cabal file
license :: Lens Atoms (Maybe License)
license = lens license_ (\ a b -> b {license_ = a})

-- | The license information from the cabal file
licenseFile :: Lens Atoms (Maybe Text)
licenseFile = lens licenseFile_ (\ a b -> b {licenseFile_ = a})

-- | The source package architecture - @Any@, @All@, or some list of specific architectures.
sourceArchitecture :: Lens Atoms (Maybe PackageArchitectures)
sourceArchitecture = lens sourceArchitecture_ (\ a b -> b {sourceArchitecture_ = a})

-- | Map of the binary package architectures
binaryArchitectures :: Lens Atoms (Map BinPkgName PackageArchitectures)
binaryArchitectures = lens binaryArchitectures_ (\ a b -> b {binaryArchitectures_ = a})

-- | The source package priority
sourcePriority :: Lens Atoms (Maybe PackagePriority)
sourcePriority = lens sourcePriority_ (\ a b -> b {sourcePriority_ = a})
 
-- | Map of the binary package priorities
binaryPriorities :: Lens Atoms (Map BinPkgName PackagePriority)
binaryPriorities = lens binaryPriorities_ (\ a b -> b {binaryPriorities_ = a})

-- | The source package's section assignment
sourceSection :: Lens Atoms (Maybe Section)
sourceSection = lens sourceSection_ (\ a b -> b {sourceSection_ = a})

-- | Map of the binary deb section assignments
binarySections :: Lens Atoms (Map BinPkgName Section)
binarySections = lens binarySections_ (\ a b -> b {binarySections_ = a})

-- * Debian dependency info

-- | Build dependencies.  FIXME: This should be a Set (Set Relation)
-- so we can build or relations, right now we just assume that each
-- Relation is a singleton set.
buildDeps :: Lens Atoms (Set Relations)
buildDeps = lens buildDeps_ (\ a b -> b {buildDeps_ = a})

-- | Architecture independent
buildDepsIndep :: Lens Atoms (Set Relations)
buildDepsIndep = lens buildDepsIndep_ (\ a b -> b {buildDepsIndep_ = a})

-- | Map of extra install dependencies for the package's binary debs.
-- This should be [[Relation]] for full generality, or Set (Set Relation)
depends :: Lens Atoms (Map BinPkgName (Set Relation))
depends = lens depends_ (\ a b -> b {depends_ = a})

-- | Map of extra install conflicts for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
conflicts :: Lens Atoms (Map BinPkgName (Set Relation))
conflicts = lens conflicts_ (\ a b -> b {conflicts_ = a})

-- | Map of extra install replaces for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
replaces :: Lens Atoms (Map BinPkgName (Set Relation))
replaces = lens replacesMap_ (\ a b -> b {replacesMap_ = a})

-- | Map of extra install provides for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
provides :: Lens Atoms (Map BinPkgName (Set Relation))
provides = lens providesMap_ (\ a b -> b {providesMap_ = a})

-- | Extra install dependencies for the devel library.  Redundant
-- with depends, but kept for backwards compatibility.  Also, I
-- think maybe this is or was needed because it can be set before
-- the exact name of the library package is known.
extraDevDeps :: Lens Atoms (Set Relation)
extraDevDeps = lens extraDevDeps_ (\ a b -> b {extraDevDeps_ = a})

-- | The beginning of the rules file
rulesHead :: Lens Atoms (Maybe Text)
rulesHead = lens rulesHead_ (\ a b -> b {rulesHead_ = a})

-- | Additional fragments of the rules file
rulesFragments :: Lens Atoms (Set Text)
rulesFragments = lens rulesFragments_ (\ a b -> b {rulesFragments_ = a})

-- | Map of @debian/postinst@ scripts
postInst :: Lens Atoms (Map BinPkgName Text)
postInst = lens postInst_ (\ a b -> b {postInst_ = a})

-- | Map of @debian/postrm@ scripts
postRm :: Lens Atoms (Map BinPkgName Text)
postRm = lens postRm_ (\ a b -> b {postRm_ = a})

-- | Map of @debian/preinst@ scripts
preInst :: Lens Atoms (Map BinPkgName Text)
preInst = lens preInst_ (\ a b -> b {preInst_ = a})

-- | Map of @debian/prerm@ scripts
preRm :: Lens Atoms (Map BinPkgName Text)
preRm = lens preRm_ (\ a b -> b {preRm_ = a})

-- | The @debian/compat@ file, contains the minimum compatible version of the @debhelper@ package
compat :: Lens Atoms (Maybe Int)
compat = lens compat_ (\ a b -> b {compat_ = a})

-- | The @debian/source/format@ file.
sourceFormat :: Lens Atoms (Maybe SourceFormat)
sourceFormat = lens sourceFormat_ (\ a b -> b {sourceFormat_ = a})

-- | the @debian/watch@ file
watch :: Lens Atoms (Maybe Text)
watch = lens watch_ (\ a b -> b {watch_ = a})

-- | the @debian/changelog@ file
changelog :: Lens Atoms (Maybe ChangeLog)
changelog = lens changelog_ (\ a b -> b {changelog_ = a})

-- | Comment entries for the latest changelog entry (DebLogComments [[Text]])
comments :: Lens Atoms (Maybe [[Text]])
comments = lens comments_ (\ a b -> b {comments_ = a})

-- | The @debian/control@ file.
control :: Lens Atoms SourceDebDescription
control = lens control_ (\ a b -> b {control_ = a})

-- | The @Standards-Version@ field of the @debian/control@ file
standards :: Lens Atoms (Maybe StandardsVersion)
standards = lens standardsVersion (\ b a -> a {standardsVersion = b}) . control

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens Atoms (Map BinPkgName (Set Text))
logrotateStanza = lens logrotateStanza_ (\ a b -> b {logrotateStanza_ = a})

-- | Add entries to a binary deb's debian/foo.links file.
link :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
link = lens link_ (\ a b -> b {link_ = a})

-- | Install files into directories by adding entries to the binary
-- deb's debian/foo.install file.
install :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
install = lens install_ (\ a b -> b {install_ = a})

-- | Rename and install files.  This is done by adding rules to debian/rules.
installTo :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installTo = lens installTo_ (\ a b -> b {installTo_ = a})

-- | Install files into the a binary deb's data directory,
-- /usr/share/packagename-version.  This expands to either an install
-- or an installTo.
installData :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installData = lens installData_ (\ a b -> b {installData_ = a})

-- | Create a file in the binary deb with the given text.  This is done by
-- writing the file into the cabalInstall directory and adding an entry
-- to the binary deb's .install file.
file :: Lens Atoms (Map BinPkgName (Set (FilePath, Text)))
file = lens file_ (\ a b -> b {file_ = a})

-- | Install a cabal executable into a binary deb.
installCabalExec :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExec = lens installCabalExec_ (\ a b -> b {installCabalExec_ = a})

-- | Rename and install a cabal executable
installCabalExecTo :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExecTo = lens installCabalExecTo_ (\ a b -> b {installCabalExecTo_ = a})

-- | Create directories in the package
installDir :: Lens Atoms (Map BinPkgName (Set FilePath))
installDir = lens installDir_ (\ a b -> b {installDir_ = a})

-- | Create an /etc/init.d file in the package
installInit :: Lens Atoms (Map BinPkgName Text)
installInit = lens installInit_ (\ a b -> b {installInit_ = a})

-- | Create a file in the debianization.  This is used to implement the file lens above.
intermediateFiles :: Lens Atoms (Set (FilePath, Text))
intermediateFiles = lens intermediateFiles_ (\ a b -> b {intermediateFiles_ = a})
