{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Facts.Lenses
    (
    -- * Location of unpacked cabal package
      top

    -- * Modes of operation
    , verbosity
    , dryRun
    , debAction
    , cabalFlagAssignments

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
import Data.Map as Map (empty, foldWithKey, insertWith, Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set as Set (empty, fold, insert, maxView, Set, singleton, union)
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
import Distribution.PackageDescription as Cabal (FlagName)
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

-- Build a value with a default
getter1 :: forall a. (Eq a) => a -> (DebAtom -> Maybe a) -> Atoms -> a
getter1 def un atoms = fromMaybe def $ getter6 un atoms

setter1 :: forall a. Ord a => (DebAtomKey -> DebAtom -> Maybe a) -> (a -> DebAtom) -> a -> Atoms -> Atoms
setter1 un mk x atoms = modifyAtoms' un (const ((singleton . (Source,) . mk) x)) atoms

-- Build a maybe value
getter6 :: forall a. (Eq a) => (DebAtom -> Maybe a) -> Atoms -> Maybe a
getter6 un atoms = foldAtoms from Nothing atoms
    where
      from Source y (Just x) | maybe False (/= x) (un y) = error "Conflicting values"
      from Source y z = maybe z Just (un y)
      from _ _ x = x

setter6 :: Ord a => (DebAtomKey -> DebAtom -> Maybe a) -> (a -> DebAtom) -> Maybe a -> Atoms -> Atoms
setter6 un mk x atoms = modifyAtoms' un (const (maybe Set.empty (singleton . (Source,) . mk) x)) atoms

-- Build a set
getter2 :: Ord a => (DebAtom -> Maybe a) -> Atoms -> Set a
getter2 un atoms = foldAtoms from Set.empty atoms
    where
      from Source t x = maybe x (`Set.insert` x) (un t)
      from _ _ x = x

-- Insert set elements
setter2 :: Ord a => (DebAtomKey -> DebAtom -> Bool) -> (a -> DebAtom) -> Set a -> Atoms -> Atoms
setter2 p mk x atoms = Set.fold (\ text atoms' -> insertAtom Source (mk text) atoms') (deleteAtoms p atoms) x

-- Build a set from many sets
getter4 :: Ord a => (DebAtom -> Maybe (Set a)) -> Atoms -> Set a
getter4 un atoms = foldAtoms from Set.empty atoms
    where
      from Source t x = maybe x (union x) (un t)
      from _ _ x = x

setter4 :: (DebAtomKey -> DebAtom -> Bool) -> (a -> DebAtom) -> a -> Atoms -> Atoms
setter4 p mk x atoms = insertAtom Source (mk x) (deleteAtoms p atoms)

-- Build a map, error on conflicting values
getter5 :: forall a b. (Ord a) => (DebAtom -> Maybe (a, b)) -> Atoms -> Map a b
getter5 un atoms = foldAtoms from Map.empty atoms
    where
      from :: DebAtomKey -> DebAtom -> Map a b -> Map a b
      from Source y x = maybe x (\ (a, b) -> Map.insertWith (error "Conflict in map") a b x) (un y)
      from _ _ x = x

setter5 :: (DebAtomKey -> DebAtom -> Bool) -> (a -> b -> DebAtom) -> Map a b -> Atoms -> Atoms
setter5 p mk x atoms = Map.foldWithKey (\ cabal debian atoms' -> insertAtom Source (mk cabal debian) atoms') (deleteAtoms p atoms) x


-- Build a binary map, error on conflicting values
getter7 :: forall b. (DebAtom -> Maybe b) -> Atoms -> Map BinPkgName b
getter7 un atoms = foldAtoms from Map.empty atoms
    where
      from :: DebAtomKey -> DebAtom -> Map BinPkgName b -> Map BinPkgName b
      from (Binary b) y x = maybe x (\ v -> Map.insertWith (error "Conflict in map") b v x) (un y)
      from _ _ x = x

setter7 :: (DebAtomKey -> DebAtom -> Bool) -> (a -> DebAtom) -> Map BinPkgName a -> Atoms -> Atoms
setter7 p mk x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (mk y) atoms') (deleteAtoms p atoms) x

-- Build a map of sets
getter3 :: forall a b. (Ord a, Ord b) => (DebAtom -> Maybe (a, b)) -> Atoms -> Map a (Set b)
getter3 un atoms = foldAtoms from Map.empty atoms
    where
      from :: DebAtomKey -> DebAtom -> Map a (Set b) -> Map a (Set b)
      from Source y x = maybe x (\ (a, b) -> Map.insertWith union a (singleton b) x) (un y)
      from _ _ x = x


setter3 :: (a ~ String, b ~ Relations) => (DebAtomKey -> DebAtom -> Bool) -> (a -> b -> DebAtom) ->  Map a (Set b) -> Atoms -> Atoms
setter3 p mk x atoms = Map.foldWithKey (\ k a atoms' -> Set.fold (\ debian' atoms'' -> insertAtom Source (mk k debian') atoms'') atoms' a) (deleteAtoms p atoms) x

-- Build a map of sets
getter8 :: forall b. (Ord b) => (DebAtom -> Maybe b) -> Atoms -> Map BinPkgName (Set b)
getter8 un atoms = foldAtoms from Map.empty atoms
    where
      from :: DebAtomKey -> DebAtom -> Map BinPkgName (Set b) -> Map BinPkgName (Set b)
      from (Binary b) y x = maybe x (\ v -> Map.insertWith union b (singleton v) x) (un y)
      from _ _ x = x

setter8 :: (DebAtomKey -> DebAtom -> Bool) -> (a -> DebAtom) -> Map BinPkgName (Set a) -> Atoms -> Atoms
setter8 p mk x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (mk rel) atoms'') atoms' rels) (deleteAtoms p atoms) x

-- | Obsolete record containing verbosity, dryRun, validate, and debAction.
flags :: Lens Atoms Flags
flags = lens g s
    where
      g :: Atoms -> Flags
      g = getter1 defaultFlags (\ y -> case y of DHFlags x -> Just x; _ -> Nothing)
      s = setter1 (\ k y -> case (k, y) of (Source, DHFlags x) -> Just x; _ -> Nothing) DHFlags

-- | Unused
warning :: Lens Atoms (Set Text)
warning = lens g s
    where
      g = getter2 (\ y -> case y of Warning t -> Just t; _ -> Nothing)
      s = setter2 (\ _ x -> case x of Warning _ -> True; _ -> False) Warning

-- | Set the compiler version, this is used when loading the cabal file to
compilerVersion :: Lens Atoms (Set Version)
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
extraLibMap = lens g s
    where
      g = getter3 (\ y -> case y of ExtraLibMapping cabal debian -> Just (cabal, debian); _ -> Nothing)
      s = setter3 (\ k a -> case (k, a) of (Source, ExtraLibMapping _ _) -> True; _ -> False) ExtraLibMapping

-- | Map from cabal Build-Tool names to debian binary package names.
execMap :: Lens Atoms (Map String Relations)
execMap = lens g s
    where
      g :: Atoms -> Map String Relations
      g = getter5 (\ y -> case y of ExecMapping cabal debian -> Just (cabal, debian); _ -> Nothing)
      s :: Map String Relations -> Atoms -> Atoms
      s = setter5 (\ k y -> case (k, y) of (Source, ExecMapping _ _) -> True; _ -> False) ExecMapping

-- | Cabal flag assignments to use when loading the cabal file.
cabalFlagAssignments :: Lens Atoms (Set (FlagName, Bool))
cabalFlagAssignments = lens g s
    where
      g = getter4 (\ y -> case y of DHCabalFlagAssignments pairs -> Just pairs; _ -> Nothing)
      s = setter4 (\ k y -> case (k, y) of (Source, DHCabalFlagAssignments _) -> True; _ -> False) DHCabalFlagAssignments

-- | Map from cabal version number ranges to debian package names.  This is a
-- result of the fact that only one version of a debian package can be
-- installed at a given time, while multiple versions of a cabal packages can.
debianNameMap :: Lens Atoms (Map PackageName VersionSplits)
debianNameMap = lens g s
    where
      g = getter1 mempty (\ y -> case y of DebianNameMap mp -> Just mp; _ -> Nothing)
      s = setter1 (\ k y -> case (k, y) of (Source, DebianNameMap x) -> Just x; _ -> Nothing) DebianNameMap

-- | Map of Debian epoch numbers assigned to cabal packages.
epochMap :: Lens Atoms (Map PackageName Int)
epochMap = lens g s
    where
      g = getter5 (\ y -> case y of EpochMapping name epoch -> Just (name, epoch); _ -> Nothing)
      s = setter5 (\ k y -> case (k, y) of (Source, EpochMapping _ _) -> True; _ -> False) EpochMapping

-- | Map of binary deb descriptions.
description :: Lens Atoms (Map BinPkgName Text)
description = lens g s
    where
      g = getter7 (\ y -> case y of DHDescription d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHDescription _) -> True; _ -> False) DHDescription

-- | Create a package to hold a cabal executable
executable :: Lens Atoms (Map BinPkgName InstallFile)
executable = lens g s
    where
      g = getter7 (\ y -> case y of DHExecutable d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHExecutable _) -> True; _ -> False) DHExecutable

-- | Create a package for an operating service using the given executable
serverInfo :: Lens Atoms (Map BinPkgName Server)
serverInfo = lens g s
    where
      g = getter7 (\ y -> case y of DHServer d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHServer _) -> True; _ -> False) DHServer

-- | Create a package for a website using the given executable as the server
website :: Lens Atoms (Map BinPkgName Site)
website = lens g s
    where
      g = getter7 (\ y -> case y of DHWebsite d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHWebsite _) -> True; _ -> False) DHWebsite

-- | Generate a backups package using the given cabal executable
backups :: Lens Atoms (Map BinPkgName String)
backups = lens g s
    where
      g = getter7 (\ y -> case y of DHBackups d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHBackups _) -> True; _ -> False) DHBackups

-- | Create an apache configuration file with the given
-- (domain, logdir, filetext).  This is called when expanding
-- the result of the website lens above.
apacheSite :: Lens Atoms (Map BinPkgName (String, FilePath, Text))
apacheSite = lens g s
    where
      g = getter7 (\ y -> case y of DHApacheSite dom log text -> Just (dom, log, text); _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHApacheSite _ _ _) -> True; _ -> False) (\ (dom, log, text) -> DHApacheSite dom log text)

-- * Lower level hints about the debianization


-- | List if packages that should be omitted from any
-- dependency list - e.g. a profiling package missing due
-- to use of noProfilingPackage lens elsewhere.
missingDependencies :: Lens Atoms (Set BinPkgName)
missingDependencies = lens g s
    where
      g = getter2 (\ y -> case y of MissingDependency b -> Just b; _ -> Nothing)
      s = setter2 (\ _ x -> case x of MissingDependency _ -> True; _ -> False) MissingDependency

-- | Override the package name used to hold left over data files and executables.
-- Usually only one package is specified, but if more then one are they will each
-- receive the same list of files.
utilsPackageNames :: Lens Atoms (Set BinPkgName)
utilsPackageNames = lens g s
    where
      g = getter2 (\ y -> case y of UtilsPackageName b -> Just b; _ -> Nothing)
      s x atoms = Set.fold (\ d atoms' -> insertAtom Source (UtilsPackageName d) atoms') (deleteAtoms p atoms) x
          where
            p Source (UtilsPackageName _) = True
            p _ _ = False

-- | Override the debian source package name constructed from the cabal name
sourcePackageName :: Lens Atoms (Maybe SrcPkgName)
sourcePackageName = lens g s
    where
      g = getter6 (\ y -> case y of SourcePackageName x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, SourcePackageName y) -> Just y; _ -> Nothing) SourcePackageName

-- | Revision string used in constructing the debian verison number from the cabal version
revision :: Lens Atoms (Maybe String)
revision = lens g s
    where
      g = getter6 (\ y -> case y of DebRevision x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebRevision y) -> Just y; _ -> Nothing) DebRevision

-- | Exact debian version number, overrides the version generated from the cabal version
debVersion :: Lens Atoms (Maybe DebianVersion)
debVersion = lens g s
    where
      g = getter6 (\ y -> case y of DebVersion x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebVersion y) -> Just y; _ -> Nothing) DebVersion

-- | Maintainer field.  Overrides any value found in the cabal file, or
-- in the DEBIANMAINTAINER environment variable.
maintainer :: Lens Atoms (Maybe NameAddr)
maintainer = lens g s
    where
      g = getter6 (\ y -> case y of DHMaintainer x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DHMaintainer y) -> Just y; _ -> Nothing) DHMaintainer

-- | No longer sure what the purpose of this lens is.
packageInfo :: Lens Atoms (Map PackageName PackageInfo)
packageInfo = lens g s
    where
      g = getter5 (\ y -> case y of DebPackageInfo i -> Just (cabalName i, i); _ -> Nothing)
      s = setter5 (\ k y -> case (k, y) of (Source, DebPackageInfo _) -> True; _ -> False) (\ _ i -> DebPackageInfo i)

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

-- | The copyright information
copyright :: Lens Atoms (Maybe (Either License Text))
copyright = lens g s
    where
      g = getter6 (\ y -> case y of DebCopyright x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebCopyright y) -> Just y; _ -> Nothing) DebCopyright

-- | The source package architecture - @Any@, @All@, or some list of specific architectures.
sourceArchitecture :: Lens Atoms (Maybe PackageArchitectures)
sourceArchitecture = lens g s
    where
      g = getter6 (\ y -> case y of DHArch x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DHArch y) -> Just y; _ -> Nothing) DHArch

-- | Map of the binary package architectures
binaryArchitectures :: Lens Atoms (Map BinPkgName PackageArchitectures)
binaryArchitectures = lens g s
    where
      g = getter7 (\ y -> case y of DHArch d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHArch _) -> True; _ -> False) DHArch

-- | The source package priority
sourcePriority :: Lens Atoms (Maybe PackagePriority)
sourcePriority = lens g s
    where
      g = getter6 (\ y -> case y of DHPriority x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DHPriority y) -> Just y; _ -> Nothing) DHPriority

-- | Map of the binary package priorities
binaryPriorities :: Lens Atoms (Map BinPkgName PackagePriority)
binaryPriorities = lens g s
    where
      g = getter7 (\ y -> case y of DHPriority d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHPriority _) -> True; _ -> False) DHPriority

-- | The source package's section assignment
sourceSection :: Lens Atoms (Maybe Section)
sourceSection = lens g s
    where
      g = getter6 (\ y -> case y of DHSection x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DHSection y) -> Just y; _ -> Nothing) DHSection

-- | Map of the binary deb section assignments
binarySections :: Lens Atoms (Map BinPkgName Section)
binarySections = lens g s
    where
      g = getter7 (\ y -> case y of DHSection d -> Just d; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHSection _) -> True; _ -> False) DHSection

-- * Debian dependency info

-- | Build dependencies.  FIXME: This should be a Set (Set Relation)
-- so we can build or relations, right now we just assume that each
-- Relation is a singleton set.
buildDeps :: Lens Atoms (Set Relations)
buildDeps = lens g s
    where
      g = getter2 (\ y -> case y of BuildDep t -> Just t; _ -> Nothing)
      s = setter2 (\ _ x -> case x of BuildDep _ -> True; _ -> False) BuildDep

-- | Architecture independent
buildDepsIndep :: Lens Atoms (Set Relations)
buildDepsIndep = lens g s
    where
      g = getter2 (\ y -> case y of BuildDepIndep t -> Just t; _ -> Nothing)
      s = setter2 (\ _ x -> case x of BuildDepIndep _ -> True; _ -> False) BuildDepIndep

-- | Map of extra install dependencies for the package's binary debs.
-- This should be [[Relation]] for full generality, or Set (Set Relation)
depends :: Lens Atoms (Map BinPkgName (Set Relation))
depends = lens g s
    where
      g = getter8 (\ y -> case y of Depends rel -> Just rel; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, Depends _) -> True; _ -> False) Depends

-- | Map of extra install conflicts for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
conflicts :: Lens Atoms (Map BinPkgName (Set Relation))
conflicts = lens g s
    where
      g = getter8 (\ y -> case y of Conflicts rel -> Just rel; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, Conflicts _) -> True; _ -> False) Conflicts

-- | Map of extra install replaces for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
replaces :: Lens Atoms (Map BinPkgName (Set Relation))
replaces = lens g s
    where
      g = getter8 (\ y -> case y of Replaces rel -> Just rel; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, Replaces _) -> True; _ -> False) Replaces

-- | Map of extra install provides for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
provides :: Lens Atoms (Map BinPkgName (Set Relation))
provides = lens g s
    where
      g = getter8 (\ y -> case y of Provides rel -> Just rel; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, Provides _) -> True; _ -> False) Provides

-- | Extra install dependencies for the devel library.  Redundant
-- with depends, but kept for backwards compatibility.  Also, I
-- think maybe this is or was needed because it can be set before
-- the exact name of the library package is known.
extraDevDeps :: Lens Atoms (Set Relation)
extraDevDeps = lens g s
    where
      g = getter2 (\ y -> case y of DevDepends t -> Just t; _ -> Nothing)
      s = setter2 (\ _ x -> case x of DevDepends _ -> True; _ -> False) DevDepends

-- | The beginning of the rules file
rulesHead :: Lens Atoms (Maybe Text)
rulesHead = lens g s
    where
      g = getter6 (\ y -> case y of DebRulesHead x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebRulesHead y) -> Just y; _ -> Nothing) DebRulesHead

-- | Additional fragments of the rules file
rulesFragments :: Lens Atoms (Set Text)
rulesFragments = lens g s
    where
      g = getter2 (\ y -> case y of DebRulesFragment t -> Just t; _ -> Nothing)
      s = setter2 (\ _ x -> case x of DebRulesFragment _ -> True; _ -> False) DebRulesFragment

-- | Map of @debian/postinst@ scripts
postInst :: Lens Atoms (Map BinPkgName Text)
postInst = lens g s
    where
      g = getter7 (\ y -> case y of DHPostInst rel -> Just rel; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHPostInst _) -> True; _ -> False) DHPostInst

-- | Map of @debian/postrm@ scripts
postRm :: Lens Atoms (Map BinPkgName Text)
postRm = lens g s
    where
      g = getter7 (\ y -> case y of DHPostRm rel -> Just rel; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHPostRm _) -> True; _ -> False) DHPostRm

-- | Map of @debian/preinst@ scripts
preInst :: Lens Atoms (Map BinPkgName Text)
preInst = lens g s
    where
      g = getter7 (\ y -> case y of DHPreInst rel -> Just rel; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHPreInst _) -> True; _ -> False) DHPreInst

-- | Map of @debian/prerm@ scripts
preRm :: Lens Atoms (Map BinPkgName Text)
preRm = lens g s
    where
      g = getter7 (\ y -> case y of DHPreRm rel -> Just rel; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHPreRm _) -> True; _ -> False) DHPreRm

-- | The @debian/compat@ file, contains the minimum compatible version of the @debhelper@ package
compat :: Lens Atoms (Maybe Int)
compat = lens g s
    where
      g = getter6 (\ y -> case y of DebCompat x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebCompat y) -> Just y; _ -> Nothing) DebCompat

-- | The @debian/source/format@ file.
sourceFormat :: Lens Atoms (Maybe SourceFormat)
sourceFormat = lens g s
    where
      g = getter6 (\ y -> case y of DebSourceFormat x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebSourceFormat y) -> Just y; _ -> Nothing) DebSourceFormat

-- | the @debian/watch@ file
watch :: Lens Atoms (Maybe Text)
watch = lens g s
    where
      g = getter6 (\ y -> case y of DebWatch x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebWatch y) -> Just y; _ -> Nothing) DebWatch

-- | the @debian/changelog@ file
changelog :: Lens Atoms (Maybe ChangeLog)
changelog = lens g s
    where
      g = getter6 (\ y -> case y of DebChangeLog x -> Just x; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebChangeLog y) -> Just y; _ -> Nothing) DebChangeLog

-- | Comment entries for the latest changelog entry (DebLogComments [[Text]])
comments :: Lens Atoms (Maybe [[Text]])
comments = lens g s
    where
      g = getter6 (\ y -> case y of (DebLogComments xss') -> Just xss'; _ -> Nothing)
      s = setter6 (\ k a -> case (k, a) of (Source, DebLogComments y) -> Just y; _ -> Nothing) DebLogComments

-- | The @debian/control@ file.
control :: Lens Atoms SourceDebDescription
control = lens g s
    where
      g = getter1 newSourceDebDescription (\ y -> case y of DebControl x -> Just x; _ -> Nothing)
      s = setter1 (\ k y -> case (k, y) of (Source, DebControl x) -> Just x; _ -> Nothing) DebControl

-- | The @Standards-Version@ field of the @debian/control@ file
standards :: Lens Atoms (Maybe StandardsVersion)
standards = lens standardsVersion (\ b a -> a {standardsVersion = b}) . control

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens Atoms (Map BinPkgName (Set Text))
logrotateStanza = lens g s
    where
      g = getter8 (\ y -> case y of DHLogrotateStanza rel -> Just rel; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHLogrotateStanza _) -> True; _ -> False) DHLogrotateStanza

-- | Add entries to a binary deb's debian/foo.links file.
link :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
link = lens g s
    where
      g = getter8 (\ y -> case y of DHLink loc txt -> Just (loc, txt); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHLink _ _) -> True; _ -> False) (\ (loc, txt) -> DHLink loc txt)

-- | Install files into directories by adding entries to the binary
-- deb's debian/foo.install file.
install :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
install = lens g s
    where
      g = getter8 (\ y -> case y of DHInstall src dst -> Just (src, dst); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstall _ _) -> True; _ -> False) (\ (src, dst) -> DHInstall src dst)

-- | Rename and install files.  This is done by adding rules to debian/rules.
installTo :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installTo = lens g s
    where
      g = getter8 (\ y -> case y of DHInstallTo src dst -> Just (src, dst); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstallTo _ _) -> True; _ -> False) (\ (src, dst) -> DHInstallTo src dst)

-- | Install files into the a binary deb's data directory,
-- /usr/share/packagename-version.  This expands to either an install
-- or an installTo.
installData :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installData = lens g s
    where
      g = getter8 (\ y -> case y of DHInstallData src dst -> Just (src, dst); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstallData _ _ ) -> True; _ -> False) (\ (src, dst) -> DHInstallData src dst)

-- | Create a file in the binary deb with the given text.  This is done by
-- writing the file into the cabalInstall directory and adding an entry
-- to the binary deb's .install file.
file :: Lens Atoms (Map BinPkgName (Set (FilePath, Text)))
file = lens g s
    where
      g = getter8 (\ y -> case y of DHFile path text -> Just (path, text); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHFile _ _) -> True; _ -> False) (\ (path, text) -> DHFile path text)

-- | Install a cabal executable into a binary deb.
installCabalExec :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExec = lens g s
    where
      g = getter8 (\ y -> case y of DHInstallCabalExec name dst -> Just (name, dst); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstallCabalExec _ _) -> True; _ -> False) (\ (name, dst) -> DHInstallCabalExec name dst)

-- | Rename and install a cabal executable
installCabalExecTo :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExecTo = lens g s
    where
      g = getter8 (\ y -> case y of DHInstallCabalExecTo name dst -> Just (name, dst); _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstallCabalExecTo _ _) -> True; _ -> False) (\ (name, dst) -> DHInstallCabalExecTo name dst)

-- | Create directories in the package
installDir :: Lens Atoms (Map BinPkgName (Set FilePath))
installDir = lens g s
    where
      g = getter8 (\ y -> case y of DHInstallDir path -> Just path; _ -> Nothing)
      s = setter8 (\ k y -> case (k, y) of (Binary _, DHInstallDir _) -> True; _ -> False) DHInstallDir

-- | Create an /etc/init.d file in the package
installInit :: Lens Atoms (Map BinPkgName Text)
installInit = lens g s
    where
      g = getter7 (\ y -> case y of DHInstallInit text -> Just text; _ -> Nothing)
      s = setter7 (\ k y -> case (k, y) of (Binary _, DHInstallInit _) -> True; _ -> False) DHInstallInit

-- | Create a file in the debianization.  This is used to implement the file lens above.
intermediateFiles :: Lens Atoms (Set (FilePath, Text))
intermediateFiles = lens g s
    where
      g = getter2 (\ y -> case y of DHIntermediate path text -> Just (path, text); _ -> Nothing)
      s = setter2 (\ _ x -> case x of DHIntermediate _ _ -> True; _ -> False) (\ (path, text) -> DHIntermediate path text)

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity_ = 1
    , debAction_ = Debianize
    , dryRun_ = False
    , validate_ = False
    }

insertAtom :: DebAtomKey -> DebAtom -> Atoms -> Atoms
insertAtom mbin atom atoms = atoms {atomMap = insertWith union mbin (singleton atom) (atomMap atoms)}

insertAtoms :: Set (DebAtomKey, DebAtom) -> Atoms -> Atoms
insertAtoms s atoms =
    case maxView s of
      Nothing -> atoms
      Just ((k, a), s') -> insertAtoms s' (insertAtom k a atoms)

foldAtoms :: (DebAtomKey -> DebAtom -> r -> r) -> r -> Atoms -> r
foldAtoms f r0 atoms = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 (atomMap atoms)

-- | Split atoms out of an Atoms by predicate.
partitionAtoms :: (DebAtomKey -> DebAtom -> Bool) -> Atoms -> (Set (DebAtomKey, DebAtom), Atoms)
partitionAtoms f deb =
    foldAtoms g (mempty, newAtoms (unTop (top deb))) deb
    where
      g k atom (atoms, deb') =
          case f k atom of
            True -> (Set.insert (k, atom) atoms, deb')
            False -> (atoms, insertAtom k atom deb')

deleteAtoms :: (DebAtomKey -> DebAtom -> Bool) -> Atoms -> Atoms
deleteAtoms p atoms = snd (partitionAtoms p atoms)

-- | Split atoms out of a Atoms by predicate.
partitionAtoms' :: (Ord a) => (DebAtomKey -> DebAtom -> Maybe a) -> Atoms -> (Set a, Atoms)
partitionAtoms' f deb =
    foldAtoms g (mempty, newAtoms (unTop (top deb))) deb
    where
      g k atom (xs, deb') =
          case f k atom of
            Just x -> (Set.insert x xs, deb')
            Nothing -> (xs, insertAtom k atom deb')

-- | Like modifyAtoms, but...
modifyAtoms' :: (Ord a) =>
               (DebAtomKey -> DebAtom -> Maybe a)
            -> (Set a -> Set (DebAtomKey, DebAtom))
            -> Atoms
            -> Atoms
modifyAtoms' f g atoms =
    insertAtoms (g s) atoms'
    where
      (s, atoms') = partitionAtoms' f atoms
