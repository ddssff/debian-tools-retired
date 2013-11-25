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

import Data.Lens.Lazy (getL, lens, Lens, modL)
import Data.Map as Map (empty, fold, foldWithKey, insert, insertWith, Map)
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
import Prelude hiding (init, log, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- Lenses to access values in the Atoms type.  This is an old
-- design which I plan to make private and turn into something
-- nicer, so these will remain ugly and repetitive for now.

-- | Set how much progress messages get generated.
verbosity :: Lens Atoms Int
verbosity = lens (\ a -> verbosity_ (getL flags a)) (\ b a -> modL flags (\ x -> x {verbosity_ = b}) a)

-- | Don't write anything, just output a description of what would have happened
dryRun :: Lens Atoms Bool
dryRun = lens (\ a -> dryRun_ (getL flags a)) (\ b a -> modL flags (\ x -> x {dryRun_ = b}) a)

-- | Make sure the version number and package names of the supplied
-- and generated debianizations match.
validate :: Lens Atoms Bool
validate = lens (\ a -> validate_ (getL flags a)) (\ b a -> modL flags (\ x -> x {validate_ = b}) a)

-- | Debianize, SubstVars, or Usage.  I'm no longer sure what SubstVars does, but someone
-- may still be using it.
debAction :: Lens Atoms DebAction
debAction = lens (\ a -> debAction_ (getL flags a)) (\ b a -> modL flags (\ x -> x {debAction_ = b}) a)

-- | Obsolete record containing verbosity, dryRun, validate, and debAction.
flags :: Lens Atoms Flags
flags = lens g s
    where
      g atoms = fromMaybe defaultFlags $ foldAtoms from Nothing atoms
          where
            from Source (DHFlags x') (Just x) | x /= x' = error $ "Conflicting control values:" ++ show (x, x')
            from Source (DHFlags x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const ((singleton . (Source,) . DHFlags) x)) atoms
          where
            f Source (DHFlags y) = Just y
            f _ _ = Nothing

-- | Unused
warning :: Lens Atoms (Set Text)
warning = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (Warning t) x = Set.insert t x
            from _ _ x = x
      s x atoms = Set.fold (\ text atoms' -> insertAtom Source (Warning text) atoms') (deleteAtoms p atoms) x
          where
            p Source (Warning _) = True
            p _ _ = False


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

{-
-- | The data directory for the package, generated from the packageDescription
dataDir :: Lens Atoms (Maybe FilePath)
dataDir = lens g s
    where
      g atoms =
          fmap (\ p -> let PackageName pkgname = pkgName . package $ p in
                       "usr/share" </> map toLower pkgname) (getL packageDescription atoms)
      s _ _ = error "setL dataDir"
-}

-- | Map from cabal Extra-Lib names to debian binary package names.
extraLibMap :: Lens Atoms (Map String (Set Relations))
extraLibMap = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from Source (ExtraLibMapping cabal debian) x = Map.insertWith union cabal (singleton debian) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ cabal debian atoms' -> Set.fold (\ debian' atoms'' -> insertAtom Source (ExtraLibMapping cabal debian') atoms'') atoms' debian) (deleteAtoms p atoms) x
          where
            p Source (ExtraLibMapping _ _) = True
            p _ _ = False

-- | Map from cabal Build-Tool names to debian binary package names.
execMap :: Lens Atoms (Map String Relations)
execMap = lens g s
    where
      g :: Atoms -> Map String Relations
      g atoms = foldAtoms from Map.empty atoms
          where
            from :: DebAtomKey -> DebAtom -> Map String Relations -> Map String Relations
            from Source (ExecMapping cabal debian) x = Map.insertWith (error "Conflict in execMap") cabal debian x
            from _ _ x = x
      s :: Map String Relations -> Atoms -> Atoms
      s x atoms = Map.foldWithKey (\ cabal debian atoms' -> insertAtom Source (ExecMapping cabal debian) atoms') (deleteAtoms p atoms) x
          where
            p Source (ExecMapping _ _) = True
            p _ _ = False

-- | Cabal flag assignments to use when loading the cabal file.
cabalFlagAssignments :: Lens Atoms (Set (FlagName, Bool))
cabalFlagAssignments = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (DHCabalFlagAssignments pairs) x = union pairs x
            from _ _ x = x
      s x atoms = insertAtom Source (DHCabalFlagAssignments x) (deleteAtoms p atoms)
          where
            p Source (DHCabalFlagAssignments _) = True
            p _ _ = False

-- | Map from cabal version number ranges to debian package names.  This is a
-- result of the fact that only one version of a debian package can be
-- installed at a given time, while multiple versions of a cabal packages can.
debianNameMap :: Lens Atoms (Map PackageName VersionSplits)
debianNameMap = lens g s
    where
      g atoms = foldAtoms from mempty atoms
          where
            from Source (DebianNameMap mp) _ = mp
            from _ _ mp = mp
      s x atoms = insertAtom Source (DebianNameMap x) (deleteAtoms p atoms)
          where
            p Source (DebianNameMap _) = True
            p _ _ = False

-- | Map of Debian epoch numbers assigned to cabal packages.
epochMap :: Lens Atoms (Map PackageName Int)
epochMap = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from Source (EpochMapping name epoch) x = Map.insertWith (error "Conflicting Epochs") name epoch x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ name epoch atoms' -> insertAtom Source (EpochMapping name epoch) atoms') (deleteAtoms p atoms) x
          where
            p Source (EpochMapping _ _) = True
            p _ _ = False

-- | Map of binary deb descriptions.
description :: Lens Atoms (Map BinPkgName Text)
description = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHDescription d) x = Map.insertWith (error "description") b d x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHDescription y) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHDescription _) = True
            p _ _ = False

-- | Create a package to hold a cabal executable
executable :: Lens Atoms (Map BinPkgName InstallFile)
executable = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHExecutable f) x = Map.insertWith (\ k a -> error $ "executable: " ++ show (k, a)) b f x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHExecutable y) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHExecutable _) = True
            p _ _ = False

-- | Create a package for an operating service using the given executable
serverInfo :: Lens Atoms (Map BinPkgName Server)
serverInfo = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHServer s') x = Map.insertWith (error "server") b s' x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHServer y) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHServer _) = True
            p _ _ = False

-- | Create a package for a website using the given executable as the server
website :: Lens Atoms (Map BinPkgName Site)
website = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHWebsite s') x = Map.insertWith (error "website") b s' x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHWebsite y) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHWebsite _) = True
            p _ _ = False

-- | Generate a backups package using the given cabal executable
backups :: Lens Atoms (Map BinPkgName String)
backups = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHBackups s') x = Map.insertWith (error "backups") b s' x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHBackups y) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHBackups _) = True
            p _ _ = False

-- | Create an apache configuration file with the given
-- (domain, logdir, filetext).  This is called when expanding
-- the result of the website lens above.
apacheSite :: Lens Atoms (Map BinPkgName (String, FilePath, Text))
apacheSite = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHApacheSite dom log text) x = Map.insertWith (error "backups") b (dom, log, text) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b (dom, log, text) atoms' -> insertAtom (Binary b) (DHApacheSite dom log text) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHApacheSite _ _ _) = True
            p _ _ = False

-- * Lower level hints about the debianization


-- | List if packages that should be omitted from any
-- dependency list - e.g. a profiling package missing due
-- to use of noProfilingPackage lens elsewhere.
missingDependencies :: Lens Atoms (Set BinPkgName)
missingDependencies = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (MissingDependency b) x = Set.insert b x
            from _ _ x = x
      s x atoms = Set.fold (\ b atoms' -> insertAtom Source (MissingDependency b) atoms') (deleteAtoms p atoms) x
          where
            p Source (MissingDependency _) = True
            p _ _ = False

-- | Override the package name used to hold left over data files and executables.
-- Usually only one package is specified, but if more then one are they will each
-- receive the same list of files.
utilsPackageNames :: Lens Atoms (Set BinPkgName)
utilsPackageNames = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (UtilsPackageName r) x = Set.insert r x
            from _ _ x = x
      s x atoms = Set.fold (\ d atoms' -> insertAtom Source (UtilsPackageName d) atoms') (deleteAtoms p atoms) x
          where
            p Source (UtilsPackageName _) = True
            p _ _ = False

-- | Override the debian source package name constructed from the cabal name
sourcePackageName :: Lens Atoms (Maybe SrcPkgName)
sourcePackageName = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (SourcePackageName x') (Just x) | x /= x' = error $ "Conflicting sourcePackageName values:" ++ show (x, x')
            from Source (SourcePackageName x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . SourcePackageName) x)) atoms
          where
            f Source (SourcePackageName y) = Just y
            f _ _ = Nothing

-- | Revision string used in constructing the debian verison number from the cabal version
revision :: Lens Atoms (Maybe String)
revision = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebRevision x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DebRevision x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebRevision) x)) atoms
          where
            f Source (DebRevision y) = Just y
            f _ _ = Nothing

-- | Exact debian version number, overrides the version generated from the cabal version
debVersion :: Lens Atoms (Maybe DebianVersion)
debVersion = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebVersion x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DebVersion x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebVersion) x)) atoms
          where
            f Source (DebVersion y) = Just y
            f _ _ = Nothing

-- | Maintainer field.  Overrides any value found in the cabal file, or
-- in the DEBIANMAINTAINER environment variable.
maintainer :: Lens Atoms (Maybe NameAddr)
maintainer = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DHMaintainer x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DHMaintainer x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHMaintainer) x)) atoms
          where
            f Source (DHMaintainer y) = Just y
            f _ _ = Nothing

-- | No longer sure what the purpose of this lens is.
packageInfo :: Lens Atoms (Map PackageName PackageInfo)
packageInfo = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from Source (DebPackageInfo i) x = Map.insert (cabalName i) i x
            from _ _ x = x
      s x atoms =
          Map.fold (\ i atoms' -> insertAtom Source (DebPackageInfo i) atoms') (deleteAtoms p atoms) x
          where
            p Source (DebPackageInfo _) = True
            p _ _ = False

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
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebCopyright x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DebCopyright x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebCopyright) x)) atoms
          where
            f Source (DebCopyright y) = Just y
            f _ _ = Nothing

-- | The source package architecture - @Any@, @All@, or some list of specific architectures.
sourceArchitecture :: Lens Atoms (Maybe PackageArchitectures)
sourceArchitecture = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DHArch x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DHArch x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHArch) x)) atoms
          where
            f Source (DHArch y) = Just y
            f _ _ = Nothing

-- | Map of the binary package architectures
binaryArchitectures :: Lens Atoms (Map BinPkgName PackageArchitectures)
binaryArchitectures = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHArch x) m = Map.insert b x m
            from _ _ m = m
      s x atoms = Map.foldWithKey (\ b a atoms' -> insertAtom (Binary b) (DHArch a) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHArch _) = True
            p _ _ = False

-- | The source package priority
sourcePriority :: Lens Atoms (Maybe PackagePriority)
sourcePriority = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DHPriority x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DHPriority x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHPriority) x)) atoms
          where
            f Source (DHPriority y) = Just y
            f _ _ = Nothing

-- | Map of the binary package priorities
binaryPriorities :: Lens Atoms (Map BinPkgName PackagePriority)
binaryPriorities = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHPriority p) x = Map.insertWith (error "priorities") b p x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b p' atoms'-> insertAtom (Binary b) (DHPriority p') atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHPriority _) = True
            p _ _ = False

-- | The source package's section assignment
sourceSection :: Lens Atoms (Maybe Section)
sourceSection = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DHSection x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DHSection x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHSection) x)) atoms
          where
            f Source (DHSection y) = Just y
            f _ _ = Nothing

-- | Map of the binary deb section assignments
binarySections :: Lens Atoms (Map BinPkgName Section)
binarySections = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHSection p) x = Map.insertWith (error "sections") b p x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b p' atoms'-> insertAtom (Binary b) (DHSection p') atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHSection _) = True
            p _ _ = False

-- * Debian dependency info

-- | Build dependencies.  FIXME: This should be a Set (Set Relation)
-- so we can build or relations, right now we just assume that each
-- Relation is a singleton set.
buildDeps :: Lens Atoms (Set Relations)
buildDeps = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (BuildDep r) x = Set.insert r x
            from _ _ x = x
      s x atoms = Set.fold (\ d atoms' -> insertAtom Source (BuildDep d) atoms') (deleteAtoms p atoms) x
          where
            p Source (BuildDep _) = True
            p _ _ = False

-- | Architecture independent
buildDepsIndep :: Lens Atoms (Set Relations)
buildDepsIndep = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (BuildDepIndep r) x = Set.insert r x
            from _ _ x = x
      s r atoms = Set.fold (\ d atoms' -> insertAtom Source (BuildDepIndep d) atoms') (deleteAtoms p atoms) r
          where
            p Source (BuildDepIndep _) = True
            p _ _ = False

-- | Map of extra install dependencies for the package's binary debs.
-- This should be [[Relation]] for full generality, or Set (Set Relation)
depends :: Lens Atoms (Map BinPkgName (Set Relation))
depends = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (Depends rel) x = Map.insertWith union b (singleton rel) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Depends rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
          where
            p (Binary _) (Depends _) = True
            p _ _ = False

-- | Map of extra install conflicts for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
conflicts :: Lens Atoms (Map BinPkgName (Set Relation))
conflicts = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (Conflicts rel) x = Map.insertWith union b (singleton rel) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Conflicts rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
          where
            p (Binary _) (Conflicts _) = True
            p _ _ = False

-- | Map of extra install replaces for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
replaces :: Lens Atoms (Map BinPkgName (Set Relation))
replaces = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (Replaces rel) x = Map.insertWith union b (singleton rel) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Replaces rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
          where
            p (Binary _) (Replaces _) = True
            p _ _ = False

-- | Map of extra install provides for the package's binary debs.
-- We should support all the other dependency fields - provides, replaces, etc.
provides :: Lens Atoms (Map BinPkgName (Set Relation))
provides = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (Provides rel) x = Map.insertWith union b (singleton rel) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Provides rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
          where
            p (Binary _) (Provides _) = True
            p _ _ = False

-- | Extra install dependencies for the devel library.  Redundant
-- with depends, but kept for backwards compatibility.  Also, I
-- think maybe this is or was needed because it can be set before
-- the exact name of the library package is known.
extraDevDeps :: Lens Atoms (Set Relation)
extraDevDeps = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (DevDepends b) x = Set.insert b x
            from _ _ x = x
      s x atoms = Set.fold (\ d atoms' -> insertAtom Source (DevDepends d) atoms') (deleteAtoms p atoms) x
          where
            p Source (DevDepends _) = True
            p _ _ = False

-- | The beginning of the rules file
rulesHead :: Lens Atoms (Maybe Text)
rulesHead = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebRulesHead x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
            from Source (DebRulesHead x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebRulesHead) x)) atoms
          where
            f Source (DebRulesHead y) = Just y
            f _ _ = Nothing

-- | Additional fragments of the rules file
rulesFragments :: Lens Atoms (Set Text)
rulesFragments = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (DebRulesFragment t) x = Set.insert t x
            from _ _ x = x
      s x atoms = Set.fold (\ text atoms' -> insertAtom Source (DebRulesFragment text) atoms') (deleteAtoms p atoms) x
          where
            p Source (DebRulesFragment _) = True
            p _ _ = False

-- | Map of @debian/postinst@ scripts
postInst :: Lens Atoms (Map BinPkgName Text)
postInst = lens g s
    where
      g atoms = foldAtoms from mempty atoms
          where
            from :: DebAtomKey -> DebAtom -> Map BinPkgName Text -> Map BinPkgName Text
            from (Binary b) (DHPostInst t) x = Map.insertWith (error "Conflicting postInsts") b t x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b t atoms' -> insertAtom (Binary b) (DHPostInst t) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHPostInst _) = True
            p _ _ = False

-- | Map of @debian/postrm@ scripts
postRm :: Lens Atoms (Map BinPkgName Text)
postRm = lens g s
    where
      g atoms = foldAtoms from mempty atoms
          where
            from :: DebAtomKey -> DebAtom -> Map BinPkgName Text -> Map BinPkgName Text
            from (Binary b) (DHPostRm t) m = Map.insertWith (error "Conflicting postRms") b t m
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b t atoms' -> insertAtom (Binary b) (DHPostRm t) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHPostRm _) = True
            p _ _ = False

-- | Map of @debian/preinst@ scripts
preInst :: Lens Atoms (Map BinPkgName Text)
preInst = lens g s
    where
      g atoms = foldAtoms from mempty atoms
          where
            from :: DebAtomKey -> DebAtom -> Map BinPkgName Text -> Map BinPkgName Text
            from (Binary b) (DHPreInst t) m = Map.insertWith (error "Conflicting preInsts") b t m
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b t atoms' -> insertAtom (Binary b) (DHPreInst t) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHPreInst _) = True
            p _ _ = False

-- | Map of @debian/prerm@ scripts
preRm :: Lens Atoms (Map BinPkgName Text)
preRm = lens g s
    where
      g atoms = foldAtoms from mempty atoms
          where
            from :: DebAtomKey -> DebAtom -> Map BinPkgName Text -> Map BinPkgName Text
            from (Binary b) (DHPreRm t) m = Map.insertWith (error "Conflicting preRms") b t m
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b t atoms' -> insertAtom (Binary b) (DHPreRm t) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHPreRm _) = True
            p _ _ = False

-- | The @debian/compat@ file, contains the minimum compatible version of the @debhelper@ package
compat :: Lens Atoms (Maybe Int)
compat = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebCompat x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
            from Source (DebCompat x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebCompat) x)) atoms
          where
            f Source (DebCompat y) = Just y
            f _ _ = Nothing

-- | The @debian/source/format@ file.
sourceFormat :: Lens Atoms (Maybe SourceFormat)
sourceFormat = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebSourceFormat x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
            from Source (DebSourceFormat x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebSourceFormat) x)) atoms
          where
            f Source (DebSourceFormat y) = Just y
            f _ _ = Nothing

-- | the @debian/watch@ file
watch :: Lens Atoms (Maybe Text)
watch = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebWatch x') (Just x) | x /= x' = error $ "Conflicting watch values:" ++ show (x, x')
            from Source (DebWatch x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebWatch) x)) atoms
          where
            f Source (DebWatch y) = Just y
            f _ _ = Nothing

-- | the @debian/changelog@ file
changelog :: Lens Atoms (Maybe ChangeLog)
changelog = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebChangeLog x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
            from Source (DebChangeLog x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebChangeLog) x)) atoms
          where
            f Source (DebChangeLog y) = Just y
            f _ _ = Nothing

-- | Comment entries for the latest changelog entry (DebLogComments [[Text]])
comments :: Lens Atoms (Maybe [[Text]])
comments = lens g s
    where
      g atoms = foldAtoms from Nothing atoms
          where
            from Source (DebLogComments xss') (Just xss) | xss == xss' = error $ "Conflicting log comments: " ++ show (xss, xss')
            from Source (DebLogComments xss) _ = Just xss
            from _ _ x = x
      s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebLogComments) x)) atoms
          where
            f Source (DebLogComments y) = Just y
            f _ _ = Nothing

-- | The @debian/control@ file.
control :: Lens Atoms SourceDebDescription
control = lens g s
    where
      g atoms = fromMaybe newSourceDebDescription $ foldAtoms from Nothing atoms
          where
            from Source (DebControl x') (Just x) | x /= x' = error $ "Conflicting control values:" ++ show (x, x')
            from Source (DebControl x) _ = Just x
            from _ _ x = x
      s x atoms = modifyAtoms' f (const ((singleton . (Source,) . DebControl) x)) atoms
          where
            f Source (DebControl y) = Just y
            f _ _ = Nothing

-- | The @Standards-Version@ field of the @debian/control@ file
standards :: Lens Atoms (Maybe StandardsVersion)
standards = lens (\ a -> standardsVersion (getL control a)) (\ b a -> modL control (\ x -> x {standardsVersion = b}) a)

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens Atoms (Map BinPkgName (Set Text))
logrotateStanza = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHLogrotateStanza r) x = Map.insertWith Set.union b (singleton r) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b ts atoms'-> Set.fold (\ t atoms'' -> insertAtom (Binary b) (DHLogrotateStanza t) atoms'') atoms' ts) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHLogrotateStanza _) = True
            p _ _ = False

-- | Add entries to a binary deb's debian/foo.links file.
link :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
link = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHLink loc txt) x = Map.insertWith Set.union b (singleton (loc, txt)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (loc, txt) atoms'' -> insertAtom (Binary b) (DHLink loc txt) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHLink _ _) = True
            p _ _ = False

-- | Install files into directories by adding entries to the binary
-- deb's debian/foo.install file.
install :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
install = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstall src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstall src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstall _ _) = True
            p _ _ = False

-- | Rename and install files.  This is done by adding rules to debian/rules.
installTo :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installTo = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallTo src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstallTo src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallTo _ _) = True
            p _ _ = False

-- | Install files into the a binary deb's data directory,
-- /usr/share/packagename-version.  This expands to either an install
-- or an installTo.
installData :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installData = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallData src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstallData src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallData _ _) = True
            p _ _ = False

-- | Create a file in the binary deb with the given text.  This is done by
-- writing the file into the cabalInstall directory and adding an entry
-- to the binary deb's .install file.
file :: Lens Atoms (Map BinPkgName (Set (FilePath, Text)))
file = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHFile path text) x = Map.insertWith Set.union b (singleton (path, text)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (path, text) atoms'' -> insertAtom (Binary b) (DHFile path text) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHFile _ _) = True
            p _ _ = False

-- | Install a cabal executable into a binary deb.
installCabalExec :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExec = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallCabalExec name dst) x = Map.insertWith Set.union b (singleton (name, dst)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (name, dst) atoms'' -> insertAtom (Binary b) (DHInstallCabalExec name dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallCabalExec _ _) = True
            p _ _ = False

-- | Rename and install a cabal executable
installCabalExecTo :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExecTo = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallCabalExecTo name dst) x = Map.insertWith Set.union b (singleton (name, dst)) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (name, dst) atoms'' -> insertAtom (Binary b) (DHInstallCabalExecTo name dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallCabalExecTo _ _) = True
            p _ _ = False

-- | Create directories in the package
installDir :: Lens Atoms (Map BinPkgName (Set FilePath))
installDir = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallDir path) x = Map.insertWith Set.union b (singleton path) x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b paths atoms'-> Set.fold (\ path atoms'' -> insertAtom (Binary b) (DHInstallDir path) atoms'') atoms' paths) (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallDir _) = True
            p _ _ = False

-- | Create an /etc/init.d file in the package
installInit :: Lens Atoms (Map BinPkgName Text)
installInit = lens g s
    where
      g atoms = foldAtoms from Map.empty atoms
          where
            from (Binary b) (DHInstallInit text) x = Map.insertWith (error "installInit") b text x
            from _ _ x = x
      s x atoms = Map.foldWithKey (\ b text atoms'-> insertAtom (Binary b) (DHInstallInit text) atoms') (deleteAtoms p atoms) x
          where
            p (Binary _) (DHInstallInit _) = True
            p _ _ = False

-- | Create a file in the debianization.  This is used to implement the file lens above.
intermediateFiles :: Lens Atoms (Set (FilePath, Text))
intermediateFiles = lens g s
    where
      g atoms = foldAtoms from Set.empty atoms
          where
            from Source (DHIntermediate path text) x = Set.insert (path, text) x
            from _ _ x = x
      s x atoms =  Set.fold (\ (path, text) atoms' -> insertAtom Source (DHIntermediate path text) atoms') (deleteAtoms p atoms) x
          where
            p Source (DHIntermediate _ _) = True
            p _ _ = False

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
