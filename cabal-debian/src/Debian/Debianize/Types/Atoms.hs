{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Atoms
    ( DebAtomKey(..)
    , DebAtom(..)
    , HasAtoms(..)
    , insertAtom
    , insertAtoms
    , insertAtoms'
    , lookupAtom
    , lookupAtoms
    , foldAtoms
    , mapAtoms
    , partitionAtoms
    , compiler
    , packageDescription
    , compilerVersion
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageName
    -- * DependencyHint getter and setters
    , dependencyHints
    , doDependencyHint
    , missingDependency
    , setRevision
    , putExecMap
    , putExtraDevDep
    , doExecutable
    , doServer
    , doWebsite
    , setSourcePackageName
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey, insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton, fold, insert)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Debianize.Utility (setMapMaybe)
import Debian.Debianize.Types.Dependencies (DependencyHints(..), defaultDependencyHints)
import Debian.Debianize.Types.PackageHints (InstallFile, Server, Site)
import Debian.Orphans ()
import Debian.Policy (SourceFormat)
import Debian.Relation (BinPkgName, SrcPkgName)
import Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init)

data DebAtomKey
    = Source
    | Binary BinPkgName
    deriving (Eq, Ord, Data, Typeable, Show)

-- | The smallest pieces of debhelper information.  Some of these are
-- converted directly into files in the debian directory, others
-- become fragments of those files, and others are first converted
-- into different DebAtom values as new information becomes available.
data DebAtom
    = NoDocumentationLibrary
    -- ^ Do not produce a libghc-foo-doc package.
    | NoProfilingLibrary
    -- ^ Do not produce a libghc-foo-prof package.
    | CompilerVersion Version
      -- ^ Specify the version number of the GHC compiler in the build
      -- environment.  The default is to assume that version is the same
      -- as the one in the environment where cabal-debian is running.
      -- This is used to look up hard coded lists of packages bundled
      -- with the compiler and their version numbers.  (This could
      -- certainly be done in a more beautiful way.)
    | DHPackageDescription PackageDescription
    -- ^ The cabal package description record
    | DHCompiler Compiler
    -- ^ The Compiler value returned with the Cabal
    -- PackageDescription, then used to determine what libraries
    -- (i.e. dependencies) are provided by the compiler.
    | BuildDir FilePath
    -- ^ The build directory used by cabal, typically dist/build when
    -- building manually or dist-ghc/build when building using GHC and
    -- haskell-devscripts.  This value is used to locate files
    -- produced by cabal so we can move them into the deb.
    | DebSourceFormat SourceFormat                -- ^ Write debian/source/format
    | DebWatch Text                               -- ^ Write debian/watch
    | DHIntermediate FilePath Text                -- ^ Put this text into a file with the given name in the debianization.
    | DebRulesFragment Text                       -- ^ A Fragment of debian/rules
    | Warning Text                                -- ^ A warning to be reported later
    | UtilsPackageName BinPkgName                 -- ^ Name to give the package for left-over data files and executables
    | SourcePackageName SrcPkgName                -- ^ Name to give to debian source package.  If not supplied name is constructed
                                                  -- from the cabal package name.
    | DHDependencyHints DependencyHints           -- ^ Information about the mapping from cabal package names and
                                                  -- versions to debian package names and versions.  (This could be
                                                  -- broken up into smaller atoms, many of which would be attached to
                                                  -- binary packages.)

    -- From here down are atoms to be associated with a Debian binary
    -- package.  This could be done with more type safety, separate
    -- maps for the Source atoms and the Binary atoms.
    | DHApacheSite String FilePath Text           -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    | DHLogrotateStanza Text		          -- ^ Add a stanza of a logrotate file to the binary package
    | DHLink FilePath FilePath          	  -- ^ Create a symbolic link in the binary package
    | DHPostInst Text                   	  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm Text                     	  -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst Text                    	  -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm Text                      	  -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
    | DHInstall FilePath FilePath       	  -- ^ Install a build file into the binary package
    | DHInstallTo FilePath FilePath     	  -- ^ Install a build file into the binary package at an exact location
    | DHInstallData FilePath FilePath   	  -- ^ DHInstallTo the package's data directory: /usr/share/package-version/
    | DHFile FilePath Text              	  -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec String FilePath	  -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo String FilePath	  -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir FilePath             	  -- ^ Create a directory in the binary package
    | DHInstallInit Text                	  -- ^ Add an init.d file to the binary package
    | DHExecutable InstallFile                    -- ^ Create a binary package to hold a cabal executable
    | DHServer Server                             -- ^ Like DHExecutable, but configure the executable as a server process
    | DHWebsite Site                              -- ^ Like DHServer, but configure the server as a web server
    deriving (Eq, Ord, Show)

class HasAtoms atoms where
    getAtoms :: atoms -> Map DebAtomKey (Set DebAtom)
    putAtoms :: Map DebAtomKey (Set DebAtom) -> atoms -> atoms

instance HasAtoms (Map DebAtomKey (Set DebAtom)) where
    getAtoms x = x
    putAtoms _ x = x

lookupAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Maybe a
lookupAtom mbin from atoms =
    case maxView (lookupAtoms mbin from (getAtoms atoms)) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

insertAtom :: HasAtoms atoms => DebAtomKey -> DebAtom -> atoms -> atoms
insertAtom mbin atom x = putAtoms (insertWith union mbin (singleton atom) (getAtoms x)) x

insertAtoms :: HasAtoms atoms => DebAtomKey -> Set DebAtom -> atoms -> atoms
insertAtoms mbin atoms x = putAtoms (insertWith union mbin atoms (getAtoms x)) x

insertAtoms' :: HasAtoms atoms => DebAtomKey -> [DebAtom] -> atoms -> atoms
insertAtoms' mbin atoms x = insertAtoms mbin (fromList atoms) x

foldAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> r -> r) -> r -> atoms -> r
foldAtoms f r0 xs = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 (getAtoms xs)

-- | Map each atom of a HasAtoms instance to zero or more new atoms.
mapAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> Set DebAtom) -> atoms -> atoms
mapAtoms f xs = foldAtoms (\ k atom xs' -> insertAtoms k (f k atom) xs') (putAtoms mempty xs) (getAtoms xs)

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms :: (HasAtoms atoms, Ord a) => (DebAtomKey -> DebAtom -> Maybe a) -> atoms -> (Set a, atoms)
partitionAtoms f deb =
    foldAtoms (\ k atom (xs, deb') -> case f k atom of
                                        Just x -> (Set.insert x xs, deb')
                                        Nothing -> (xs, insertAtom k atom deb'))
              (mempty, putAtoms mempty deb)
              (getAtoms deb)

compiler :: HasAtoms atoms => Compiler -> atoms -> Compiler
compiler def deb =
    fromMaybe def $ lookupAtom Source fromCompiler deb
    where fromCompiler (DHCompiler x) = Just x
          fromCompiler _ = Nothing

packageDescription :: HasAtoms atoms => PackageDescription -> atoms -> PackageDescription
packageDescription def deb =
    fromMaybe def $ lookupAtom Source from deb
    where from (DHPackageDescription x) = Just x
          from _ = Nothing

compilerVersion :: HasAtoms atoms => atoms -> Maybe Version
compilerVersion deb =
    lookupAtom Source from deb
    where from (CompilerVersion x) = Just x
          from _ = Nothing

noProfilingLibrary :: HasAtoms atoms => atoms -> Bool
noProfilingLibrary deb =
    not . Set.null . lookupAtoms Source isNoProfilingLibrary $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = Just NoProfilingLibrary
      isNoProfilingLibrary _ = Nothing

noDocumentationLibrary :: HasAtoms atoms => atoms -> Bool
noDocumentationLibrary deb =
    not . Set.null . lookupAtoms Source isNoDocumentationLibrary $ deb
    where
      isNoDocumentationLibrary NoDocumentationLibrary = Just NoDocumentationLibrary
      isNoDocumentationLibrary _ = Nothing

utilsPackageName :: HasAtoms atoms => atoms -> Maybe BinPkgName
utilsPackageName deb =
    foldAtoms from Nothing deb
    where
      from Source (UtilsPackageName r) Nothing = Just r
      from Source (UtilsPackageName _) (Just _) = error "Multiple values for UtilsPackageName"
      from _ _ r = r

doDependencyHint :: HasAtoms atoms => (DependencyHints -> DependencyHints) -> atoms -> atoms
doDependencyHint f deb =
    if Set.null hints
    then insertAtom Source (DHDependencyHints (f defaultDependencyHints)) deb'
    else insertAtoms' Source (map (DHDependencyHints . f) (Set.toList hints)) deb'
    where
      (hints, deb') = partitionAtoms p deb
      p Source (DHDependencyHints x) = Just x
      p _ _ = Nothing

dependencyHints :: HasAtoms atoms => DependencyHints -> atoms -> DependencyHints
dependencyHints def deb =
    fromMaybe def $ lookupAtom Source from deb
    where
      from (DHDependencyHints x) = Just x
      from _ = Nothing

missingDependency :: HasAtoms atoms => BinPkgName -> atoms -> atoms
missingDependency b deb = doDependencyHint (\ x -> x {missingDependencies = b : missingDependencies x}) deb

setRevision :: HasAtoms atoms => String -> atoms -> atoms
setRevision s deb = doDependencyHint (\ x -> x {revision = s}) deb

putExecMap :: HasAtoms atoms => String -> BinPkgName -> atoms -> atoms
putExecMap cabal debian deb = doDependencyHint (\ x -> x {execMap = Map.insert cabal debian (execMap x)}) deb

putExtraDevDep :: HasAtoms atoms => BinPkgName -> atoms -> atoms
putExtraDevDep bin deb = doDependencyHint (\ x -> x {extraDevDeps = bin : extraDevDeps x}) deb

doExecutable :: HasAtoms atoms => BinPkgName -> InstallFile -> atoms -> atoms
doExecutable bin x deb = insertAtom (Binary bin) (DHExecutable x) deb

doServer :: HasAtoms atoms => BinPkgName -> Server -> atoms -> atoms
doServer bin x deb = insertAtom (Binary bin) (DHServer x) deb

doWebsite :: HasAtoms atoms => BinPkgName -> Site -> atoms -> atoms
doWebsite bin x deb = insertAtom (Binary bin) (DHWebsite x) deb

setSourcePackageName :: HasAtoms atoms => SrcPkgName -> atoms -> atoms
setSourcePackageName src deb = insertAtom Source (SourcePackageName src) deb
