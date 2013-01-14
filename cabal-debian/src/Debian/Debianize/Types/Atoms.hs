{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Atoms
    ( DebAtomKey(..)
    , DebAtom(..)
    , Flags(..)
    , defaultFlags
    , DebAction(..)
    , HasAtoms(..)
    , AtomMap
    , insertAtom
    , insertAtoms
    , insertAtoms'
    , lookupAtom
    , lookupAtomDef
    , lookupAtoms
    , hasAtom
    , foldAtoms
    , mapAtoms
    , partitionAtoms
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey, insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, unions, singleton, fold, insert)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Debianize.Utility (setMapMaybe)
import Debian.Debianize.Types.Dependencies (DependencyHints(..), defaultDependencyHints)
import Debian.Debianize.Types.PackageHints (InstallFile, Server, Site)
import Debian.Debianize.Types.PackageType (DebType)
import Debian.Orphans ()
import Debian.Policy (SourceFormat)
import Debian.Relation (BinPkgName, SrcPkgName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

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
    -- produced by cabal so we can move them into the deb.  Note that
    -- the --builddir option of runhaskell Setup appends the "/build"
    -- to the value it receives, so, yes, try not to get confused.
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
    | DHMaintainer NameAddr			  -- ^ Value for the maintainer field in the control file.  Note that
                                                  -- the cabal maintainer field can have multiple addresses, but debian
                                                  -- only one.  If this is not explicitly set, it is obtained from the
                                                  -- cabal file, and if it is not there then from the environment.  As a
                                                  -- last resort, there is a hard coded string in here somewhere.
    | DHCabalFlagAssignments (Set (FlagName, Bool)) -- ^ Flags to pass to Cabal function finalizePackageDescription, this
                                                  -- can be used to control the flags in the cabal file.
    | DHFlags Flags                               -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc

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

-- | This record supplies information about the task we want done -
-- debianization, validataion, help message, etc.
data Flags = Flags
    {
    -------------------------
    -- Modes of Operation ---
    -------------------------
      verbosity :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , dryRun :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , validate :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , debAction :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    } deriving (Eq, Ord, Show)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord)

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity = 1
    , debAction = Usage
    , dryRun = False
    , validate = False
    }

type AtomMap = Map DebAtomKey (Set DebAtom)

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

lookupAtomDef :: (HasAtoms atoms, Show a, Ord a) => a -> DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> a
lookupAtomDef def key from atoms = fromMaybe def $ lookupAtom key from atoms

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

insertAtom :: HasAtoms atoms => DebAtomKey -> DebAtom -> atoms -> atoms
insertAtom mbin atom x = putAtoms (insertWith union mbin (singleton atom) (getAtoms x)) x

insertAtoms :: HasAtoms atoms => DebAtomKey -> Set DebAtom -> atoms -> atoms
insertAtoms mbin atoms x = putAtoms (insertWith union mbin atoms (getAtoms x)) x

insertAtoms' :: HasAtoms atoms => DebAtomKey -> [DebAtom] -> atoms -> atoms
insertAtoms' mbin atoms x = insertAtoms mbin (fromList atoms) x

hasAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Bool
hasAtom key pred atoms = not . Set.null . lookupAtoms key pred $ atoms

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
