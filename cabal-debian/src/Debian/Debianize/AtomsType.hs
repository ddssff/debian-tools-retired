{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.AtomsType
    ( DebAtomKey(..)
    , DebAtom(..)
    , Flags(..)
    , PackageInfo(..)
    , defaultFlags
    , DebAction(..)
    , HasAtoms(..)
    , Atoms(atomMap) -- FIXME: don't export atomMap
    , defaultAtoms
    , insertAtom
    , insertAtoms
    , insertAtoms'
    , lookupAtom
    , lookupAtomDef
    , lookupAtoms
    , hasAtom
    , foldAtoms
    , mapAtoms
    -- , partitionAtoms
    , replaceAtoms
    -- , modifyAtoms
    , partitionAtoms'
    , modifyAtoms'
    , getMaybeSingleton
    , getSingleton
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton, fold, insert)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog)
import Debian.Debianize.Utility (setMapMaybe)
import Debian.Debianize.Types.PackageHints (InstallFile, Server, Site)
import Debian.Debianize.Types.PackageType (DebType, VersionSplits, knownVersionSplits)
import Debian.Orphans ()
import Debian.Policy (SourceFormat, PackageArchitectures, PackagePriority, Section)
import Debian.Relation (BinPkgName, SrcPkgName, Relation)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
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
    | DataDir FilePath
    -- ^ the pathname of the package's data directory, generally the
    -- value of the dataDirectory field in the PackageDescription.
    | DebSourceFormat SourceFormat                -- ^ Write debian/source/format
    | DebWatch Text                               -- ^ Write debian/watch
    | DHIntermediate FilePath Text                -- ^ Put this text into a file with the given name in the debianization.
    | DebRulesHead Text				  -- ^ The header of the debian/rules file.  The remainder is assembled
                                                  -- from DebRulesFragment values in the atom list.
    | DebRulesFragment Text                       -- ^ A Fragment of debian/rules
    | Warning Text                                -- ^ A warning to be reported later
    | UtilsPackageName BinPkgName                 -- ^ Name to give the package for left-over data files and executables
    | DebChangeLog ChangeLog			  -- ^ The changelog, first entry contains the source package name and version
    | SourcePackageName SrcPkgName                -- ^ Name to give to debian source package.  If not supplied name is constructed
                                                  -- from the cabal package name.
    | DHMaintainer NameAddr			  -- ^ Value for the maintainer field in the control file.  Note that
                                                  -- the cabal maintainer field can have multiple addresses, but debian
                                                  -- only one.  If this is not explicitly set, it is obtained from the
                                                  -- cabal file, and if it is not there then from the environment.  As a
                                                  -- last resort, there is a hard coded string in here somewhere.
    | DHCabalFlagAssignments (Set (FlagName, Bool)) -- ^ Flags to pass to Cabal function finalizePackageDescription, this
                                                  -- can be used to control the flags in the cabal file.
    | DHFlags Flags                               -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc
    | DebRevision String			  -- ^ Specify the revision string to use when converting the cabal
                                                  -- version to debian.

    | OmitLTDeps				  -- ^ If present, don't generate the << dependency when we see a cabal
                                                  -- equals dependency.  (The implementation of this was somehow lost.)
    | DebVersion DebianVersion			  -- ^ Specify the exact debian version of the resulting package,
                                                  -- including epoch.  One use case is to work around the the
                                                  -- "buildN" versions that are often uploaded to the debian and
                                                  -- ubuntu repositories.  Say the latest cabal version of
                                                  -- transformers is 0.3.0.0, but the debian repository contains
                                                  -- version 0.3.0.0-1build3, we need to specify
                                                  -- debVersion="0.3.0.0-1build3" or the version we produce will
                                                  -- look older than the one already available upstream.
    | VersionSplits [VersionSplits]		  -- ^ Instances where the debian package name is different (for
                                                  -- some range of version numbers) from the default constructed
                                                  -- by mkPkgName.
    | BuildDep BinPkgName			  -- ^ Add a build dependency (FIXME: should be a Rel, or an OrRelation, not a BinPkgName)
    | BuildDepIndep BinPkgName			  -- ^ Add an arch independent build dependency
    | MissingDependency BinPkgName		  -- ^ Lets cabal-debian know that a package it might expect to exist
                                                  -- actually does not, so omit all uses in resulting debianization.
    | ExtraLibMapping String BinPkgName		  -- ^ Map a cabal Extra-Library name to a debian binary package name,
                                                  -- e.g. @ExtraLibMapping extraLibMap "cryptopp" "libcrypto-dev"@ adds a
                                                  -- build dependency on @libcrypto-dev@ to any package that has @cryptopp@
                                                  -- in its cabal Extra-Library list.
    | ExecMapping String BinPkgName		  -- ^ Map a cabal Build-Tool name to a debian binary package name,
                                                  -- e.g. @ExecMapping "trhsx" "haskell-hsx-utils"@ adds a build
                                                  -- dependency on @haskell-hsx-utils@ to any package that has @trhsx@ in its
                                                  -- cabal build-tool list.
    | EpochMapping PackageName Int		  -- ^ Specify epoch numbers for the debian package generated from a
                                                  -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
    | DebPackageInfo PackageInfo		  -- ^ Supply some info about a cabal package.
    | DebCompat Int				  -- ^ The debhelper compatibility level, from debian/compat.
    | DebCopyright (Either License Text)	  -- ^ Copyright information, either as a Cabal License value or
                                                  -- the full text.
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
    | DHArch PackageArchitectures       	  -- ^ Set the Architecture field of source or binary
    | DHPriority PackagePriority	       	  -- ^ Set the Priority field of source or binary
    | DHSection Section			       	  -- ^ Set the Section field of source or binary
    | DHDescription Text		       	  -- ^ Set the description of source or binary
    | DHInstall FilePath FilePath       	  -- ^ Install a build file into the binary package
    | DHInstallTo FilePath FilePath     	  -- ^ Install a build file into the binary package at an exact location
    | DHInstallData FilePath FilePath   	  -- ^ DHInstallTo somewhere relative to DataDir (see above)
    | DHFile FilePath Text              	  -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec String FilePath	  -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo String FilePath	  -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir FilePath             	  -- ^ Create a directory in the binary package
    | DHInstallInit Text                	  -- ^ Add an init.d file to the binary package
    | DHExecutable InstallFile                    -- ^ Create a binary package to hold a cabal executable
    | DHServer Server                             -- ^ Like DHExecutable, but configure the executable as a server process
    | DHWebsite Site                              -- ^ Like DHServer, but configure the server as a web server
    | DHBackups String                            -- ^ Configure the executable to do incremental backups
    | Depends Relation				  -- ^ Says that the debian package should have this relation in Depends
    | Conflicts Relation			  -- ^ Says that the debian package should have this relation in Conflicts
    | DevDepends BinPkgName			  -- ^ Limited version of Depends, put a dependency on the dev library package.  The only
                                                  -- reason to use this is because we don't yet know the name of the dev library package.
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

data PackageInfo = PackageInfo { cabalName :: String
                               , devDeb :: Maybe (BinPkgName, DebianVersion)
                               , profDeb :: Maybe (BinPkgName, DebianVersion)
                               , docDeb :: Maybe (BinPkgName, DebianVersion) } deriving (Eq, Ord, Show)

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity = 1
    , debAction = Usage
    , dryRun = False
    , validate = False
    }

data Atoms =
    Atoms
    { atomMap :: Map DebAtomKey (Set DebAtom)
    -- ^ Information about the mapping from cabal package names and
    -- versions to debian package names and versions.  (This could be
    -- broken up into smaller atoms, many of which would be attached to
    -- binary packages.)
    } deriving (Eq, Ord, Show)

defaultAtoms :: Atoms
defaultAtoms = insertAtom Source (VersionSplits knownVersionSplits) $ Atoms {atomMap = mempty}

class HasAtoms atoms where
    getAtoms :: atoms -> Map DebAtomKey (Set DebAtom)
    putAtoms :: Map DebAtomKey (Set DebAtom) -> atoms -> atoms

instance HasAtoms Atoms where
    getAtoms = atomMap
    putAtoms mp x = x {atomMap = mp}

lookupAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Maybe a
lookupAtom mbin from xs =
    case maxView (lookupAtoms mbin from xs) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtomDef :: (HasAtoms atoms, Show a, Ord a) => a -> DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> a
lookupAtomDef def key from xs = fromMaybe def $ lookupAtom key from xs

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

insertAtom :: HasAtoms atoms => DebAtomKey -> DebAtom -> atoms -> atoms
insertAtom mbin atom x = putAtoms (insertWith union mbin (singleton atom) (getAtoms x)) x

insertAtoms :: HasAtoms atoms => Set (DebAtomKey, DebAtom) -> atoms -> atoms
insertAtoms s atoms =
    case maxView s of
      Nothing -> atoms
      Just ((k, a), s') -> insertAtoms s' (insertAtom k a atoms)

insertAtoms' :: HasAtoms atoms => [(DebAtomKey, DebAtom)] -> atoms -> atoms
insertAtoms' xs atoms = insertAtoms (fromList xs) atoms

hasAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Bool
hasAtom key p xs = not . Set.null . lookupAtoms key p $ xs

foldAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> r -> r) -> r -> atoms -> r
foldAtoms f r0 xs = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 (getAtoms xs)

-- | Map each atom of a HasAtoms instance to zero or more new atoms.
mapAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> Set (DebAtomKey, DebAtom)) -> atoms -> atoms
mapAtoms f xs = foldAtoms (\ k atom xs' -> insertAtoms (f k atom) xs') (putAtoms mempty xs) xs

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> Bool) -> atoms -> (Set (DebAtomKey, DebAtom), atoms)
partitionAtoms f deb =
    foldAtoms g (mempty, putAtoms mempty deb) deb
    where
      g k atom (atoms, deb') =
          case f k atom of
            True -> (Set.insert (k, atom) atoms, deb')
            False -> (atoms, insertAtom k atom deb')

replaceAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> Bool) -> DebAtomKey -> DebAtom -> atoms -> atoms
replaceAtoms f k atom atoms = insertAtom k atom (snd (partitionAtoms f atoms))

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms' :: (HasAtoms atoms, Ord a) => (DebAtomKey -> DebAtom -> Maybe a) -> atoms -> (Set a, atoms)
partitionAtoms' f deb =
    foldAtoms g (mempty, putAtoms mempty deb) deb
    where
      g k atom (xs, deb') =
          case f k atom of
            Just x -> (Set.insert x xs, deb')
            Nothing -> (xs, insertAtom k atom deb')

-- | Like modifyAtoms, but 
modifyAtoms' :: (HasAtoms atoms, Ord a) =>
               (DebAtomKey -> DebAtom -> Maybe a)
            -> (Set a -> Set (DebAtomKey, DebAtom))
            -> atoms
            -> atoms
modifyAtoms' f g atoms =
    insertAtoms (g s) atoms'
    where
      (s, atoms') = partitionAtoms' f atoms

getMaybeSingleton :: (HasAtoms atoms, Eq a) => Maybe a -> (DebAtomKey -> DebAtom -> Maybe a) -> atoms -> Maybe a
getMaybeSingleton multiple f atoms =
    foldAtoms from Nothing atoms
    where
      from k a (Just x) =
          case f k a of
            Just x' -> if x /= x' then multiple else Just x
            Nothing -> Just x
      from k a Nothing =
          f k a

getSingleton :: (HasAtoms atoms, Eq a) => a -> (DebAtomKey -> DebAtom -> Maybe a) -> atoms -> a
getSingleton def f atoms = fromMaybe def (getMaybeSingleton Nothing f atoms)
