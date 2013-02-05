{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Atoms
    ( HasAtoms(..)
    , DebAtomKey(..)
    , DebAtom(..)
    , Flags(..)
    , DebAction(..)
    , PackageInfo(..)
    , Site(..)
    , Server(..)
    , InstallFile(..)
    , DebType(..)
    , VersionSplits(..)
    , knownVersionSplits
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    , Atoms(Atoms, unAtoms)
    , defaultFlags
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
    , deleteAtoms
    -- , modifyAtoms
    , partitionAtoms'
    , modifyAtoms'
    , getMaybeSingleton
    , getSingleton
    -- * Future class methods
    , compiler
    , setCompiler
    , dataDir
    , compilerVersion
    , putCompilerVersion
    , noProfilingLibrary
    , putNoProfilingLibrary
    , noDocumentationLibrary
    , putNoDocumentationLibrary
    , utilsPackageName
    -- * DependencyHint getter and setters
    , missingDependency
    , setRevision
    , revision
    , setDebVersion
    , debVersion
    , setOmitLTDeps
    , omitLTDeps
    , versionSplits
    , buildDeps
    , buildDepsIndep
    , missingDependencies
    , extraLibMap
    , execMap
    , filterMissing
    , putExecMap
    , putExtraDevDep
    , extraDevDeps
    , putEpochMapping
    , epochMap
    , knownEpochMappings
    , depends
    , conflicts
    , binaryPackageDeps
    , binaryPackageConflicts
    , putPackageInfo
    , packageInfo
    , setSourceArchitecture
    , setSourcePriority
    , setSourceSection
    , setSourceDescription
    , setArchitecture
    , setPriority
    , setSection
    , setDescription
    , doExecutable
    , doServer
    , doWebsite
    , doBackups
    , putCopyright
    , copyright
    , warning
    , debMaintainer
    , buildDir
    , setBuildDir
    , cabalFlagAssignments
    , putCabalFlagAssignments
    , flags
    , mapFlags
    , watchAtom
    , tightDependencyFixup
    , modControl
    , modifySourceDebDescription'
    , modifySourceDebDescription''
    -- , newDebianization
    , putBuildDep
    , putBuildDepIndep
    , putExtraLibMapping
    , putDebMaintainer
    , intermediateFile
    , getIntermediateFiles
    , installInit
    , install
    , installData
    , getInstalls
    , logrotateStanza
    , putPostInst
    , getPostInsts
    , getPostInst
    , modPostInst
    , postRm
    , preInst
    , preRm
    , link
    , file
    , installDir
    , rulesFragment
    , installCabalExec
    , installCabalExecTo
    , installTo
    , getInstallDirs
    , getInstallInits
    , getLogrotateStanzas
    , getLinks
    , getPostRms
    , getPreInsts
    , getPreRms
    , foldExecs
    , foldArchitectures
    , foldPriorities
    , foldSections
    , foldDescriptions
    , finalizeAtoms
    , foldCabalDatas
    , foldCabalExecs
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Lazy (Lens)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Set (Set)
import Data.Text (Text)
import Data.Version (Version(Version))
import Debian.Changes (ChangeLog)
import Debian.Debianize.ControlFile (SourceDebDescription)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, SourceFormat, PackagePriority, Section)
import Debian.Relation (Relation, SrcPkgName, BinPkgName)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName(PackageName))
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.Lens.Lazy (lens, getL, modL)
import Data.List as List (map)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey, empty, null, insert, update)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set as Set (maxView, toList, fromList, null, empty, union, singleton, fold, insert, member)
import Data.Text (unpack)
import Debian.Debianize.Utility (setMapMaybe)
import Debian.Orphans ()
import Debian.Policy (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Distribution.Package (PackageIdentifier(..))
import Prelude hiding (init, unlines, log)
import System.Process (showCommandForUser)

import Data.List (intersperse)
import Data.Monoid ((<>))
--import Data.Set as Set (Set, maxView, toList, null, union, singleton, insert, member)
import Data.Text (pack, unlines)
import Data.Version (showVersion)
import Debian.Debianize.ControlFile (newSourceDebDescription)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), Relation(..))
import Distribution.PackageDescription as Cabal (PackageDescription(package))
import System.FilePath ((</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

class (Monoid atoms,
       Show atoms) -- for debugging
    => HasAtoms atoms where
    rulesHead :: Lens atoms (Maybe Text)
    packageDescription :: Lens atoms (Maybe PackageDescription)
    postInst :: Lens atoms (Map BinPkgName Text)
    compat :: Lens atoms (Maybe Int)
    sourceFormat :: Lens atoms (Maybe SourceFormat)
    watch :: Lens atoms (Maybe Text)
    sourcePackageName :: Lens atoms (Maybe SrcPkgName)
    changelog :: Lens atoms (Maybe ChangeLog)
    comments :: Lens atoms (Maybe [[Text]]) -- ^ Comment entries for the latest changelog entry
    control :: Lens atoms (Maybe SourceDebDescription)

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
    | DebLogComments [[Text]]			  -- ^ Each element is a comment to be added to the changelog, where the
                                                  -- element's text elements are the lines of the comment.
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
    | DebVersionSplits [VersionSplits]		  -- ^ Instances where the debian package name is different (for
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
    | DebControl SourceDebDescription		  -- ^ The parsed contents of the control file
    | DHPostInst (Map BinPkgName Text)	 	  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0

    -- From here down are atoms to be associated with a Debian binary
    -- package.  This could be done with more type safety, separate
    -- maps for the Source atoms and the Binary atoms.
    | DHApacheSite String FilePath Text           -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    | DHLogrotateStanza Text		          -- ^ Add a stanza of a logrotate file to the binary package
    | DHLink FilePath FilePath          	  -- ^ Create a symbolic link in the binary package
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
    deriving (Eq, Ord, Show, Typeable)

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

-- | Information about the web site we are packaging.
data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      , server :: Server   -- ^ The hint to install the server job
      } deriving (Read, Show, Eq, Ord)

-- | Information about the server we are packaging.
data Server
    = Server
      { hostname :: String      -- ^ Host on which the server will run
      , port :: Int             -- ^ Port on which the server will run.
                                -- Obviously, this must assign each and
                                -- every server package to a different
                                -- port.
      , headerMessage :: String -- ^ A comment that will be inserted to
                                -- explain how the file was generated
      , retry :: String         -- ^ start-stop-daemon --retry argument
      , serverFlags :: [String] -- ^ Extra flags to pass to the server via the init script
      , installFile :: InstallFile -- ^ The hint to install the server executable
      } deriving (Read, Show, Eq, Ord)

data InstallFile
    = InstallFile
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      } deriving (Read, Show, Eq, Ord)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show)

data VersionSplits
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: PackageName
      , splits :: [(Version, PackageName)] -- Assumed to be in version number order
      } deriving (Eq, Ord, Show)

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: [VersionSplits]
knownVersionSplits =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = PackageName "parsec2"
      , splits = [(Version [3] [], PackageName "parsec3")] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = PackageName "quickcheck1"
      , splits = [(Version [2] [], PackageName "quickcheck2")] }
    ]

oldClckwrksSiteFlags :: Site -> [String]
oldClckwrksSiteFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ domain x ++ "/"
    , "--http-port", show port]
oldClckwrksServerFlags :: Server -> [String]
oldClckwrksServerFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ hostname x ++ ":" ++ show (port x) ++ "/"
    , "--http-port", show port]

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity = 1
    , debAction = Usage
    , dryRun = False
    , validate = False
    }

-- | Information about the mapping from cabal package names and
-- versions to debian package names and versions.  (This could be
-- broken up into smaller atoms, many of which would be attached to
-- binary packages.
newtype Atoms = Atoms {unAtoms :: Map DebAtomKey (Set DebAtom)} deriving (Eq, Show)

defaultAtoms :: Atoms
defaultAtoms =
    insertAtom Source (DebVersionSplits knownVersionSplits) $
    Atoms mempty

instance Monoid Atoms where
    -- We need mempty to actually be an empty map because we test for
    -- this in the expandAtoms recursion.
    mempty = Atoms mempty -- defaultAtoms
    mappend a b = foldAtoms insertAtom a b

instance HasAtoms Atoms where
    -- Lenses to access values in the Atoms type.  This is an old
    -- design which I plan to make private and turn into something
    -- nicer, so these will remain ugly and repetitive for now.
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

    packageDescription = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (DHPackageDescription x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
                from Source (DHPackageDescription x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHPackageDescription) x)) atoms
              where
                f Source (DHPackageDescription y) = Just y
                f _ _ = Nothing

    postInst = lens g s
        where
          g atoms = fromMaybe Map.empty $ foldAtoms from mempty atoms
              where
                from :: DebAtomKey -> DebAtom -> Maybe (Map BinPkgName Text) -> Maybe (Map BinPkgName Text)
                from Source (DHPostInst x') (Just x) | x' /= x = error $ "Conflicting postinsts: " ++ show (x, x')
                from Source (DHPostInst x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const ((singleton . (Source,) . DHPostInst) x)) atoms
              where
                f Source (DHPostInst m) = Just m
                f _ _ = Nothing

    sourcePackageName = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (SourcePackageName x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
                from Source (SourcePackageName x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . SourcePackageName) x)) atoms
              where
                f Source (SourcePackageName y) = Just y
                f _ _ = Nothing

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

    control = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (DebControl x') (Just x) | x /= x' = error $ "Conflicting control values:" ++ show (x, x')
                from Source (DebControl x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DebControl) x)) atoms
              where
                f Source (DebControl y) = Just y
                f _ _ = Nothing

lookupAtom :: (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> Atoms -> Maybe a
lookupAtom mbin from xs =
    case maxView (lookupAtoms mbin from xs) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtomDef :: (Show a, Ord a) => a -> DebAtomKey -> (DebAtom -> Maybe a) -> Atoms -> a
lookupAtomDef def key from xs = fromMaybe def $ lookupAtom key from xs

lookupAtoms :: (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> Atoms -> Set a
lookupAtoms mbin from x = maybe Set.empty (setMapMaybe from) (Map.lookup mbin (unAtoms x))

insertAtom :: DebAtomKey -> DebAtom -> Atoms -> Atoms
insertAtom mbin atom (Atoms x) = Atoms (insertWith union mbin (singleton atom) x)

insertAtoms :: Set (DebAtomKey, DebAtom) -> Atoms -> Atoms
insertAtoms s atoms =
    case maxView s of
      Nothing -> atoms
      Just ((k, a), s') -> insertAtoms s' (insertAtom k a atoms)

insertAtoms' :: [(DebAtomKey, DebAtom)] -> Atoms -> Atoms
insertAtoms' xs atoms = insertAtoms (fromList xs) atoms

hasAtom :: (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> Atoms -> Bool
hasAtom key p xs = not . Set.null . lookupAtoms key p $ xs

foldAtoms :: (DebAtomKey -> DebAtom -> r -> r) -> r -> Atoms -> r
foldAtoms f r0 (Atoms xs) = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 xs

-- | Map each atom of a HasAtoms instance to zero or more new atoms.
mapAtoms :: (DebAtomKey -> DebAtom -> Set (DebAtomKey, DebAtom)) -> Atoms -> Atoms
mapAtoms f atoms@(Atoms xs) = foldAtoms (\ k atom xs' -> insertAtoms (f k atom) xs') (Atoms xs) atoms

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms :: (DebAtomKey -> DebAtom -> Bool) -> Atoms -> (Set (DebAtomKey, DebAtom), Atoms)
partitionAtoms f deb =
    foldAtoms g (mempty, Atoms mempty) deb
    where
      g k atom (atoms, deb') =
          case f k atom of
            True -> (Set.insert (k, atom) atoms, deb')
            False -> (atoms, insertAtom k atom deb')

replaceAtoms :: (DebAtomKey -> DebAtom -> Bool) -> DebAtomKey -> DebAtom -> Atoms -> Atoms
replaceAtoms p k atom atoms = insertAtom k atom (deleteAtoms p atoms)

deleteAtoms :: (DebAtomKey -> DebAtom -> Bool) -> Atoms -> Atoms
deleteAtoms p atoms = snd (partitionAtoms p atoms)

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms' :: (Ord a) => (DebAtomKey -> DebAtom -> Maybe a) -> Atoms -> (Set a, Atoms)
partitionAtoms' f deb =
    foldAtoms g (mempty, Atoms mempty) deb
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

getMaybeSingleton :: (Eq a) => Maybe a -> (DebAtomKey -> DebAtom -> Maybe a) -> Atoms -> Maybe a
getMaybeSingleton multiple f atoms =
    foldAtoms from Nothing atoms
    where
      from k a (Just x) =
          case f k a of
            Just x' -> if x /= x' then multiple else Just x
            Nothing -> Just x
      from k a Nothing =
          f k a

getSingleton :: (Eq a) => a -> (DebAtomKey -> DebAtom -> Maybe a) -> Atoms -> a
getSingleton def f atoms = fromMaybe def (getMaybeSingleton Nothing f atoms)

compiler :: Compiler -> Atoms -> Compiler
compiler def deb =
    lookupAtomDef def Source from deb
    where from (DHCompiler x) = Just x
          from _ = Nothing

setCompiler :: Compiler -> Atoms -> Atoms
setCompiler comp atoms =
    replaceAtoms f Source (DHCompiler comp) atoms
    where
      f Source (DHCompiler _) = True
      f _ _ = False

dataDir :: FilePath -> Atoms -> FilePath
dataDir def atoms =
    maybe def dataDirectory $ getL packageDescription atoms
    where
      -- This is the directory where the files listed in the Data-Files
      -- section of the .cabal file need to be installed.
      dataDirectory :: PackageDescription -> FilePath
      dataDirectory pkgDesc =
          "usr/share" </> (pkgname ++ "-" ++ (showVersion . pkgVersion . package $ pkgDesc))
          where
            PackageName pkgname = pkgName . package $ pkgDesc

compilerVersion :: Atoms -> Maybe Version
compilerVersion deb =
    lookupAtom Source from deb
    where from (CompilerVersion x) = Just x
          from _ = Nothing

putCompilerVersion :: Version -> Atoms -> Atoms
putCompilerVersion ver deb = insertAtom Source (CompilerVersion ver) deb

putNoProfilingLibrary :: Atoms -> Atoms
putNoProfilingLibrary deb = insertAtom Source NoProfilingLibrary deb

noProfilingLibrary :: Atoms -> Bool
noProfilingLibrary deb =
    not . Set.null . lookupAtoms Source isNoProfilingLibrary $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = Just NoProfilingLibrary
      isNoProfilingLibrary _ = Nothing

putNoDocumentationLibrary :: Atoms -> Atoms
putNoDocumentationLibrary deb = insertAtom Source NoDocumentationLibrary deb

noDocumentationLibrary :: Atoms -> Bool
noDocumentationLibrary deb =
    hasAtom Source isNoDocumentationLibrary deb
    where
      isNoDocumentationLibrary NoDocumentationLibrary = Just NoDocumentationLibrary
      isNoDocumentationLibrary _ = Nothing

utilsPackageName :: Atoms -> Maybe BinPkgName
utilsPackageName deb =
    foldAtoms from Nothing deb
    where
      from Source (UtilsPackageName r) Nothing = Just r
      from Source (UtilsPackageName _) (Just _) = error "Multiple values for UtilsPackageName"
      from _ _ r = r

missingDependency :: BinPkgName -> Atoms -> Atoms
missingDependency b deb = insertAtom Source (MissingDependency b) deb

setRevision :: String -> Atoms -> Atoms
setRevision s deb =
    replaceAtoms from Source (DebRevision s) deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source (DebRevision _) = True
      from _ _ = False

revision :: Atoms -> String
revision atoms =
    getSingleton "" from atoms
    where
      from Source (DebRevision s) = Just s
      from _ _ = Nothing

setDebVersion :: DebianVersion -> Atoms -> Atoms
setDebVersion v deb =
    replaceAtoms from Source (DebVersion v) deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source (DebVersion _) = True
      from _ _ = False

debVersion :: Atoms -> Maybe DebianVersion
debVersion atoms =
    getMaybeSingleton (error "Conflicting DebVersion values") from atoms
    where
      from Source (DebVersion v) = Just v
      from _ _ = Nothing

setOmitLTDeps :: Atoms -> Atoms
setOmitLTDeps deb =
    replaceAtoms from Source OmitLTDeps deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source OmitLTDeps = True
      from _ _ = False

omitLTDeps :: Atoms -> Bool
omitLTDeps atoms =
    not $ Set.null $ fst $ partitionAtoms' from atoms
    where
      from Source OmitLTDeps = Just ()
      from _ _ = Nothing

buildDeps :: Atoms -> Set BinPkgName
buildDeps atoms =
    foldAtoms from mempty atoms
    where
      from Source (BuildDep x) s = Set.insert x s
      from _ _ s = s

putBuildDep :: BinPkgName -> Atoms -> Atoms
putBuildDep bin atoms = insertAtom Source (BuildDep bin) atoms

buildDepsIndep :: Atoms -> Set BinPkgName
buildDepsIndep atoms =
    foldAtoms from mempty atoms
    where
      from Source (BuildDepIndep x) s = Set.insert x s
      from _ _ s = s

putBuildDepIndep :: BinPkgName -> Atoms -> Atoms
putBuildDepIndep bin atoms = insertAtom Source (BuildDep bin) atoms

missingDependencies :: Atoms -> Set BinPkgName
missingDependencies atoms =
    foldAtoms from mempty atoms
    where
      from Source (MissingDependency x) s = Set.insert x s
      from _ _ s = s

extraLibMap :: Atoms -> Map.Map String (Set BinPkgName)
extraLibMap atoms =
    foldAtoms from mempty atoms
    where
      from Source (ExtraLibMapping cabal debian) m =
          Map.insertWith union cabal (singleton debian) m
      from _ _ m = m

putExtraLibMapping :: String -> BinPkgName -> Atoms -> Atoms
putExtraLibMapping cab deb atoms = insertAtom Source (ExtraLibMapping cab deb) atoms

putExecMap :: String -> BinPkgName -> Atoms -> Atoms
putExecMap cabal debian deb = insertAtom Source (ExecMapping cabal debian) deb

putPackageInfo :: PackageInfo -> Atoms -> Atoms
putPackageInfo info atoms = insertAtom Source (DebPackageInfo info) atoms

packageInfo :: PackageName -> Atoms -> Maybe PackageInfo
packageInfo (PackageName name) atoms =
    foldAtoms from Nothing atoms
    where
      from Source (DebPackageInfo p) Nothing | cabalName p == name = Just p
      from Source (DebPackageInfo p') (Just _) | cabalName p' == name = error $ "Multiple DebPackageInfo entries for " ++ name
      from _ _ x = x

execMap :: Atoms -> Map.Map String BinPkgName
execMap atoms =
    foldAtoms from mempty atoms
    where
      from Source (ExecMapping cabal debian) m =
          Map.insertWith (\ a b -> if a /= b
                                   then error $ "Conflicting mapping for Build-Tool " ++ cabal ++ ": " ++ show (a, b)
                                   else a) cabal debian m
      from _ _ m = m

putEpochMapping :: PackageName -> Int -> Atoms -> Atoms
putEpochMapping cab n atoms = insertAtom Source (EpochMapping cab n) atoms

epochMap :: Atoms -> Map.Map PackageName Int
epochMap atoms =
    foldAtoms from mempty atoms
    where
      from Source (EpochMapping name epoch) m =
          Map.insertWith (\ a b -> if a /= b
                                   then error $ "Conflicting epochs for " ++ show name ++ ": " ++ show (a, b)
                                   else a) name epoch m
      from _ _ m = m

-- | We should always call this, just as we should always apply
-- knownVersionSplits.
knownEpochMappings :: Atoms -> Atoms
knownEpochMappings = putEpochMapping (PackageName "HaXml") 1

filterMissing :: Atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    filter (/= []) (List.map (filter (\ (Rel name _ _) -> not (Set.member name (missingDependencies atoms)))) rels)

versionSplits :: Atoms -> [VersionSplits]
versionSplits atoms =
    getSingleton (error "versionSplits") from atoms
    where
      from Source (DebVersionSplits x) = Just x
      from _ _ = Nothing

putExtraDevDep :: BinPkgName -> Atoms -> Atoms
putExtraDevDep bin atoms = insertAtom Source (DevDepends bin) atoms

extraDevDeps :: Atoms -> Set BinPkgName
extraDevDeps atoms =
    foldAtoms f mempty atoms
    where
      f Source (DevDepends p) s = Set.insert p s
      f _ _ s = s

depends :: BinPkgName -> Relation -> Atoms -> Atoms
depends pkg rel atoms = insertAtom (Binary pkg) (Depends rel) atoms

conflicts :: BinPkgName -> Relation -> Atoms -> Atoms
conflicts pkg rel atoms = insertAtom (Binary pkg) (Conflicts rel) atoms

binaryPackageDeps :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageDeps p atoms =
    foldAtoms f [] atoms
    where f (Binary p') (Depends rel) rels | p == p' = [rel] : rels
          f _ _ rels = rels

binaryPackageConflicts :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageConflicts p atoms =
    foldAtoms f [] atoms
    where f (Binary p') (Conflicts rel) rels | p == p' = [rel] : rels
          f _ _ rels = rels

setSourceArchitecture :: PackageArchitectures -> Atoms -> Atoms
setSourceArchitecture x deb = insertAtom Source (DHArch x) deb

foldArchitectures :: (PackageArchitectures -> r -> r)
                 -> (BinPkgName -> PackageArchitectures -> r -> r)
                 -> r
                 -> Atoms
                 -> r
foldArchitectures sourceArch binaryArch r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHArch x) r = binaryArch p x r
      from Source (DHArch x) r = sourceArch x r
      from _ _ r = r

foldPriorities :: (PackagePriority -> r -> r)
               -> (BinPkgName -> PackagePriority -> r -> r)
               -> r
               -> Atoms
               -> r
foldPriorities sourcePriority binaryPriority r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHPriority x) r = binaryPriority p x r
      from Source (DHPriority x) r = sourcePriority x r
      from _ _ r = r

foldSections :: (Section -> r -> r)
             -> (BinPkgName -> Section -> r -> r)
               -> r
               -> Atoms
               -> r
foldSections sourceSection binarySection r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHSection x) r = binarySection p x r
      from Source (DHSection x) r = sourceSection x r
      from _ _ r = r

foldDescriptions :: (BinPkgName -> Text -> r -> r)
                 -> r
                 -> Atoms
                 -> r
foldDescriptions description r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHDescription x) r = description p x r
      from _ _ r = r

setSourcePriority :: PackagePriority -> Atoms -> Atoms
setSourcePriority x deb = insertAtom Source (DHPriority x) deb

setSourceSection :: Section -> Atoms -> Atoms
setSourceSection x deb = insertAtom Source (DHSection x) deb

setSourceDescription :: Text -> Atoms -> Atoms
setSourceDescription x deb = insertAtom Source (DHDescription x) deb

setArchitecture :: BinPkgName -> PackageArchitectures -> Atoms -> Atoms
setArchitecture k x deb = insertAtom (Binary k) (DHArch x) deb

setPriority :: BinPkgName -> PackagePriority -> Atoms -> Atoms
setPriority k x deb = insertAtom (Binary k) (DHPriority x) deb

setSection :: BinPkgName -> Section -> Atoms -> Atoms
setSection k x deb = insertAtom (Binary k) (DHSection x) deb

setDescription :: BinPkgName -> Text -> Atoms -> Atoms
setDescription k x deb = insertAtom (Binary k) (DHDescription x) deb

doExecutable :: BinPkgName -> InstallFile -> Atoms -> Atoms
doExecutable bin x deb = insertAtom (Binary bin) (DHExecutable x) deb

doServer :: BinPkgName -> Server -> Atoms -> Atoms
doServer bin x deb = insertAtom (Binary bin) (DHServer x) deb

doWebsite :: BinPkgName -> Site -> Atoms -> Atoms
doWebsite bin x deb = insertAtom (Binary bin) (DHWebsite x) deb

doBackups :: BinPkgName -> String -> Atoms -> Atoms
doBackups bin s deb =
    insertAtom (Binary bin) (DHBackups s) $
    depends bin (Rel (BinPkgName "anacron") Nothing Nothing) $
    deb

putCopyright :: Either License Text -> Atoms -> Atoms
putCopyright copy deb = insertAtom Source (DebCopyright copy) deb

copyright :: Either License Text -> Atoms -> Either License Text
copyright def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (DebCopyright x') (Just x) | x /= x' = error $ "Conflicting copyright atoms: " ++ show x ++ " vs. " ++ show x'
      from Source (DebCopyright x) _ = Just x
      from _ _ x = x

warning :: Text -> Atoms -> Atoms
warning text deb = insertAtom Source (Warning text) deb

putDebMaintainer :: NameAddr -> Atoms -> Atoms
putDebMaintainer maint atoms = insertAtom Source (DHMaintainer maint) atoms

debMaintainer :: Atoms -> Maybe NameAddr
debMaintainer atoms =
    foldAtoms from Nothing atoms
    where
      from Source (DHMaintainer x) (Just maint) | x /= maint = error $ "Conflicting maintainer values: " ++ show x ++ " vs. " ++ show maint
      from Source (DHMaintainer x) _ = Just x
      from _ _ x = x

buildDir :: FilePath -> Atoms -> FilePath
buildDir def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (BuildDir path') (Just path) | path /= path' = error $ "Conflicting buildDir atoms: " ++ show path ++ " vs. " ++ show path'
      from Source (BuildDir path') _ = Just path'
      from _ _ x = x

intermediateFile :: FilePath -> Text -> Atoms -> Atoms
intermediateFile path text atoms = insertAtom Source (DHIntermediate path text) atoms

getIntermediateFiles :: Atoms -> [(FilePath, Text)]
getIntermediateFiles atoms =
    foldAtoms from [] atoms
    where
      from Source (DHIntermediate path text) xs = (path, text) : xs
      from _ _ xs = xs

setBuildDir :: FilePath -> Atoms -> Atoms
setBuildDir path atoms =
    replaceAtoms f Source (BuildDir path) atoms
    where
      f Source (BuildDir _) = True
      f _ _ = False

cabalFlagAssignments :: Atoms -> Set (FlagName, Bool)
cabalFlagAssignments atoms =
    foldAtoms from mempty atoms
    where
      from Source (DHCabalFlagAssignments xs) ys = union xs ys
      from _ _ ys = ys

putCabalFlagAssignments :: Set (FlagName, Bool) -> Atoms -> Atoms
putCabalFlagAssignments xs atoms =
    modifyAtoms' f g atoms
    where
      f Source (DHCabalFlagAssignments xs') = Just (union xs xs')
      f _ _ = Nothing
      g xss = singleton (Source, DHCabalFlagAssignments (mconcat (Set.toList xss)))

flags :: Atoms -> Flags
flags atoms =
    fromMaybe defaultFlags $ foldAtoms from Nothing atoms
    where
      from Source (DHFlags _) (Just _) = error "Conflicting Flag atoms"
      from Source (DHFlags fs) _ = Just fs
      from _ _ x = x

mapFlags :: (Flags -> Flags) -> Atoms -> Atoms
mapFlags f atoms =
    modifyAtoms' g h atoms
    where
      g Source (DHFlags x) = Just x
      g _ _ = Nothing
      h xs = singleton (Source, DHFlags (f (case maxView xs of
                                              Just (_, s) | not (Set.null s) -> error "Conflicting Flag atoms"
                                              Just (x, _) -> x
                                              Nothing -> defaultFlags)))

watchAtom :: PackageName -> Text
watchAtom (PackageName pkgname) =
    pack $ "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
           "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
           " \\\n    ([\\d\\.]*\\d)/\n"

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: [(BinPkgName, BinPkgName)] -> BinPkgName -> Atoms -> Atoms
tightDependencyFixup [] _ deb = deb
tightDependencyFixup pairs p deb =
    insertAtom Source atom deb
    where
      atom = DebRulesFragment
              (unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

{-
setSourceDebDescription' :: SourceDebDescription -> Atoms -> Atoms
setSourceDebDescription' d x = modifySourceDebDescription' (const d) x

modifySourceDebDescription :: (SourceDebDescription -> SourceDebDescription) -> Atoms -> Atoms
modifySourceDebDescription f deb = modL control (fmap f) deb
-}

modControl :: (SourceDebDescription -> SourceDebDescription) -> Atoms -> Atoms
modControl = modifySourceDebDescription'

modifySourceDebDescription' :: (SourceDebDescription -> SourceDebDescription) -> Atoms -> Atoms
modifySourceDebDescription' f deb = modL control (Just . f . fromMaybe newSourceDebDescription) deb

modifySourceDebDescription'' :: (SourceDebDescription -> SourceDebDescription) -> Atoms -> Atoms
modifySourceDebDescription'' f deb = modL control (Just . f . fromMaybe (error "modifySourceDebDescription")) deb

install :: BinPkgName -> FilePath -> FilePath -> Atoms -> Atoms
install p path d atoms = insertAtom (Binary p) (DHInstall path d) atoms

installData :: BinPkgName -> FilePath -> FilePath -> Atoms -> Atoms
installData p path dest atoms = insertAtom (Binary p) (DHInstallData path dest) atoms

getInstalls :: Atoms -> Map BinPkgName (Set (FilePath, FilePath))
getInstalls atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstall src dst) mp = Map.insertWith Set.union p (singleton (src, dst)) mp
      from _ _ mp = mp

installInit :: BinPkgName -> Text -> Atoms -> Atoms
installInit p text atoms = insertAtom (Binary p) (DHInstallInit text) atoms

logrotateStanza :: BinPkgName -> Text -> Atoms -> Atoms
logrotateStanza p text atoms = insertAtom (Binary p) (DHLogrotateStanza text) atoms

getPostInsts :: Atoms -> Map BinPkgName Text
getPostInsts atoms =
    fromMaybe Map.empty $ foldAtoms from Nothing atoms
    where
      from :: DebAtomKey -> DebAtom -> Maybe (Map BinPkgName Text) -> Maybe (Map BinPkgName Text)
      from Source (DHPostInst mp') (Just mp) | mp /= mp' = error "Multiple PostInst maps"
      from Source (DHPostInst mp) _ = Just mp
      from _ _ x = x

getPostInst :: Atoms -> BinPkgName -> Maybe Text
getPostInst atoms b = Map.lookup b (getPostInsts atoms)

putPostInst :: BinPkgName -> Text -> Atoms -> Atoms
putPostInst p text atoms =
    modL postInst (\ m -> Map.insertWith f p text m) atoms
    where
      f a b = if a == b then a else error $ "putPostInst: conflicting postinsts: " ++ show (a, b)

modPostInst :: BinPkgName -> (Maybe Text -> Maybe Text) -> Atoms -> Atoms
modPostInst p f atoms = modL postInst (\ m -> case Map.lookup p m of
                                                Nothing -> case f Nothing of
                                                             Just t' -> Map.insert p t' m
                                                             Nothing -> m
                                                Just t -> update (f . Just) p m) atoms

postRm :: BinPkgName -> Text -> Atoms -> Atoms
postRm p text atoms = insertAtom (Binary p) (DHPostRm text) atoms

preInst :: BinPkgName -> Text -> Atoms -> Atoms
preInst p text atoms = insertAtom (Binary p) (DHPreInst text) atoms

preRm :: BinPkgName -> Text -> Atoms -> Atoms
preRm p text atoms = insertAtom (Binary p) (DHPreRm text) atoms

link :: BinPkgName -> FilePath -> FilePath -> Atoms -> Atoms
link p path text atoms = insertAtom (Binary p) (DHLink path text) atoms

file :: BinPkgName -> FilePath -> Text -> Atoms -> Atoms
file p path text atoms = insertAtom (Binary p) (DHFile path text) atoms 

installDir :: BinPkgName -> FilePath -> Atoms -> Atoms
installDir p path atoms = insertAtom (Binary p) (DHInstallDir path) atoms

rulesFragment :: Text -> Atoms -> Atoms
rulesFragment text atoms = insertAtom Source (DebRulesFragment text) atoms

installCabalExec :: BinPkgName -> String -> FilePath -> Atoms -> Atoms
installCabalExec p name d atoms = insertAtom (Binary p) (DHInstallCabalExec name d) atoms

installCabalExecTo :: BinPkgName -> String -> FilePath -> Atoms -> Atoms
installCabalExecTo p name dest atoms = insertAtom (Binary p) (DHInstallCabalExecTo name dest) atoms

installTo :: BinPkgName -> FilePath -> FilePath -> Atoms -> Atoms
installTo p from dest atoms = insertAtom (Binary p) (DHInstallTo from dest) atoms

one :: (Eq a, Show a) => a -> a -> a
one old new | old /= new = error $ "Conflict: " ++ show (old, new)
one old _ = old

getInstallDirs :: Atoms -> Map BinPkgName (Set FilePath)
getInstallDirs atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstallDir d) mp = Map.insertWith Set.union p (singleton d) mp
      from _ _ mp = mp

getInstallInits :: Atoms -> Map BinPkgName Text
getInstallInits atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstallInit t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getLogrotateStanzas :: Atoms -> Map BinPkgName (Set Text)
getLogrotateStanzas atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHLogrotateStanza t) mp = Map.insertWith Set.union p (singleton t) mp
      from _ _ mp = mp

getLinks :: Atoms -> Map BinPkgName (Set (FilePath, FilePath))
getLinks atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHLink loc txt) mp = Map.insertWith Set.union p (singleton (loc, txt)) mp
      from _ _ mp = mp

getPostRms :: Atoms -> Map BinPkgName Text
getPostRms atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPostRm t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getPreInsts :: Atoms -> Map BinPkgName Text
getPreInsts atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPreInst t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getPreRms :: Atoms -> Map BinPkgName Text
getPreRms atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPreRm t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

foldExecs :: (BinPkgName -> Site -> r -> r)
          -> (BinPkgName -> Server -> r -> r)
          -> (BinPkgName -> String -> r -> r)
          -> (BinPkgName -> InstallFile -> r -> r)
          -> r
          -> Atoms
          -> r
foldExecs site serv backup exec r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHWebsite x) r = site p x r
      from (Binary p) (DHServer x) r = serv p x r
      from (Binary p) (DHBackups x) r = backup p x r
      from (Binary p) (DHExecutable x) r = exec p x r
      from _ _ r = r

finalizeAtoms :: Atoms -> Atoms
finalizeAtoms atoms =
    expanded
    where
      expanded = foldAtoms insertAtom atoms (newer atoms)

      newer :: Atoms -> Atoms
      newer x =
          let x' = foldAtoms next mempty x in
          -- I don't understand why folding an empty map causes an infinite recursion, but it does
          if Map.null (unAtoms x')
          then x'
          else foldAtoms insertAtom x' (newer x')

      builddir = buildDir "dist-ghc/build" atoms
      datadir = dataDir (error "foldAtomsFinalized") atoms

      -- Fully expand a single atom
      next :: DebAtomKey -> DebAtom -> Atoms -> Atoms
      next (Binary b) (DHApacheSite domain' logdir text) r =
          link b ("/etc/apache2/sites-available/" ++ domain') ("/etc/apache2/sites-enabled/" ++ domain') .
          installDir b logdir .
          file b ("/etc/apache2/sites-available" </> domain') text $
          r
      next (Binary b) (DHInstallCabalExec name dst) r =
          install b (builddir </> name </> name) dst $
          r
      next (Binary b) (DHInstallCabalExecTo n d) r =
          rulesFragment (unlines [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                 , "\tinstall -Dp " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ]) $
          r
      next (Binary b) (DHInstallData s d) r =
          if takeFileName s == takeFileName d
          then install b s (datadir </> makeRelative "/" (takeDirectory d)) r
          else installTo b s (datadir </> makeRelative "/" d) r
      next (Binary p) (DHInstallTo s d) r =
          rulesFragment (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                 , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]) $
          r
      next (Binary p) (DHFile path s) r =
          intermediateFile tmpPath s . install p tmpPath destDir' $ r
          where
            (destDir', destName') = splitFileName path
            tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
            tmpPath = tmpDir </> destName'
      next (Binary b) (DHWebsite x) r =
          siteAtoms b x $
          r
      next (Binary b) (DHServer x) r =
          serverAtoms b x False $
          r
      next (Binary b) (DHBackups s) r =
          backupAtoms b s $
          r
      next (Binary b) (DHExecutable x) r =
          execAtoms b x $
          r
      next _ _ r = r

siteAtoms :: BinPkgName -> Site -> Atoms -> Atoms
siteAtoms b site =
    installDir b "/etc/apache2/sites-available" .
    link b ("/etc/apache2/sites-available/" ++ domain site) ("/etc/apache2/sites-enabled/" ++ domain site) .
    file b ("/etc/apache2/sites-available" </> domain site) apacheConfig .
    installDir b (apacheLogDirectory b) .
    logrotateStanza b (unlines $              [ pack (apacheAccessLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}"]) .
    logrotateStanza b (unlines $              [ pack (apacheErrorLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}" ]) .
    serverAtoms b (server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " <> pack (serverAdmin site)
                   , "    ServerName www." <> pack (domain site)
                   , "    ServerAlias " <> pack (domain site)
                   , ""
                   , "    ErrorLog " <> pack (apacheErrorLog b)
                   , "    CustomLog " <> pack (apacheAccessLog b) <> " combined"
                   , ""
                   , "    ProxyRequests Off"
                   , "    AllowEncodedSlashes NoDecode"
                   , ""
                   , "    <Proxy *>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                Deny from all"
                   , "                #Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    <Proxy http://127.0.0.1:" <> port' <> "/*>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                #Deny from all"
                   , "                Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    SetEnv proxy-sendcl 1"
                   , ""
                   , "    ProxyPass / http://127.0.0.1:" <> port' <> "/ nocanon"
                   , "    ProxyPassReverse / http://127.0.0.1:" <> port' <> "/"
                   , "</VirtualHost>" ]
      port' = pack (show (port (server site)))

serverAtoms :: BinPkgName -> Server -> Bool -> Atoms -> Atoms
serverAtoms b server isSite =
    putPostInst b debianPostinst .
    installInit b debianInit .
    serverLogrotate' b .
    execAtoms b exec
    where
      exec = installFile server
      debianInit =
          unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" <> pack (destName exec) <> " || exit 0"
                   , "    log_begin_msg \"Starting " <> pack (destName exec) <> "...\""
                   , "    mkdir -p " <> pack (databaseDirectory b)
                   , "    " <> startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " <> pack (destName exec) <> "...\""
                   , "    " <> stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = pack $ showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
      stopCommand = pack $ showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server /= "" then ["--retry=" ++ retry server ] else []
      serverOptions = serverFlags server ++ commonServerOptions
      -- Without these, happstack servers chew up CPU even when idle
      commonServerOptions = ["+RTS", "-IO", "-RTS"]

      debianPostinst =
          unlines $
                   ([ "#!/bin/sh"
                    , ""
                    , "case \"$1\" in"
                    , "  configure)" ] ++
                    (if isSite
                     then [ "    # Apache won't start if this directory doesn't exist"
                          , "    mkdir -p " <> pack (apacheLogDirectory b)
                          , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
                          , "    /usr/sbin/a2enmod proxy"
                          , "    /usr/sbin/a2enmod proxy_http"
                          , "    service apache2 restart" ]
                     else []) ++
                    [ "    service " <> pack (show (pretty b)) <> " start"
                    , "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate' :: BinPkgName -> Atoms -> Atoms
serverLogrotate' b =
    logrotateStanza b (unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]) .
    logrotateStanza b (unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])

backupAtoms :: BinPkgName -> String -> Atoms -> Atoms
backupAtoms b name =
    putPostInst b (unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ]) .
    execAtoms b (InstallFile { execName = name
                              , destName = name
                              , sourceDir = Nothing
                              , destDir = Just "/etc/cron.hourly" })

execAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
execAtoms b ifile r =
    rulesFragment (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp")) .
    fileAtoms b ifile $
    r

fileAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
fileAtoms b installFile r =
    fileAtoms' b (sourceDir installFile) (execName installFile) (destDir installFile) (destName installFile) r

fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> Atoms -> Atoms
fileAtoms' b sourceDir execName destDir destName r =
    case (sourceDir, execName == destName) of
      (Nothing, True) -> installCabalExec b execName d r
      (Just s, True) -> install b (s </> execName) d r
      (Nothing, False) -> installCabalExecTo b execName (d </> destName) r
      (Just s, False) -> installTo b (s </> execName) (d </> destName) r
    where
      d = fromMaybe "usr/bin" destDir

foldCabalExecs :: (String -> r -> r) -> r -> Atoms -> r
foldCabalExecs f r0 atoms =
    foldAtoms g r0 atoms
    where
      g (Binary _) (DHInstallCabalExec name _) r = f name r
      g (Binary _) (DHInstallCabalExecTo name _) r = f name r
      g (Binary p) (DHExecutable i@(InstallFile {})) r =
          let d = fromMaybe "usr/bin" (destDir i) in
          case (sourceDir i, execName i == destName i) of
            (Nothing, True) -> g (Binary p) (DHInstallCabalExec (execName i) d) r
            (Just s, True) ->  g (Binary p) (DHInstall (s </> execName i) d) r
            (Nothing, False) ->  g (Binary p) (DHInstallCabalExecTo (execName i) (d </> destName i)) r
            (Just s, False) ->  g (Binary p) (DHInstallTo (s </> execName i) (d </> destName i)) r
      g _ _ r = r

foldCabalDatas :: (FilePath -> r -> r) -> r -> Atoms -> r
foldCabalDatas f r0 atoms =
    foldAtoms g r0 atoms
    where
      g (Binary _) (DHInstall path _) r = f path r
      g (Binary _) (DHInstallTo path _) r = f path r
      g (Binary _) (DHInstallData path _) r = f path r
      g _ _ r = r
