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
    -- * DependencyHint getter and setters
    , filterMissing
    , knownEpochMappings
    , binaryPackageDeps
    , binaryPackageConflicts
    , setSourcePriority
    , setSourceSection
    , setSourceDescription
    , setPriority
    , setSection
    , setDescription
    , doExecutable
    , doServer
    , doWebsite
    , doBackups
    , watchAtom
    , tightDependencyFixup
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
    , foldPriorities
    , foldSections
    , foldDescriptions
    , finalizeAtoms
    , foldCabalDatas
    , foldCabalExecs
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Lazy (Lens, setL)
import Data.Map as Map (Map, fold, fromList, foldWithKey)
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
import Data.Map as Map (lookup, insertWith, empty, null, insert, update)
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
insertAtoms' xs atoms = insertAtoms (Set.fromList xs) atoms

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

data PackageInfo = PackageInfo { cabalName :: PackageName
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

-- | We should always call this, just as we should always apply
-- knownVersionSplits.
knownEpochMappings :: Map PackageName Int
knownEpochMappings =
    Map.fromList [(PackageName "HaXml", 1)]

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
    setL epochMap knownEpochMappings $
    setL versionSplits knownVersionSplits $
    Atoms mempty

instance Monoid Atoms where
    -- We need mempty to actually be an empty map because we test for
    -- this in the expandAtoms recursion.
    mempty = Atoms mempty -- defaultAtoms
    mappend a b = foldAtoms insertAtom a b

class (Monoid atoms, Show atoms {- Show instance for debugging only -}) => HasAtoms atoms where
    rulesHead :: Lens atoms (Maybe Text)
    packageDescription :: Lens atoms (Maybe PackageDescription)
    postInst :: Lens atoms (Map BinPkgName Text)
    compat :: Lens atoms (Maybe Int)
    sourceFormat :: Lens atoms (Maybe SourceFormat)
    watch :: Lens atoms (Maybe Text)
    sourcePackageName :: Lens atoms (Maybe SrcPkgName)
    changelog :: Lens atoms (Maybe ChangeLog)
    comments :: Lens atoms (Maybe [[Text]]) -- ^ Comment entries for the latest changelog entry (DebLogComments [[Text]])
    control :: Lens atoms SourceDebDescription
    compilerVersion :: Lens atoms (Maybe Version)
    compiler :: Lens atoms (Maybe Compiler)
    dataDir :: Lens atoms (Maybe FilePath)
    noProfilingLibrary :: Lens atoms Bool
    noDocumentationLibrary :: Lens atoms Bool
    utilsPackageName :: Lens atoms (Maybe BinPkgName)
    missingDependencies :: Lens atoms (Set BinPkgName)
    flags :: Lens atoms Flags

    buildDir :: Lens atoms (Maybe FilePath)
    revision :: Lens atoms (Maybe String)
    debVersion :: Lens atoms (Maybe DebianVersion)
    packageInfo :: Lens atoms (Map PackageName PackageInfo)
    versionSplits :: Lens atoms [VersionSplits]
    sourceArchitecture :: Lens atoms (Maybe PackageArchitectures)
    binaryArchitectures :: Lens atoms (Map BinPkgName PackageArchitectures)
    maintainer :: Lens atoms (Maybe NameAddr)
    copyright :: Lens atoms (Maybe (Either License Text))

    rulesFragments :: Lens atoms (Set Text)
    omitLTDeps :: Lens atoms Bool
    buildDeps :: Lens atoms (Set BinPkgName)
    buildDepsIndep :: Lens atoms (Set BinPkgName)
    extraLibMap :: Lens atoms (Map String (Set BinPkgName))
    execMap :: Lens atoms (Map String BinPkgName)
    epochMap :: Lens atoms (Map PackageName Int)
    extraDevDeps :: Lens atoms (Set BinPkgName)
    intermediateFiles :: Lens atoms (Set (FilePath, Text))
    warning :: Lens atoms (Set Text)
    cabalFlagAssignments :: Lens atoms (Set (FlagName, Bool))

    depends :: Lens atoms (Map BinPkgName (Set Relation)) -- This should be [[Relation]] for full generality, or Set (Set Relation)
    conflicts :: Lens atoms (Map BinPkgName (Set Relation)) -- This too

    -- DHApacheSite String FilePath Text
    -- DHLogrotateStanza Text
    -- DHLink FilePath FilePath
    -- DHPostRm Text
    -- DHPreInst Text
    -- DHPreRm Text
    -- DHArch PackageArchitectures
    -- DHPriority PackagePriority
    -- DHSection Section
    -- DHDescription Text
    -- DHInstall FilePath FilePath
    -- DHInstallTo FilePath FilePath
    -- DHInstallData FilePath FilePath
    -- DHFile FilePath Text
    -- DHInstallCabalExec String FilePath
    -- DHInstallCabalExecTo String FilePath
    -- DHInstallDir FilePath
    -- DHInstallInit Text
    -- DHExecutable InstallFile
    -- DHServer Server
    -- DHWebsite Site
    -- DHBackups String

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
          g atoms = fromMaybe newSourceDebDescription $ foldAtoms from Nothing atoms
              where
                from Source (DebControl x') (Just x) | x /= x' = error $ "Conflicting control values:" ++ show (x, x')
                from Source (DebControl x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const ((singleton . (Source,) . DebControl) x)) atoms
              where
                f Source (DebControl y) = Just y
                f _ _ = Nothing

    compilerVersion = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (CompilerVersion x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
                from Source (CompilerVersion x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . CompilerVersion) x)) atoms
              where
                f Source (CompilerVersion y) = Just y
                f _ _ = Nothing

    compiler = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (DHCompiler x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
                from Source (DHCompiler x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . DHCompiler) x)) atoms
              where
                f Source (DHCompiler y) = Just y
                f _ _ = Nothing

    dataDir = lens g s
        where
          g atoms =
              fmap (\ p -> let PackageName pkgname = pkgName. package $ p in
                           "usr/share" </> (pkgname ++ "-" ++ (showVersion . pkgVersion . package $ p))) (getL packageDescription atoms)
          s _ _ = error "setL dataDir"

    noProfilingLibrary = lens g s
        where
          g atoms = foldAtoms from False atoms
              where
                from Source NoProfilingLibrary _ = True
                from _ _ x = x
          s x atoms = (if x then insertAtom Source NoProfilingLibrary else id) (deleteAtoms p atoms)
              where
                p Source NoProfilingLibrary = True
                p _ _ = False

    noDocumentationLibrary = lens g s
        where
          g atoms = foldAtoms from False atoms
              where
                from Source NoDocumentationLibrary _ = True
                from _ _ x = x
          s x atoms = (if x then insertAtom Source NoProfilingLibrary else id) (deleteAtoms p atoms)
              where
                p Source NoDocumentationLibrary = True
                p _ _ = False

    utilsPackageName = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (UtilsPackageName x') (Just x) | x /= x' = error $ "Conflicting compat values:" ++ show (x, x')
                from Source (UtilsPackageName x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . UtilsPackageName) x)) atoms
              where
                f Source (UtilsPackageName y) = Just y
                f _ _ = Nothing

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

    buildDir = lens g s
        where
          g atoms = foldAtoms from Nothing atoms
              where
                from Source (BuildDir x') (Just x) | x /= x' = error $ "Conflicting rulesHead values:" ++ show (x, x')
                from Source (BuildDir x) _ = Just x
                from _ _ x = x
          s x atoms = modifyAtoms' f (const (maybe Set.empty (singleton . (Source,) . BuildDir) x)) atoms
              where
                f Source (BuildDir y) = Just y
                f _ _ = Nothing

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

    versionSplits = lens g s
        where
          g atoms = foldAtoms from [] atoms
              where
                from Source (DebVersionSplits xs') xs = xs ++ xs'
                from _ _ xs = xs
          s x atoms = insertAtom Source (DebVersionSplits x) (deleteAtoms p atoms)
              where
                p Source (DebVersionSplits _) = True
                p _ _ = False

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

    omitLTDeps = lens g s
        where
          g atoms = foldAtoms from False atoms
              where
                from Source OmitLTDeps _ = True
                from _ _ x = x
          s x atoms = (if x then insertAtom Source OmitLTDeps else id) (deleteAtoms p atoms)
              where
                p Source OmitLTDeps = True
                p _ _ = False
    buildDeps = lens g s
        where
          g atoms = foldAtoms from Set.empty atoms
              where
                from Source (BuildDep d) x = Set.insert d x
                from _ _ x = x
          s x atoms = Set.fold (\ d atoms' -> insertAtom Source (BuildDep d) atoms') (deleteAtoms p atoms) x
              where
                p Source (BuildDep _) = True
                p _ _ = False
    buildDepsIndep = lens g s
        where
          g atoms = foldAtoms from Set.empty atoms
              where
                from Source (BuildDepIndep d) x = Set.insert d x
                from _ _ x = x
          s x atoms = Set.fold (\ d atoms' -> insertAtom Source (BuildDepIndep d) atoms') (deleteAtoms p atoms) x
              where
                p Source (BuildDepIndep _) = True
                p _ _ = False
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
    execMap = lens g s
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from Source (ExecMapping cabal debian) x = Map.insertWith (error "Conflict in execMap") cabal debian x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ cabal debian atoms' -> insertAtom Source (ExecMapping cabal debian) atoms') (deleteAtoms p atoms) x
              where
                p Source (ExecMapping _ _) = True
                p _ _ = False
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

    depends = lens g s -- Lens atoms (Map BinPkgName (Set Relation)) -- Depends Relation
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (Depends rel) x = Map.insertWith union b (singleton rel) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Depends rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
              where
                p (Binary _) (Depends _) = True
                p _ _ = False

    conflicts = lens g s -- Lens atoms (Map BinPkgName (Set Relation)) -- Conflicts Relation
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (Conflicts rel) x = Map.insertWith union b (singleton rel) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b rels atoms' -> Set.fold (\ rel atoms'' -> insertAtom (Binary b) (Conflicts rel) atoms'') atoms' rels) (deleteAtoms p atoms) x
              where
                p (Binary _) (Conflicts _) = True
                p _ _ = False

binaryPackageDeps :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageDeps b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL depends atoms))

binaryPackageConflicts :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageConflicts b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL conflicts atoms))

filterMissing :: Atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    filter (/= []) (List.map (filter (\ (Rel name _ _) -> not (Set.member name (getL missingDependencies atoms)))) rels)

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
    modL depends (Map.insertWith union bin (singleton (Rel (BinPkgName "anacron") Nothing Nothing))) $
    deb

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

      builddir = fromMaybe {-(error "finalizeAtoms: no buildDir")-} "dist-ghc/build" $ getL buildDir atoms
      datadir = fromMaybe (error "finalizeAtoms: no dataDir") $ getL dataDir atoms

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
          modL rulesFragments (Set.insert (unlines [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                   , "\tinstall -Dp " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) $
          r
      next (Binary b) (DHInstallData s d) r =
          if takeFileName s == takeFileName d
          then install b s (datadir </> makeRelative "/" (takeDirectory d)) r
          else installTo b s (datadir </> makeRelative "/" d) r
      next (Binary p) (DHInstallTo s d) r =
          modL rulesFragments (Set.insert (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                   , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) $
          r
      next (Binary p) (DHFile path s) r =
          modL intermediateFiles (Set.insert (tmpPath, s)) . install p tmpPath destDir' $ r
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
    modL rulesFragments (Set.insert (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) .
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
