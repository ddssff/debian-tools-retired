{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Atoms
    ( Atoms
    , HasAtoms(..)
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Lazy (Lens, lens, getL, modL)
import Data.Map as Map (Map, fold, foldWithKey, insertWith, empty, insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set, maxView, empty, union, singleton, fold, insert)
import Data.Text (Text)
import Data.Version (Version, showVersion)
import Debian.Changes (ChangeLog)
import Debian.Debianize.ControlFile (SourceDebDescription, newSourceDebDescription)
import Debian.Debianize.Types (PackageInfo(..), Site(..), Server(..), InstallFile(..), VersionSplits(..), DebAction(..))
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, SourceFormat, PackagePriority, Section)
import Debian.Relation (SrcPkgName, BinPkgName, Relation(..))
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..))
import Distribution.PackageDescription as Cabal (PackageDescription(package), FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init, unlines, log)
import System.FilePath ((</>))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- All the internals of this module is a steaming pile of poo, except
-- for the stuff that is exported.

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

    -- From here down are atoms to be associated with a Debian binary
    -- package.  This could be done with more type safety, separate
    -- maps for the Source atoms and the Binary atoms.
    | DHApacheSite String FilePath Text           -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    | DHLogrotateStanza Text		          -- ^ Add a stanza of a logrotate file to the binary package
    | DHLink FilePath FilePath          	  -- ^ Create a symbolic link in the binary package
    | DHPostInst Text			 	  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
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
      verbosity_ :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , dryRun_ :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , validate_ :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , debAction_ :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    } deriving (Eq, Ord, Show)

-- | Information about the mapping from cabal package names and
-- versions to debian package names and versions.
newtype Atoms = Atoms (Map DebAtomKey (Set DebAtom)) deriving (Eq, Show)

instance Monoid Atoms where
    -- We need mempty to actually be an empty map because we test for
    -- this in the expandAtoms recursion.
    mempty = Atoms mempty -- defaultAtoms
    mappend a b = foldAtoms insertAtom a b

class (Monoid atoms, Show atoms {- Show instance for debugging only -}) => HasAtoms atoms where

    -- Modes of operation
    flags :: Lens atoms Flags
    verbosity :: Lens atoms Int
    dryRun :: Lens atoms Bool
    validate :: Lens atoms Bool
    debAction :: Lens atoms DebAction
    compilerVersion :: Lens atoms (Maybe Version)
    warning :: Lens atoms (Set Text)

    -- Cabal info
    packageDescription :: Lens atoms (Maybe PackageDescription)
    buildDir :: Lens atoms (Maybe FilePath)
    dataDir :: Lens atoms (Maybe FilePath)
    compiler :: Lens atoms (Maybe Compiler)
    extraLibMap :: Lens atoms (Map String (Set BinPkgName))
    execMap :: Lens atoms (Map String BinPkgName)
    cabalFlagAssignments :: Lens atoms (Set (FlagName, Bool))

    -- Global debian info
    versionSplits :: Lens atoms [VersionSplits]
    epochMap :: Lens atoms (Map PackageName Int)

    -- High level information about the debianization
    apacheSite :: Lens atoms (Map BinPkgName (String, FilePath, Text))
    description :: Lens atoms (Map BinPkgName Text)
    executable :: Lens atoms (Map BinPkgName InstallFile)
    serverInfo :: Lens atoms (Map BinPkgName Server)
    website :: Lens atoms (Map BinPkgName Site)
    backups :: Lens atoms (Map BinPkgName String)

    -- Lower level hints about the debianization
    missingDependencies :: Lens atoms (Set BinPkgName)
    utilsPackageName :: Lens atoms (Maybe BinPkgName)
    sourcePackageName :: Lens atoms (Maybe SrcPkgName)
    revision :: Lens atoms (Maybe String)
    debVersion :: Lens atoms (Maybe DebianVersion)
    maintainer :: Lens atoms (Maybe NameAddr)
    packageInfo :: Lens atoms (Map PackageName PackageInfo)
    omitLTDeps :: Lens atoms Bool
    noProfilingLibrary :: Lens atoms Bool
    noDocumentationLibrary :: Lens atoms Bool
    copyright :: Lens atoms (Maybe (Either License Text))
    sourceArchitecture :: Lens atoms (Maybe PackageArchitectures)
    binaryArchitectures :: Lens atoms (Map BinPkgName PackageArchitectures)
    sourcePriority :: Lens atoms (Maybe PackagePriority)
    binaryPriorities :: Lens atoms (Map BinPkgName PackagePriority)
    sourceSection :: Lens atoms (Maybe Section)
    binarySections :: Lens atoms (Map BinPkgName Section)

    -- Debian dependency info
    buildDeps :: Lens atoms (Set BinPkgName) -- ^ Build dependencies
    buildDepsIndep :: Lens atoms (Set BinPkgName) -- ^ Architecture independent
    extraDevDeps :: Lens atoms (Set BinPkgName) -- ^ Extra install dependencies for the devel library
    depends :: Lens atoms (Map BinPkgName (Set Relation)) -- ^ Install dependencies
    -- This should be [[Relation]] for full generality, or Set (Set Relation)
    conflicts :: Lens atoms (Map BinPkgName (Set Relation)) -- ^ Install conflicts
    -- We should support all the other dependency fields - provides, replaces, etc.

    -- Debianization files and file fragments
    rulesHead :: Lens atoms (Maybe Text) -- ^ The beginning of the rules file
    rulesFragments :: Lens atoms (Set Text) -- ^ Additional fragments of the rules file
    postInst :: Lens atoms (Map BinPkgName Text)
    postRm :: Lens atoms (Map BinPkgName Text)
    preInst :: Lens atoms (Map BinPkgName Text)
    preRm :: Lens atoms (Map BinPkgName Text)
    compat :: Lens atoms (Maybe Int)
    sourceFormat :: Lens atoms (Maybe SourceFormat)
    watch :: Lens atoms (Maybe Text)
    changelog :: Lens atoms (Maybe ChangeLog)
    comments :: Lens atoms (Maybe [[Text]]) -- ^ Comment entries for the latest changelog entry (DebLogComments [[Text]])
    control :: Lens atoms SourceDebDescription
    logrotateStanza :: Lens atoms (Map BinPkgName (Set Text))
    link :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath)))
    install :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath))) -- ^ Install files into directories
    installTo :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath))) -- ^ Rename and install files
    installData :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath))) -- ^ Install files into the package data directory
    file :: Lens atoms (Map BinPkgName (Set (FilePath, Text))) -- ^ Create a file in the deb with the given text
    installCabalExec :: Lens atoms (Map BinPkgName (Set (String, FilePath))) -- ^ Install a cabal executable
    installCabalExecTo :: Lens atoms (Map BinPkgName (Set (String, FilePath))) -- ^ Rename and install a cabal executable
    installDir :: Lens atoms (Map BinPkgName (Set FilePath)) -- ^ Create directories in the package
    installInit :: Lens atoms (Map BinPkgName Text) -- ^ Create an /etc/init.d file in the package
    intermediateFiles :: Lens atoms (Set (FilePath, Text)) -- ^ Create a file in the debianization.  This is used to implement the file lens above.

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
          g atoms = foldAtoms from mempty atoms
              where
                from :: DebAtomKey -> DebAtom -> Map BinPkgName Text -> Map BinPkgName Text
                from (Binary b) (DHPostInst t) x = Map.insertWith (error "Conflicting postInsts") b t x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b t atoms' -> insertAtom (Binary b) (DHPostInst t) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHPostInst _) = True
                p _ _ = False

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
    verbosity = lens (\ a -> verbosity_ (getL flags a)) (\ b a -> modL flags (\ x -> x {verbosity_ = b}) a)
    dryRun = lens (\ a -> dryRun_ (getL flags a)) (\ b a -> modL flags (\ x -> x {dryRun_ = b}) a)
    validate = lens (\ a -> validate_ (getL flags a)) (\ b a -> modL flags (\ x -> x {validate_ = b}) a)
    debAction = lens (\ a -> debAction_ (getL flags a)) (\ b a -> modL flags (\ x -> x {debAction_ = b}) a)

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

    apacheSite = lens g s -- :: Lens atoms (Map BinPkgName (String, FilePath, Text))    -- DHApacheSite String FilePath Text
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHApacheSite dom log text) x = Map.insertWith (error "backups") b (dom, log, text) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b (dom, log, text) atoms' -> insertAtom (Binary b) (DHApacheSite dom log text) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHApacheSite _ _ _) = True
                p _ _ = False
    logrotateStanza = lens g s -- :: Lens atoms (Map BinPkgName (Set Text))    -- DHLogrotateStanza Text
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHLogrotateStanza r) x = Map.insertWith Set.union b (singleton r) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b ts atoms'-> Set.fold (\ t atoms'' -> insertAtom (Binary b) (DHLogrotateStanza t) atoms'') atoms' ts) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHLogrotateStanza _) = True
                p _ _ = False
    binaryPriorities = lens g s -- :: Lens atoms (Map BinPkgName PackagePriority)    -- DHPriority PackagePriority
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHPriority p) x = Map.insertWith (error "priorities") b p x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b p atoms'-> insertAtom (Binary b) (DHPriority p) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHPriority _) = True
                p _ _ = False
    binarySections = lens g s -- :: Lens atoms (Map BinPkgName PackageSection)    -- DHSection (Maybe Section)
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHSection p) x = Map.insertWith (error "sections") b p x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b p atoms'-> insertAtom (Binary b) (DHSection p) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHSection _) = True
                p _ _ = False
    description = lens g s -- :: Lens atoms (Map BinPkgName Text)    -- DHDescription Text
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHDescription d) x = Map.insertWith (error "description") b d x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHDescription y) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHDescription _) = True
                p _ _ = False
    executable = lens g s -- :: Lens atoms (Map BinPkgName InstallFile)    -- DHExecutable InstallFile
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHExecutable f) x = Map.insertWith (error "executable") b f x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHExecutable y) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHExecutable _) = True
                p _ _ = False
    serverInfo = lens g s -- :: Lens atoms (Map BinPkgName Server)-- DHServer Server
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHServer s) x = Map.insertWith (error "server") b s x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHServer y) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHServer _) = True
                p _ _ = False
    website = lens g s -- :: Lens atoms (Map BinPkgName Site)    -- DHWebsite Site
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHWebsite s) x = Map.insertWith (error "website") b s x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHWebsite y) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHWebsite _) = True
                p _ _ = False
    backups = lens g s -- :: Lens atoms (Map BinPkgName String)    -- DHBackups String
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHBackups s) x = Map.insertWith (error "backups") b s x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b y atoms'-> insertAtom (Binary b) (DHBackups y) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHBackups _) = True
                p _ _ = False

    link = lens g s -- :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath)))    -- DHLink FilePath FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHLink loc txt) x = Map.insertWith Set.union b (singleton (loc, txt)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (loc, txt) atoms'' -> insertAtom (Binary b) (DHLink loc txt) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHLink _ _) = True
                p _ _ = False
    install = lens g s -- :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath)))    -- DHInstall FilePath FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstall src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstall src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstall _ _) = True
                p _ _ = False
    installTo = lens g s -- :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath)))    -- DHInstallTo FilePath FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallTo src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstallTo src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallTo _ _) = True
                p _ _ = False
    installData = lens g s -- :: Lens atoms (Map BinPkgName (Set (FilePath, FilePath)))    -- DHInstallData FilePath FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallData src dst) x = Map.insertWith Set.union b (singleton (src, dst)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (src, dst) atoms'' -> insertAtom (Binary b) (DHInstallData src dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallData _ _) = True
                p _ _ = False
    file = lens g s -- :: Lens atoms (Map BinPkgName (Set (FilePath, Text)))    -- DHFile FilePath Text
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHFile path text) x = Map.insertWith Set.union b (singleton (path, text)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (path, text) atoms'' -> insertAtom (Binary b) (DHFile path text) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHFile _ _) = True
                p _ _ = False
    installCabalExec = lens g s -- :: Lens atoms (Map BinPkgName (Set (String, FilePath)))    -- DHInstallCabalExec String FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallCabalExec name dst) x = Map.insertWith Set.union b (singleton (name, dst)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (name, dst) atoms'' -> insertAtom (Binary b) (DHInstallCabalExec name dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallCabalExec _ _) = True
                p _ _ = False
    installCabalExecTo = lens g s -- :: Lens atoms (Map BinPkgName (Set (String, FilePath)))    -- DHInstallCabalExecTo String FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallCabalExecTo name dst) x = Map.insertWith Set.union b (singleton (name, dst)) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b pairs atoms'-> Set.fold (\ (name, dst) atoms'' -> insertAtom (Binary b) (DHInstallCabalExecTo name dst) atoms'') atoms' pairs) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallCabalExecTo _ _) = True
                p _ _ = False
    installDir = lens g s -- :: Lens atoms (Map BinPkgName (Set FilePath))    -- DHInstallDir FilePath
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallDir path) x = Map.insertWith Set.union b (singleton path) x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b paths atoms'-> Set.fold (\ path atoms'' -> insertAtom (Binary b) (DHInstallDir path) atoms'') atoms' paths) (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallDir _) = True
                p _ _ = False
    installInit = lens g s -- :: Lens atoms (Map BinPkgName Text)    -- DHInstallInit Text
        where
          g atoms = foldAtoms from Map.empty atoms
              where
                from (Binary b) (DHInstallInit text) x = Map.insertWith (error "installInit") b text x
                from _ _ x = x
          s x atoms = Map.foldWithKey (\ b text atoms'-> insertAtom (Binary b) (DHInstallInit text) atoms') (deleteAtoms p atoms) x
              where
                p (Binary _) (DHInstallInit _) = True
                p _ _ = False

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity_ = 1
    , debAction_ = Usage
    , dryRun_ = False
    , validate_ = False
    }

insertAtom :: DebAtomKey -> DebAtom -> Atoms -> Atoms
insertAtom mbin atom (Atoms x) = Atoms (insertWith union mbin (singleton atom) x)

insertAtoms :: Set (DebAtomKey, DebAtom) -> Atoms -> Atoms
insertAtoms s atoms =
    case maxView s of
      Nothing -> atoms
      Just ((k, a), s') -> insertAtoms s' (insertAtom k a atoms)

foldAtoms :: (DebAtomKey -> DebAtom -> r -> r) -> r -> Atoms -> r
foldAtoms f r0 (Atoms xs) = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 xs

-- | Split atoms out of a HasAtoms instance by predicate.
partitionAtoms :: (DebAtomKey -> DebAtom -> Bool) -> Atoms -> (Set (DebAtomKey, DebAtom), Atoms)
partitionAtoms f deb =
    foldAtoms g (mempty, Atoms mempty) deb
    where
      g k atom (atoms, deb') =
          case f k atom of
            True -> (Set.insert (k, atom) atoms, deb')
            False -> (atoms, insertAtom k atom deb')

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
