{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
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
    -- * Future class methods
    , compiler
    , setCompiler
    , packageDescription
    , dataDir
    , setPackageDescription
    , compilerVersion
    , noProfilingLibrary
    , noDocumentationLibrary
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
    , packageInfo
    , rulesHead
    , setRulesHead
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
    , setSourcePackageName
    , setChangeLog
    , setChangeLog'
    , changeLog
    , compat
    , putCopyright
    , copyright
    , sourcePackageName
    , sourceFormat
    , debMaintainer
    , buildDir
    , setBuildDir
    , cabalFlagAssignments
    , putCabalFlagAssignments
    , flags
    , mapFlags
    , watchAtom
    , tightDependencyFixup
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton, fold, insert, member)
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
import Prelude hiding (init, unlines, log)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
--import Data.Set as Set (Set, maxView, toList, null, union, singleton, insert, member)
import Data.Text (pack, unlines)
import Data.Version (showVersion)
import Debian.Changes (ChangeLog(ChangeLog), ChangeLogEntry(logPackage))
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName), Relation(..))
import Distribution.Package (PackageName(..), PackageIdentifier(pkgName, pkgVersion))
import Distribution.PackageDescription as Cabal (PackageDescription(package))
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

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

compiler :: HasAtoms atoms => Compiler -> atoms -> Compiler
compiler def deb =
    lookupAtomDef def Source from deb
    where from (DHCompiler x) = Just x
          from _ = Nothing

setCompiler :: HasAtoms atoms => Compiler -> atoms -> atoms
setCompiler comp atoms =
    replaceAtoms f Source (DHCompiler comp) atoms
    where
      f Source (DHCompiler _) = True
      f _ _ = False

packageDescription :: HasAtoms atoms => atoms -> Maybe PackageDescription
packageDescription deb =
    lookupAtom Source from deb
    where from (DHPackageDescription x) = Just x
          from _ = Nothing

dataDir :: HasAtoms atoms => FilePath -> atoms -> FilePath
dataDir def atoms =
    maybe def dataDirectory $ packageDescription atoms
    where
      -- This is the directory where the files listed in the Data-Files
      -- section of the .cabal file need to be installed.
      dataDirectory :: PackageDescription -> FilePath
      dataDirectory pkgDesc =
          "usr/share" </> (pkgname ++ "-" ++ (showVersion . pkgVersion . package $ pkgDesc))
          where
            PackageName pkgname = pkgName . package $ pkgDesc

setPackageDescription :: HasAtoms atoms => PackageDescription -> atoms -> atoms
setPackageDescription desc atoms =
    replaceAtoms f Source (DHPackageDescription desc) atoms
    where
      f Source (DHPackageDescription _) = True
      f _ _ = False

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
    hasAtom Source isNoDocumentationLibrary deb
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

missingDependency :: HasAtoms atoms => BinPkgName -> atoms -> atoms
missingDependency b deb = insertAtom Source (MissingDependency b) deb

setRevision :: HasAtoms atoms => String -> atoms -> atoms
setRevision s deb =
    replaceAtoms from Source (DebRevision s) deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source (DebRevision _) = True
      from _ _ = False

revision :: HasAtoms atoms => atoms -> String
revision atoms =
    getSingleton "" from atoms
    where
      from Source (DebRevision s) = Just s
      from _ _ = Nothing

setDebVersion :: HasAtoms atoms => DebianVersion -> atoms -> atoms
setDebVersion v deb =
    replaceAtoms from Source (DebVersion v) deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source (DebVersion _) = True
      from _ _ = False

debVersion :: HasAtoms atoms => atoms -> Maybe DebianVersion
debVersion atoms =
    getMaybeSingleton (error "Conflicting DebVersion values") from atoms
    where
      from Source (DebVersion v) = Just v
      from _ _ = Nothing

setOmitLTDeps :: HasAtoms atoms => atoms -> atoms
setOmitLTDeps deb =
    replaceAtoms from Source OmitLTDeps deb
    where
      from :: DebAtomKey -> DebAtom -> Bool
      from Source OmitLTDeps = True
      from _ _ = False

omitLTDeps :: HasAtoms atoms => atoms -> Bool
omitLTDeps atoms =
    not $ Set.null $ fst $ partitionAtoms' from atoms
    where
      from Source OmitLTDeps = Just ()
      from _ _ = Nothing

buildDeps :: HasAtoms atoms => atoms -> Set BinPkgName
buildDeps atoms =
    foldAtoms from mempty atoms
    where
      from Source (BuildDep x) s = Set.insert x s
      from _ _ s = s

buildDepsIndep :: HasAtoms atoms => atoms -> Set BinPkgName
buildDepsIndep atoms =
    foldAtoms from mempty atoms
    where
      from Source (BuildDepIndep x) s = Set.insert x s
      from _ _ s = s

missingDependencies :: HasAtoms atoms => atoms -> Set BinPkgName
missingDependencies atoms =
    foldAtoms from mempty atoms
    where
      from Source (MissingDependency x) s = Set.insert x s
      from _ _ s = s

extraLibMap :: HasAtoms atoms => atoms -> Map.Map String (Set BinPkgName)
extraLibMap atoms =
    foldAtoms from mempty atoms
    where
      from Source (ExtraLibMapping cabal debian) m =
          Map.insertWith union cabal (singleton debian) m
      from _ _ m = m

putExecMap :: HasAtoms atoms => String -> BinPkgName -> atoms -> atoms
putExecMap cabal debian deb = insertAtom Source (ExecMapping cabal debian) deb

packageInfo :: HasAtoms atoms => PackageName -> atoms -> Maybe PackageInfo
packageInfo (PackageName name) atoms =
    foldAtoms from Nothing atoms
    where
      from Source (DebPackageInfo p) Nothing | cabalName p == name = Just p
      from Source (DebPackageInfo p') (Just _) | cabalName p' == name = error $ "Multiple DebPackageInfo entries for " ++ name
      from _ _ x = x

rulesHead :: HasAtoms atoms => atoms -> Text
rulesHead atoms =
    fromMaybe (defaultRulesHead atoms) $ foldAtoms from Nothing atoms
    where
      from Source (DebRulesHead text') (Just text) | text' /= text = error $ "Conflicting values for DebRulesHead: " ++ show text ++ " vs. " ++ show text'
      from Source (DebRulesHead text) _ = Just text
      from _ _ x = x

setRulesHead :: HasAtoms atoms => Text -> atoms -> atoms
setRulesHead text atoms =
    modifyAtoms' f g atoms
    where
      f :: DebAtomKey -> DebAtom -> Maybe Text
      f Source (DebRulesHead x) = Just x
      f _ _ = Nothing
      g :: Set Text -> Set (DebAtomKey, DebAtom)
      g s | Set.null s || s == singleton text = singleton (Source, DebRulesHead text)
      g s = error $ "setRulesHead: " ++ show (s, text)

defaultRulesHead :: HasAtoms atoms => atoms -> Text
defaultRulesHead atoms =
    unlines $ [ "#!/usr/bin/make -f"
              , ""
              , "DEB_CABAL_PACKAGE = " <> pack (show (pretty name))
              , ""
              , "include /usr/share/cdbs/1/rules/debhelper.mk"
              , "include /usr/share/cdbs/1/class/hlibrary.mk"
              , "" ]
    where
      name = fromMaybe logName (sourcePackageName atoms)
      logName = let ChangeLog (hd : _) = changeLog atoms in logPackage hd

execMap :: HasAtoms atoms => atoms -> Map.Map String BinPkgName
execMap atoms =
    foldAtoms from mempty atoms
    where
      from Source (ExecMapping cabal debian) m =
          Map.insertWith (\ a b -> if a /= b
                                   then error $ "Conflicting mapping for Build-Tool " ++ cabal ++ ": " ++ show (a, b)
                                   else a) cabal debian m
      from _ _ m = m

putEpochMapping :: HasAtoms atoms => PackageName -> Int -> atoms -> atoms
putEpochMapping cab n atoms = insertAtom Source (EpochMapping cab n) atoms

epochMap :: HasAtoms atoms => atoms -> Map.Map PackageName Int
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
knownEpochMappings :: HasAtoms atoms => atoms -> atoms
knownEpochMappings = putEpochMapping (PackageName "HaXml") 1

filterMissing :: HasAtoms atoms => atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    filter (/= []) (map (filter (\ (Rel name _ _) -> not (Set.member name (missingDependencies atoms)))) rels)

versionSplits :: HasAtoms atoms => atoms -> [VersionSplits]
versionSplits atoms =
    getSingleton (error "versionSplits") from atoms
    where
      from Source (VersionSplits x) = Just x
      from _ _ = Nothing

putExtraDevDep :: HasAtoms atoms => BinPkgName -> atoms -> atoms
putExtraDevDep bin atoms = insertAtom Source (DevDepends bin) atoms

extraDevDeps :: HasAtoms atoms => atoms -> Set BinPkgName
extraDevDeps atoms =
    foldAtoms f mempty atoms
    where
      f Source (DevDepends p) s = Set.insert p s
      f _ _ s = s

depends :: HasAtoms atoms => BinPkgName -> Relation -> atoms -> atoms
depends pkg rel atoms = insertAtom (Binary pkg) (Depends rel) atoms

conflicts :: HasAtoms atoms => BinPkgName -> Relation -> atoms -> atoms
conflicts pkg rel atoms = insertAtom (Binary pkg) (Conflicts rel) atoms

binaryPackageDeps :: HasAtoms atoms => BinPkgName -> atoms -> [[Relation]]
binaryPackageDeps p atoms =
    foldAtoms f [] atoms
    where f (Binary p') (Depends rel) rels | p == p' = [rel] : rels
          f _ _ rels = rels

binaryPackageConflicts :: HasAtoms atoms => BinPkgName -> atoms -> [[Relation]]
binaryPackageConflicts p atoms =
    foldAtoms f [] atoms
    where f (Binary p') (Conflicts rel) rels | p == p' = [rel] : rels
          f _ _ rels = rels

setSourceArchitecture :: HasAtoms atoms => PackageArchitectures -> atoms -> atoms
setSourceArchitecture x deb = insertAtom Source (DHArch x) deb

setSourcePriority :: HasAtoms atoms => PackagePriority -> atoms -> atoms
setSourcePriority x deb = insertAtom Source (DHPriority x) deb

setSourceSection :: HasAtoms atoms => Section -> atoms -> atoms
setSourceSection x deb = insertAtom Source (DHSection x) deb

setSourceDescription :: HasAtoms atoms => Text -> atoms -> atoms
setSourceDescription x deb = insertAtom Source (DHDescription x) deb

setArchitecture :: HasAtoms atoms => BinPkgName -> PackageArchitectures -> atoms -> atoms
setArchitecture k x deb = insertAtom (Binary k) (DHArch x) deb

setPriority :: HasAtoms atoms => BinPkgName -> PackagePriority -> atoms -> atoms
setPriority k x deb = insertAtom (Binary k) (DHPriority x) deb

setSection :: HasAtoms atoms => BinPkgName -> Section -> atoms -> atoms
setSection k x deb = insertAtom (Binary k) (DHSection x) deb

setDescription :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
setDescription k x deb = insertAtom (Binary k) (DHDescription x) deb

doExecutable :: HasAtoms atoms => BinPkgName -> InstallFile -> atoms -> atoms
doExecutable bin x deb = insertAtom (Binary bin) (DHExecutable x) deb

doServer :: HasAtoms atoms => BinPkgName -> Server -> atoms -> atoms
doServer bin x deb = insertAtom (Binary bin) (DHServer x) deb

doWebsite :: HasAtoms atoms => BinPkgName -> Site -> atoms -> atoms
doWebsite bin x deb = insertAtom (Binary bin) (DHWebsite x) deb

doBackups :: HasAtoms atoms => BinPkgName -> String -> atoms -> atoms
doBackups bin s deb =
    insertAtom (Binary bin) (DHBackups s) $
    depends bin (Rel (BinPkgName "anacron") Nothing Nothing) $
    deb

setSourcePackageName :: HasAtoms atoms => SrcPkgName -> atoms -> atoms
setSourcePackageName src deb = insertAtom Source (SourcePackageName src) deb

setChangeLog :: HasAtoms atoms => ChangeLog -> atoms -> atoms
-- setChangeLog log deb = insertAtom Source (DebChangeLog log) deb
setChangeLog log deb =
    modifyAtoms' f g deb
    where
      f Source (DebChangeLog x) = Just x
      f _ _ = Nothing
      g s | Set.null s = singleton (Source, DebChangeLog log)
      g s = error $ "Multiple changelogs: " ++ show (log, s)

-- | Like setChangeLog, but replacing the current log is not an error.
setChangeLog' :: HasAtoms atoms => ChangeLog -> atoms -> atoms
setChangeLog' log deb =
    replaceAtoms f Source (DebChangeLog log) deb
    where f Source (DebChangeLog _) = True
          f _ _ = False

changeLog :: HasAtoms atoms => atoms -> ChangeLog
changeLog deb =
    maybe (error "No changelog") g $ foldAtoms f Nothing deb
    where
      f Source (DebChangeLog log') (Just log) | log' /= log = error "Conflicting changelogs"
      f Source (DebChangeLog log') _ = Just log'
      f _ _ x = x
      g (ChangeLog (hd : tl)) = ChangeLog (hd {logPackage = fromMaybe (logPackage hd) (sourcePackageName deb)} : tl)
      g _ = error $ "Invalid changelog"

putCopyright :: HasAtoms atoms => Either License Text -> atoms -> atoms
putCopyright copy deb = insertAtom Source (DebCopyright copy) deb

copyright :: HasAtoms atoms => Either License Text -> atoms -> Either License Text
copyright def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (DebCopyright x') (Just x) | x /= x' = error $ "Conflicting copyright atoms: " ++ show x ++ " vs. " ++ show x'
      from Source (DebCopyright x) _ = Just x
      from _ _ x = x

sourcePackageName :: HasAtoms atoms => atoms -> Maybe String
sourcePackageName atoms =
    foldAtoms from Nothing atoms
    where
      from Source (SourcePackageName (SrcPkgName src')) (Just src) | src' /= src = error $ "Conflicting source package names: " ++ show (src, src')
      from Source (SourcePackageName (SrcPkgName src)) _ = Just src
      from _ _ x = x

sourceFormat :: HasAtoms atoms => SourceFormat -> atoms -> atoms
sourceFormat format deb = insertAtom Source (DebSourceFormat format) deb

debMaintainer :: HasAtoms atoms => atoms -> Maybe NameAddr
debMaintainer atoms =
    foldAtoms from Nothing atoms
    where
      from Source (DHMaintainer x) (Just maint) | x /= maint = error $ "Conflicting maintainer values: " ++ show x ++ " vs. " ++ show maint
      from Source (DHMaintainer x) _ = Just x
      from _ _ x = x

buildDir :: HasAtoms atoms => FilePath -> atoms -> FilePath
buildDir def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (BuildDir path') (Just path) | path /= path' = error $ "Conflicting buildDir atoms: " ++ show path ++ " vs. " ++ show path'
      from Source (BuildDir path') _ = Just path'
      from _ _ x = x

compat :: HasAtoms atoms => Int -> atoms -> Int
compat def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (DebCompat n') (Just n) | n /= n' = error $ "Conflicting compat levels: " ++ show (n, n')
      from Source (DebCompat n) _ = Just n
      from _ _ x = x

setBuildDir :: HasAtoms atoms => FilePath -> atoms -> atoms
setBuildDir path atoms =
    replaceAtoms f Source (BuildDir path) atoms
    where
      f Source (BuildDir _) = True
      f _ _ = False

cabalFlagAssignments :: HasAtoms atoms => atoms -> Set (FlagName, Bool)
cabalFlagAssignments atoms =
    foldAtoms from mempty atoms
    where
      from Source (DHCabalFlagAssignments xs) ys = union xs ys
      from _ _ ys = ys

putCabalFlagAssignments :: HasAtoms atoms => Set (FlagName, Bool) -> atoms -> atoms
putCabalFlagAssignments xs atoms =
    modifyAtoms' f g atoms
    where
      f Source (DHCabalFlagAssignments xs') = Just (union xs xs')
      f _ _ = Nothing
      g xss = singleton (Source, DHCabalFlagAssignments (mconcat (Set.toList xss)))

flags :: HasAtoms atoms => atoms -> Flags
flags atoms =
    fromMaybe defaultFlags $ foldAtoms from Nothing atoms
    where
      from Source (DHFlags _) (Just _) = error "Conflicting Flag atoms"
      from Source (DHFlags fs) _ = Just fs
      from _ _ x = x

mapFlags :: HasAtoms atoms => (Flags -> Flags) -> atoms -> atoms
mapFlags f atoms =
    modifyAtoms' g h atoms
    where
      g Source (DHFlags x) = Just x
      g _ _ = Nothing
      h xs = singleton (Source, DHFlags (f (case maxView xs of
                                              Just (_, s) | not (Set.null s) -> error "Conflicting Flag atoms"
                                              Just (x, _) -> x
                                              Nothing -> defaultFlags)))
{-
    insertAtom Source (DHFlags (f fs)) atoms'
    where
      fs = case maxView flagss of
             Nothing -> defaultFlags
             Just (x, s) -> if Set.null s then x else error "Conflicting Flag atoms"
      (flagss, atoms') = partitionAtoms p atoms
      p Source (DHFlags x) = Just x
      p _ _ = Nothing
-}

watchAtom :: HasAtoms atoms => PackageName -> atoms -> atoms
watchAtom (PackageName pkgname) deb =
    insertAtom Source atom deb
    where
      atom =
          DebWatch . pack $
            "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
            "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
            " \\\n    ([\\d\\.]*\\d)/\n"

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: HasAtoms atoms => [(BinPkgName, BinPkgName)] -> BinPkgName -> atoms -> atoms
tightDependencyFixup [] _ deb = deb
tightDependencyFixup pairs p deb =
    insertAtom Source atom deb
    where
      atom = DebRulesFragment
              (unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty
