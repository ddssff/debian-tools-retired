{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.AtomsType
    ( Atoms(Atoms, unAtoms)
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
    , putCompat
    , putCopyright
    , copyright
    , putDebControl
    , debControl
    , sourcePackageName
    , sourceFormat
    , warning
    , debMaintainer
    , buildDir
    , setBuildDir
    , cabalFlagAssignments
    , putCabalFlagAssignments
    , flags
    , mapFlags
    , getWatch
    , watchAtom
    , watchFile
    , tightDependencyFixup
    , sourceDebDescription
    , setSourceDebDescription
    , modifySourceDebDescription
    , newDebianization
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
    , postInst
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
    , getPostInsts
    , getPostRms
    , getPreInsts
    , getPreRms
    , foldExecs
    , foldArchitectures
    , foldPriorities
    , foldSections
    , foldDescriptions
    , foldAtomsFinalized
    , fileAtoms
    , foldCabalDatas
    , foldCabalExecs
    ) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.List as List (map)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton, fold, insert, member, map)
import Data.Text (Text, unpack)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.AtomsClass (HasAtoms)
import Debian.Debianize.AtomsClass (HasAtoms(..), DebAtomKey(..), DebAtom(..), Flags(..), DebAction(..), PackageInfo(..))
import Debian.Debianize.Utility (setMapMaybe)
import Debian.Debianize.Types.PackageHints (Server(..), Site(..), InstallFile(..))
import Debian.Debianize.Types.PackageType (VersionSplits, knownVersionSplits)
import Debian.Orphans ()
import Debian.Policy (SourceFormat, PackageArchitectures, PackagePriority, Section, StandardsVersion, parseMaintainer,
                      apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (BinPkgName, SrcPkgName, Relation)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init, unlines, log)
import System.Process (showCommandForUser)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

import Data.List (intersperse)
import Data.Monoid ((<>))
--import Data.Set as Set (Set, maxView, toList, null, union, singleton, insert, member)
import Data.Text (pack, unlines)
import Data.Version (showVersion)
import Debian.Debianize.Types.DebControl (SourceDebDescription(source, maintainer, standardsVersion), newSourceDebDescription)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName), Relation(..))
import Distribution.Package (PackageName(..), PackageIdentifier(pkgName, pkgVersion))
import Distribution.PackageDescription as Cabal (PackageDescription(package))
import System.FilePath ((</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

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
defaultAtoms = insertAtom Source (VersionSplits knownVersionSplits) $ (Atoms mempty)

instance Monoid Atoms where
    mempty = defaultAtoms
    mappend a b = foldAtoms insertAtom a b

instance HasAtoms Atoms where
    getAtoms = unAtoms
    putAtoms mp _ = Atoms mp

lookupAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Maybe a
lookupAtom mbin from xs =
    case maxView (lookupAtoms mbin from xs) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtomDef :: (HasAtoms atoms, Show a, Ord a) => a -> DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> a
lookupAtomDef def key from xs = fromMaybe def $ lookupAtom key from xs

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe Set.empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

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

putCompilerVersion :: HasAtoms atoms => Version -> atoms -> atoms
putCompilerVersion ver deb = insertAtom Source (CompilerVersion ver) deb

putNoProfilingLibrary :: HasAtoms atoms => atoms -> atoms
putNoProfilingLibrary deb = insertAtom Source NoProfilingLibrary deb

noProfilingLibrary :: HasAtoms atoms => atoms -> Bool
noProfilingLibrary deb =
    not . Set.null . lookupAtoms Source isNoProfilingLibrary $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = Just NoProfilingLibrary
      isNoProfilingLibrary _ = Nothing

putNoDocumentationLibrary :: HasAtoms atoms => atoms -> atoms
putNoDocumentationLibrary deb = insertAtom Source NoDocumentationLibrary deb

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

putBuildDep :: HasAtoms atoms => BinPkgName -> atoms -> atoms
putBuildDep bin atoms = insertAtom Source (BuildDep bin) atoms

buildDepsIndep :: HasAtoms atoms => atoms -> Set BinPkgName
buildDepsIndep atoms =
    foldAtoms from mempty atoms
    where
      from Source (BuildDepIndep x) s = Set.insert x s
      from _ _ s = s

putBuildDepIndep :: HasAtoms atoms => BinPkgName -> atoms -> atoms
putBuildDepIndep bin atoms = insertAtom Source (BuildDep bin) atoms

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

putExtraLibMapping :: HasAtoms atoms => String -> BinPkgName -> atoms -> atoms
putExtraLibMapping cab deb atoms = insertAtom Source (ExtraLibMapping cab deb) atoms

putExecMap :: HasAtoms atoms => String -> BinPkgName -> atoms -> atoms
putExecMap cabal debian deb = insertAtom Source (ExecMapping cabal debian) deb

putPackageInfo :: HasAtoms atoms => PackageInfo -> atoms -> atoms
putPackageInfo info atoms = insertAtom Source (DebPackageInfo info) atoms

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
    filter (/= []) (List.map (filter (\ (Rel name _ _) -> not (Set.member name (missingDependencies atoms)))) rels)

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

foldArchitectures :: HasAtoms atoms =>
                    (PackageArchitectures -> r -> r)
                 -> (BinPkgName -> PackageArchitectures -> r -> r)
                 -> r
                 -> atoms
                 -> r
foldArchitectures sourceArch binaryArch r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHArch x) r = binaryArch p x r
      from Source (DHArch x) r = sourceArch x r
      from _ _ r = r

foldPriorities :: HasAtoms atoms =>
                  (PackagePriority -> r -> r)
               -> (BinPkgName -> PackagePriority -> r -> r)
               -> r
               -> atoms
               -> r
foldPriorities sourcePriority binaryPriority r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHPriority x) r = binaryPriority p x r
      from Source (DHPriority x) r = sourcePriority x r
      from _ _ r = r

foldSections :: HasAtoms atoms =>
                (Section -> r -> r)
             -> (BinPkgName -> Section -> r -> r)
               -> r
               -> atoms
               -> r
foldSections sourceSection binarySection r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHSection x) r = binarySection p x r
      from Source (DHSection x) r = sourceSection x r
      from _ _ r = r

foldDescriptions :: HasAtoms atoms =>
                    (BinPkgName -> Text -> r -> r)
                 -> r
                 -> atoms
                 -> r
foldDescriptions description r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHDescription x) r = description p x r
      from _ _ r = r

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

debControl :: HasAtoms atoms => atoms -> Maybe SourceDebDescription
debControl atoms =
    foldAtoms from Nothing atoms
    where
      from Source (DebControl x') (Just x) | x /= x' = error $ "Conflicting Source Deb Descriptions: " ++ show (x, x')
      from Source (DebControl x) _ = Just x
      from _ _ x = x

putDebControl :: HasAtoms atoms => SourceDebDescription -> atoms -> atoms
putDebControl deb atoms = insertAtom Source (DebControl deb) atoms

sourcePackageName :: HasAtoms atoms => atoms -> Maybe String
sourcePackageName atoms =
    foldAtoms from Nothing atoms
    where
      from Source (SourcePackageName (SrcPkgName src')) (Just src) | src' /= src = error $ "Conflicting source package names: " ++ show (src, src')
      from Source (SourcePackageName (SrcPkgName src)) _ = Just src
      from _ _ x = x

sourceFormat :: HasAtoms atoms => SourceFormat -> atoms -> atoms
sourceFormat format deb = insertAtom Source (DebSourceFormat format) deb

warning :: HasAtoms atoms => Text -> atoms -> atoms
warning text deb = insertAtom Source (Warning text) deb

putDebMaintainer :: HasAtoms atoms => NameAddr -> atoms -> atoms
putDebMaintainer maint atoms = insertAtom Source (DHMaintainer maint) atoms

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

putCompat :: HasAtoms atoms => Int -> atoms -> atoms
putCompat n atoms = insertAtom Source (DebCompat n) atoms

intermediateFile :: HasAtoms atoms => FilePath -> Text -> atoms -> atoms
intermediateFile path text atoms = insertAtom Source (DHIntermediate path text) atoms

getIntermediateFiles :: HasAtoms atoms => atoms -> [(FilePath, Text)]
getIntermediateFiles atoms =
    foldAtoms from [] atoms
    where
      from Source (DHIntermediate path text) xs = (path, text) : xs
      from _ _ xs = xs

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

getWatch :: HasAtoms atoms => atoms -> Maybe Text
getWatch atoms =
    foldAtoms from Nothing atoms
    where
      from :: DebAtomKey -> DebAtom -> Maybe Text -> Maybe Text
      from Source (DebWatch x') (Just x) | x /= x' = error $ "Conflicting debian/watch files: " ++ show (x, x')
      from Source (DebWatch x) _ = Just x
      from _ _ x = x

watchAtom :: HasAtoms atoms => PackageName -> atoms -> atoms
watchAtom (PackageName pkgname) deb =
    watchFile (pack $ "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
                      "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
                      " \\\n    ([\\d\\.]*\\d)/\n") deb

watchFile :: HasAtoms atoms => Text -> atoms -> atoms
watchFile text deb = insertAtom Source (DebWatch text) deb

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
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

sourceDebDescription :: HasAtoms atoms => atoms -> SourceDebDescription
sourceDebDescription = fromMaybe newSourceDebDescription . debControl

setSourceDebDescription :: HasAtoms atoms => SourceDebDescription -> atoms -> atoms
setSourceDebDescription d x = modifySourceDebDescription (const d) x

modifySourceDebDescription :: HasAtoms atoms => (SourceDebDescription -> SourceDebDescription) -> atoms -> atoms
modifySourceDebDescription f deb =
    modifyAtoms' g (Set.map (\ d -> (Source, DebControl (f d)))) deb
    where
      g Source (DebControl d) = Just d
      g _ _ = Nothing

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLog -> Int -> StandardsVersion -> Atoms
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    setChangeLog log $
    insertAtom Source (DebCompat level) $
    modifySourceDebDescription (\ x -> x { source = Just (SrcPkgName (logPackage entry))
                                         , maintainer = (either error Just (parseMaintainer (logWho entry)))
                                         , standardsVersion = Just standards }) $
    defaultAtoms
newDebianization _ _ _ = error "Invalid changelog"

install :: HasAtoms atoms => BinPkgName -> FilePath -> FilePath -> atoms -> atoms
install p path d atoms = insertAtom (Binary p) (DHInstall path d) atoms

installData :: HasAtoms atoms => BinPkgName -> FilePath -> FilePath -> atoms -> atoms
installData p path dest atoms = insertAtom (Binary p) (DHInstallData path dest) atoms

getInstalls :: HasAtoms atoms => atoms -> Map BinPkgName (Set (FilePath, FilePath))
getInstalls atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstall src dst) mp = Map.insertWith Set.union p (singleton (src, dst)) mp
      from _ _ mp = mp

installInit :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
installInit p text atoms = insertAtom (Binary p) (DHInstallInit text) atoms

logrotateStanza :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
logrotateStanza p text atoms = insertAtom (Binary p) (DHLogrotateStanza text) atoms

postInst :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
postInst p text atoms = insertAtom (Binary p) (DHPostInst text) atoms

postRm :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
postRm p text atoms = insertAtom (Binary p) (DHPostRm text) atoms

preInst :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
preInst p text atoms = insertAtom (Binary p) (DHPreInst text) atoms

preRm :: HasAtoms atoms => BinPkgName -> Text -> atoms -> atoms
preRm p text atoms = insertAtom (Binary p) (DHPreRm text) atoms

link :: HasAtoms atoms => BinPkgName -> FilePath -> FilePath -> atoms -> atoms
link p path text atoms = insertAtom (Binary p) (DHLink path text) atoms

file :: HasAtoms atoms => BinPkgName -> FilePath -> Text -> atoms -> atoms
file p path text atoms = insertAtom (Binary p) (DHFile path text) atoms 

installDir :: HasAtoms atoms => BinPkgName -> FilePath -> atoms -> atoms
installDir p path atoms = insertAtom (Binary p) (DHInstallDir path) atoms

rulesFragment :: HasAtoms atoms => Text -> atoms -> atoms
rulesFragment text atoms = insertAtom Source (DebRulesFragment text) atoms

installCabalExec :: HasAtoms atoms => BinPkgName -> String -> FilePath -> atoms -> atoms
installCabalExec p name d atoms = insertAtom (Binary p) (DHInstallCabalExec name d) atoms

installCabalExecTo :: HasAtoms atoms => BinPkgName -> String -> FilePath -> atoms -> atoms
installCabalExecTo p name dest atoms = insertAtom (Binary p) (DHInstallCabalExecTo name dest) atoms

installTo :: HasAtoms atoms => BinPkgName -> FilePath -> FilePath -> atoms -> atoms
installTo p from dest atoms = insertAtom (Binary p) (DHInstallTo from dest) atoms

one :: (Eq a, Show a) => a -> a -> a
one old new | old /= new = error $ "Conflict: " ++ show (old, new)
one old _ = old

getInstallDirs :: HasAtoms atoms => atoms -> Map BinPkgName (Set FilePath)
getInstallDirs atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstallDir d) mp = Map.insertWith Set.union p (singleton d) mp
      from _ _ mp = mp

getInstallInits :: HasAtoms atoms => atoms -> Map BinPkgName Text
getInstallInits atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHInstallInit t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getLogrotateStanzas :: HasAtoms atoms => atoms -> Map BinPkgName (Set Text)
getLogrotateStanzas atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHLogrotateStanza t) mp = Map.insertWith Set.union p (singleton t) mp
      from _ _ mp = mp

getLinks :: HasAtoms atoms => atoms -> Map BinPkgName (Set (FilePath, FilePath))
getLinks atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHLink loc txt) mp = Map.insertWith Set.union p (singleton (loc, txt)) mp
      from _ _ mp = mp

getPostInsts :: HasAtoms atoms => atoms -> Map BinPkgName Text
getPostInsts atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPostInst t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getPostRms :: HasAtoms atoms => atoms -> Map BinPkgName Text
getPostRms atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPostRm t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getPreInsts :: HasAtoms atoms => atoms -> Map BinPkgName Text
getPreInsts atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPreInst t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

getPreRms :: HasAtoms atoms => atoms -> Map BinPkgName Text
getPreRms atoms =
    foldAtoms from Map.empty atoms
    where
      from (Binary p) (DHPreRm t) mp = Map.insertWith one p t mp
      from _ _ mp = mp

foldExecs :: HasAtoms atoms =>
             (BinPkgName -> Site -> r -> r)
          -> (BinPkgName -> Server -> r -> r)
          -> (BinPkgName -> String -> r -> r)
          -> (BinPkgName -> InstallFile -> r -> r)
          -> r
          -> atoms
          -> r
foldExecs site serv backup exec r0 atoms =
    foldAtoms from r0 atoms
    where
      from (Binary p) (DHWebsite x) r = site p x r
      from (Binary p) (DHServer x) r = serv p x r
      from (Binary p) (DHBackups x) r = backup p x r
      from (Binary p) (DHExecutable x) r = exec p x r
      from _ _ r = r

foldAtomsFinalized :: HasAtoms atoms => (DebAtomKey -> DebAtom -> r -> r) -> r -> atoms -> r
foldAtomsFinalized f r0 atoms =
    foldr (\ (k, a) r -> f k a r) r0 (expandAtoms pairs)
    where
      pairs = foldAtoms (\ k a xs -> (k, a) : xs) [] atoms
      builddir = buildDir "dist-ghc/build" atoms
      datadir = dataDir (error "foldAtomsFinalized") atoms

      -- | Fully expand an atom set, returning a set containing
      -- both the original and the expansion.
      expandAtoms :: [(DebAtomKey, DebAtom)] -> [(DebAtomKey, DebAtom)]
      expandAtoms [] = []
      expandAtoms xs = xs ++ expandAtoms (concatMap (uncurry expandAtom) xs)

      expandAtom :: DebAtomKey -> DebAtom -> [(DebAtomKey, DebAtom)]
      expandAtom (Binary b) (DHApacheSite domain' logdir text) =
          [(Binary b, DHLink ("/etc/apache2/sites-available/" ++ domain') ("/etc/apache2/sites-enabled/" ++ domain')),
           (Binary b, DHInstallDir logdir), -- Server won't start if log directory doesn't exist
           (Binary b, DHFile ("/etc/apache2/sites-available" </> domain') text)]
      expandAtom (Binary pkg) (DHInstallCabalExec name dst) =
          [(Binary pkg, DHInstall (builddir </> name </> name) dst)]
      expandAtom (Binary p) (DHInstallCabalExecTo n d) =
          [(Source, DebRulesFragment (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                              , "\tinstall -Dp " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]))]
      expandAtom (Binary p) (DHInstallData s d) =
          [(Binary p, if takeFileName s == takeFileName d
                      then DHInstall s (datadir </> makeRelative "/" (takeDirectory d))
                      else DHInstallTo s (datadir </> makeRelative "/" d))]
      expandAtom (Binary p) (DHInstallTo s d) =
          [(Source, (DebRulesFragment (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                               , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])))]
      expandAtom (Binary p) (DHFile path s) =
          let (destDir', destName') = splitFileName path
              tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
              tmpPath = tmpDir </> destName' in
          [(Source, DHIntermediate tmpPath s),
           (Binary p, DHInstall tmpPath destDir')]
      expandAtom k (DHWebsite x) =
          siteAtoms k x
      expandAtom k (DHServer x) =
          serverAtoms k x False
      expandAtom k (DHBackups s) =
          backupAtoms k s
      expandAtom k (DHExecutable x) =
          execAtoms k x
      expandAtom _ _ = []

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
siteAtoms :: DebAtomKey -> Site -> [(DebAtomKey, DebAtom)]
siteAtoms k@(Binary b) site =
    [(Binary b, DHInstallDir "/etc/apache2/sites-available"),
     (Binary b, DHLink ("/etc/apache2/sites-available/" ++ domain site) ("/etc/apache2/sites-enabled/" ++ domain site)),
     (Binary b, DHFile ("/etc/apache2/sites-available" </> domain site) apacheConfig),
     (Binary b, DHInstallDir (apacheLogDirectory b)),  -- Server won't start if log directory doesn't exist
     (Binary b, DHLogrotateStanza (unlines $
                                              [ pack (apacheAccessLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}"])),
     (Binary b, DHLogrotateStanza (unlines $
                                              [ pack (apacheErrorLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}" ]))] ++
    serverAtoms k (server site) True
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

serverAtoms :: DebAtomKey -> Server -> Bool -> [(DebAtomKey, DebAtom)]
serverAtoms k@(Binary b) server isSite =
    [(Binary b, DHPostInst debianPostinst),
     (Binary b, DHInstallInit debianInit)] ++
    serverLogrotate b ++
    execAtoms k exec
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
serverLogrotate :: BinPkgName -> [(DebAtomKey, DebAtom)]
serverLogrotate b =
    [(Binary b, (DHLogrotateStanza . unlines $
                   [ pack (serverAccessLog b) <> " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ])),
     (Binary b, (DHLogrotateStanza . unlines $
                   [ pack (serverAppLog b) <> " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ]))]

backupAtoms :: DebAtomKey -> String -> [(DebAtomKey, DebAtom)]
backupAtoms k name =
    [(k, DHPostInst . unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])] ++
    execAtoms k (InstallFile { execName = name
                             , destName = name
                             , sourceDir = Nothing
                             , destDir = Just "/etc/cron.hourly" })

execAtoms :: DebAtomKey -> InstallFile -> [(DebAtomKey, DebAtom)]
execAtoms (Binary b) ifile =
    [(Source, DebRulesFragment (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp")))] ++
     fileAtoms (Binary b) ifile

fileAtoms :: DebAtomKey -> InstallFile -> [(DebAtomKey, DebAtom)]
fileAtoms k installFile =
    fileAtoms' k (sourceDir installFile) (execName installFile) (destDir installFile) (destName installFile)

fileAtoms' :: DebAtomKey -> Maybe FilePath -> String -> Maybe FilePath -> String -> [(DebAtomKey, DebAtom)]
fileAtoms' (Binary b) sourceDir execName destDir destName =
    [(Binary b, case (sourceDir, execName == destName) of
                  (Nothing, True) -> DHInstallCabalExec execName d
                  (Just s, True) -> DHInstall (s </> execName) d
                  (Nothing, False) -> DHInstallCabalExecTo execName (d </> destName)
                  (Just s, False) -> DHInstallTo (s </> execName) (d </> destName))]
    where
      d = fromMaybe "usr/bin" destDir

foldCabalExecs :: HasAtoms atoms => (String -> r -> r) -> r -> atoms -> r
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

foldCabalDatas :: HasAtoms atoms => (FilePath -> r -> r) -> r -> atoms -> r
foldCabalDatas f r0 atoms =
    foldAtoms g r0 atoms
    where
      g (Binary _) (DHInstall path _) r = f path r
      g (Binary _) (DHInstallTo path _) r = f path r
      g (Binary _) (DHInstallData path _) r = f path r
      g _ _ r = r
