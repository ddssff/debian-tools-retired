{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.DebT
    ( DebT
    , runDebT
    , Deb
    , runDeb

    -- * Lens Helper Functions
    , doConst
    , doConstJust
    , doConstMaybe
    , doOnce
    , doModify
    , doSetElem
    , doMapElem
    , doMapSet

     -- * Modes of operation
    , verbosity
    , dryRun
    , debAction
    , cabalFlagAssignments

    -- * Repository info
    , execMap
    , epochMap
    , missingDependency
    , Debian.DebT.mapCabal
    , Debian.DebT.splitCabal
    , extraLibMap

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
    , dataDir
    , rulesHead
    , rulesFragments
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageName
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
    , serverInfo
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
    , conflicts
    , provides
    , depends
    , replaces
    -- * Binary Package Files
    , link
    , install
    , installTo
    , installData
    , file
    , installDir
    , logrotateStanza
    , installCabalExec
    , installCabalExecTo

    -- * Unknown, Obsolete, or Internal
    , flags -- obsolete
    , validate -- obsolete
    , packageDescription -- Internal
    , warning -- no-op?
    , compiler
    , intermediateFiles
    , packageInfo
    , control

    ) where

import Control.Monad.State (execState, execStateT, modify, State, StateT)
import Data.Lens.Lazy (Lens, modL, setL)
import Data.Map as Map (Map, insert, insertWith, alter)
import Data.Maybe (fromMaybe)
import Data.Set as Set (empty, insert, Set, singleton, union)
import Data.Text as Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog)
import Debian.Debianize.ControlFile (SourceDebDescription)
import Debian.Debianize.Lenses (Atoms, Flags)
import qualified Debian.Debianize.Lenses as Lenses
import Debian.Debianize.Types (DebAction(..), InstallFile(..), PackageInfo(..), Server(..), Site(..))
import Debian.Debianize.VersionSplits (VersionSplits, makePackage, insertSplit)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat, StandardsVersion)
import Debian.Relation (AndRelation, BinPkgName, Relation, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init, log, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

type DebT m = StateT Lenses.Atoms m
type Deb = State Lenses.Atoms

runDebT :: Monad m => DebT m () -> Atoms -> m Atoms
runDebT action atoms = execStateT action atoms

runDeb :: Deb () -> Atoms -> Atoms
runDeb action atoms = execState action atoms

-- | Set the value to x regardless of its previous value
doConst :: Monad m => Lens Atoms a -> a -> DebT m ()
doConst lens x = modify (modL lens (const x))

verbosity :: Monad m => Int -> DebT m ()
verbosity = doConst Lenses.verbosity
dryRun :: Monad m => Bool -> DebT m ()
dryRun x = modify (modL (Lenses.dryRun :: Lens Atoms Bool) (const x))
validate :: Monad m => Bool -> DebT m ()
validate x = modify (modL (Lenses.validate :: Lens Atoms Bool) (const x))
debAction :: Monad m => DebAction -> DebT m ()
debAction = doConst Lenses.debAction
flags :: Monad m => Flags -> DebT m ()
flags = doConst Lenses.flags
omitLTDeps :: Monad m => Bool -> DebT m ()
omitLTDeps = doConst Lenses.omitLTDeps
noProfilingLibrary :: Monad m => Bool -> DebT m ()
noProfilingLibrary = doConst Lenses.noProfilingLibrary
noDocumentationLibrary :: Monad m => Bool -> DebT m ()
noDocumentationLibrary = doConst Lenses.noDocumentationLibrary

doConstJust :: Monad m => Lens Atoms (Maybe a) -> a -> DebT m ()
doConstJust lens x = modify (setL lens (Just x))
doConstMaybe :: Monad m => Lens Atoms (Maybe a) -> Maybe a -> DebT m ()
doConstMaybe = doConst

compilerVersion :: Monad m => Version -> DebT m () -- Lens Atoms (Maybe Version)
compilerVersion v = doConstJust Lenses.compilerVersion v
packageDescription :: Monad m => PackageDescription -> DebT m ()
packageDescription d = doConstJust Lenses.packageDescription d
buildDir :: Monad m => FilePath -> DebT m ()
buildDir = doConstJust Lenses.buildDir
dataDir :: Monad m => FilePath -> DebT m ()
dataDir = doConstJust Lenses.dataDir
compiler :: Monad m => Compiler -> DebT m ()
compiler = doConstJust Lenses.compiler
revision :: Monad m => String -> DebT m ()
revision = doConstJust Lenses.revision
debVersion :: Monad m => DebianVersion -> DebT m ()
debVersion = doConstJust Lenses.debVersion
maintainer :: Monad m => NameAddr -> DebT m ()
maintainer = doConstJust Lenses.maintainer

copyright :: Monad m => Either License Text -> DebT m ()
copyright = doConstJust Lenses.copyright

-- sourcePackageName :: Monad m => Lens Atoms (Maybe SrcPkgName)
sourcePackageName :: Monad m => SrcPkgName -> DebT m ()
sourcePackageName = doConstJust Lenses.sourcePackageName
sourceArchitecture :: Monad m => PackageArchitectures -> DebT m ()
sourceArchitecture = doConstJust Lenses.sourceArchitecture
sourcePriority :: Monad m => PackagePriority -> DebT m ()
sourcePriority = doConstJust Lenses.sourcePriority
sourceSection :: Monad m => Section -> DebT m ()
sourceSection = doConstJust Lenses.sourceSection
rulesHead :: Monad m => Text -> DebT m ()
rulesHead = doConstJust Lenses.rulesHead
-- compat :: Monad m => Lens Atoms (Maybe Int)
compat :: Monad m => Int -> DebT m ()
compat = doConstJust Lenses.compat
sourceFormat :: Monad m => SourceFormat -> DebT m ()
sourceFormat = doConstJust Lenses.sourceFormat
watch :: Monad m => Text -> DebT m ()
watch = doConstJust Lenses.watch
changelog :: Monad m => ChangeLog -> DebT m ()
changelog = doConstJust Lenses.changelog
standards :: Monad m => StandardsVersion -> DebT m ()
standards = doConstJust Lenses.standards

-- | Set a value if it is unset, but refuse to update it if it is set
-- again to a different value.
doOnce :: (Monad m, Eq a) => Lens Atoms (Maybe a) -> a -> DebT m ()
doOnce lens x = modify (modL lens (maybe (Just x) (\ x' -> if x /= x' then error "Conflict" else Just x)))

comments :: Monad m => [[Text]] -> DebT m ()
comments = doOnce Lenses.comments

doModify :: Monad m => Lens Atoms a -> (a -> a) -> DebT m ()
doModify lens f = modify (modL lens f)

-- control :: Monad m => Lens Atoms SourceDebDescription
control :: Monad m => (SourceDebDescription -> SourceDebDescription) -> DebT m ()
control = doModify Lenses.control

doSetElem :: (Monad m, Ord a) => Lens Atoms (Set a) -> a -> DebT m ()
doSetElem lens x = modify (modL lens (Set.insert x))

-- | Unused.
warning :: Monad m => Text -> DebT m ()
warning = doSetElem Lenses.warning
-- missingDependencies :: Monad m => Lens Atoms (Set BinPkgName)
missingDependency :: Monad m => BinPkgName -> DebT m ()
missingDependency = doSetElem Lenses.missingDependencies
cabalFlagAssignments :: Monad m => (FlagName, Bool) -> DebT m ()
cabalFlagAssignments = doSetElem Lenses.cabalFlagAssignments
buildDeps :: Monad m => Relations -> DebT m ()
buildDeps = doSetElem Lenses.buildDeps
-- buildDepsIndep :: Monad m => Lens Atoms (Set Relations)
buildDepsIndep :: Monad m => AndRelation -> DebT m ()
buildDepsIndep = doSetElem Lenses.buildDepsIndep
extraDevDeps :: Monad m => Relation -> DebT m ()
extraDevDeps = doSetElem Lenses.extraDevDeps
-- rulesFragments :: Monad m => Lens Atoms (Set Text)
rulesFragments :: Monad m => Text -> DebT m ()
rulesFragments text = doSetElem Lenses.rulesFragments text
intermediateFiles :: Monad m => (FilePath, Text) -> DebT m ()
intermediateFiles = doSetElem Lenses.intermediateFiles

-- We should change the lens type from Maybe (Set a) to Set a
utilsPackageName :: Monad m => BinPkgName -> DebT m ()
utilsPackageName x = modify (modL Lenses.utilsPackageNames (\ s -> Just (Set.insert x (fromMaybe Set.empty s))))

doMapElem :: (Monad m, Ord a) => Lens Atoms (Map a b) -> a -> b -> DebT m ()
doMapElem lens a b = modify (modL lens (Map.insert a b))

-- binaryArchitectures :: Monad m => Lens Atoms (Map BinPkgName PackageArchitectures)
binaryArchitectures :: Monad m => BinPkgName -> PackageArchitectures -> DebT m ()
binaryArchitectures = doMapElem Lenses.binaryArchitectures
execMap :: Monad m => String -> Relations -> DebT m ()
execMap = doMapElem Lenses.execMap
epochMap :: Monad m => PackageName -> Int -> DebT m ()
epochMap = doMapElem Lenses.epochMap
-- description :: Monad m => BinPkgName -> Text -> DebT m ()
description :: Monad m => BinPkgName -> Text -> DebT m ()
description = doMapElem Lenses.description
executable :: Monad m => BinPkgName -> InstallFile -> DebT m ()
executable = doMapElem Lenses.executable
serverInfo :: Monad m => BinPkgName -> Server -> DebT m ()
serverInfo = doMapElem Lenses.serverInfo
website :: Monad m => BinPkgName -> Site -> DebT m ()
website = doMapElem Lenses.website
backups :: Monad m => BinPkgName -> String -> DebT m ()
backups = doMapElem Lenses.backups
mapCabal :: Monad m => PackageName -> String -> DebT m ()

-- | Map all versions of Cabal package pname to Debian package dname.
-- Not really a debian package name, but the name of a cabal package
-- that maps to the debian package name we want.  (Should this be a
-- SrcPkgName?)
mapCabal pname dname =
    modify (modL Lenses.debianNameMap (Map.alter f pname))
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = Just (makePackage dname)
      f (Just sp) = error $ "mapCabal - already mapped: " ++ show sp
-- | Map versions less than ver of Cabal Package pname to Debian package ltname
splitCabal :: Monad m => PackageName -> String -> Version -> DebT m ()
splitCabal pname ltname ver =
    modify (modL Lenses.debianNameMap (Map.alter f pname))
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = error $ "splitCabal - not mapped: " ++ show pname
      f (Just sp) = Just (insertSplit ver ltname sp)

apacheSite :: Monad m => BinPkgName -> (String, FilePath, Text) -> DebT m ()
apacheSite = doMapElem Lenses.apacheSite
packageInfo :: Monad m => PackageName -> PackageInfo -> DebT m ()
packageInfo = doMapElem Lenses.packageInfo
binaryPriorities :: Monad m => BinPkgName -> PackagePriority -> DebT m ()
binaryPriorities = doMapElem Lenses.binaryPriorities
binarySections :: Monad m => BinPkgName -> Section -> DebT m ()
binarySections = doMapElem Lenses.binarySections
postInst :: Monad m => BinPkgName -> Text -> DebT m ()
postInst = doMapElem Lenses.postInst
postRm :: Monad m => BinPkgName -> Text -> DebT m ()
postRm = doMapElem Lenses.postRm
preInst :: Monad m => BinPkgName -> Text -> DebT m ()
preInst = doMapElem Lenses.preInst
preRm :: Monad m => BinPkgName -> Text -> DebT m ()
preRm = doMapElem Lenses.preRm
installInit :: Monad m => BinPkgName -> Text -> DebT m ()
installInit = doMapElem Lenses.installInit

doMapSet :: (Monad m, Ord a, Ord b) => Lens Atoms (Map a (Set b)) -> a -> b -> DebT m ()
doMapSet lens a b = modify (modL lens (Map.insertWith union a (singleton b)))

-- depends :: Monad m => Lens Atoms (Map BinPkgName (Set Relation))
-- conflicts :: Monad m => Lens Atoms (Map BinPkgName (Set Relation))
-- replaces :: Monad m => Lens Atoms (Map BinPkgName (Set Relation))
-- provides :: Monad m => Lens Atoms (Map BinPkgName (Set Relation))
conflicts :: Monad m => BinPkgName -> Relation -> DebT m ()
conflicts = doMapSet Lenses.conflicts
provides :: Monad m => BinPkgName -> Relation -> DebT m ()
provides = doMapSet Lenses.provides
depends :: Monad m => BinPkgName -> Relation -> DebT m ()
depends = doMapSet Lenses.depends
replaces :: Monad m => BinPkgName -> Relation -> DebT m ()
replaces = doMapSet Lenses.replaces

link :: Monad m => BinPkgName -> (FilePath, FilePath) -> DebT m ()
link = doMapSet Lenses.link
install :: Monad m => BinPkgName -> (FilePath, FilePath) -> DebT m ()
install = doMapSet Lenses.install
installTo :: Monad m => BinPkgName -> (FilePath, FilePath) -> DebT m ()
installTo = doMapSet Lenses.installTo
installData :: Monad m => BinPkgName -> (FilePath, FilePath) -> DebT m ()
installData = doMapSet Lenses.installData
file :: Monad m => BinPkgName -> (FilePath, Text) -> DebT m ()
file = doMapSet Lenses.file
logrotateStanza :: Monad m => BinPkgName -> Text -> DebT m ()
logrotateStanza = doMapSet Lenses.logrotateStanza
-- installCabalExec :: Monad m => Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExec :: Monad m => BinPkgName -> (String, FilePath) -> DebT m ()
installCabalExec = doMapSet Lenses.installCabalExec
installCabalExecTo :: Monad m => BinPkgName -> (String, FilePath) -> DebT m ()
installCabalExecTo = doMapSet Lenses.installCabalExecTo
installDir :: Monad m => BinPkgName -> FilePath -> DebT m ()
installDir = doMapSet Lenses.installDir

extraLibMap :: Monad m => String -> Relations -> DebT m ()
extraLibMap = doMapSet Lenses.extraLibMap
