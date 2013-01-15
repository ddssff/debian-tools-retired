{-# LANGUAGE ScopedTypeVariables #-}
module Debian.Debianize.Atoms
    ( defaultAtoms
    , compiler
    , setCompiler
    , packageDescription
    , setPackageDescription
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
    , putBinaryPackageDep
    , doExecutable
    , doServer
    , doWebsite
    , setSourcePackageName
    , debMaintainer
    , buildDir
    , setBuildDir
    , dataDir
    , setDataDir
    , cabalFlagAssignments
    , putCabalFlagAssignments
    , flags
    , mapFlags
    ) where

import Data.Map as Map (Map, insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, null, union, unions)
import Data.Version (Version)
import Debian.Debianize.Types.Atoms (HasAtoms(..), DebAtomKey(..), DebAtom(..), Flags, defaultFlags,
                                     lookupAtom, lookupAtomDef, lookupAtoms, foldAtoms, hasAtom, insertAtom, insertAtoms', partitionAtoms)
import Debian.Debianize.Types.Dependencies (DependencyHints(..), defaultDependencyHints)
import Debian.Debianize.Types.PackageHints (InstallFile, Server, Site)
import Debian.Orphans ()
import Debian.Relation (BinPkgName, SrcPkgName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

defaultAtoms :: Map DebAtomKey (Set DebAtom)
defaultAtoms = mempty

compiler :: HasAtoms atoms => Compiler -> atoms -> Compiler
compiler def deb =
    lookupAtomDef def Source from deb
    where from (DHCompiler x) = Just x
          from _ = Nothing

setCompiler :: forall atoms. HasAtoms atoms => Compiler -> atoms -> atoms
setCompiler comp atoms =
    insertAtom Source (DHCompiler comp) atoms'
    where
      (_, atoms') = partitionAtoms p atoms
      p Source (DHCompiler x) = Just x
      p _ _ = Nothing

packageDescription :: HasAtoms atoms => PackageDescription -> atoms -> PackageDescription
packageDescription def deb =
    lookupAtomDef def Source from deb
    where from (DHPackageDescription x) = Just x
          from _ = Nothing

setPackageDescription :: HasAtoms atoms => PackageDescription -> atoms -> atoms
setPackageDescription desc atoms =
    insertAtom Source (DHPackageDescription desc) atoms'
    where
      (_, atoms') = partitionAtoms p atoms
      p Source (DHPackageDescription x) = Just x
      p _ _ = Nothing

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

doDependencyHint :: HasAtoms atoms => (DependencyHints -> DependencyHints) -> atoms -> atoms
doDependencyHint f deb =
    if Set.null hints
    then insertAtom Source (DHDependencyHints (f defaultDependencyHints)) deb'
    else insertAtoms' Source (map (DHDependencyHints . f) (Set.toList hints)) deb'
    where
      (hints, deb') = partitionAtoms p deb
      p Source (DHDependencyHints x) = Just x
      p _ _ = Nothing

dependencyHints :: HasAtoms atoms => atoms -> DependencyHints
dependencyHints deb =
    fromMaybe defaultDependencyHints $ lookupAtom Source from deb
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

putBinaryPackageDep :: HasAtoms atoms => BinPkgName -> BinPkgName -> atoms -> atoms
putBinaryPackageDep pkg dep deb =
    doDependencyHint (\ x -> x {binaryPackageDeps = (pkg, dep) : binaryPackageDeps x}) deb

doExecutable :: HasAtoms atoms => BinPkgName -> InstallFile -> atoms -> atoms
doExecutable bin x deb = insertAtom (Binary bin) (DHExecutable x) deb

doServer :: HasAtoms atoms => BinPkgName -> Server -> atoms -> atoms
doServer bin x deb = insertAtom (Binary bin) (DHServer x) deb

doWebsite :: HasAtoms atoms => BinPkgName -> Site -> atoms -> atoms
doWebsite bin x deb = insertAtom (Binary bin) (DHWebsite x) deb

setSourcePackageName :: HasAtoms atoms => SrcPkgName -> atoms -> atoms
setSourcePackageName src deb = insertAtom Source (SourcePackageName src) deb

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

setBuildDir :: HasAtoms atoms => FilePath -> atoms -> atoms
setBuildDir path atoms =
    insertAtom Source (BuildDir path) atoms'
    where
      (_, atoms') = partitionAtoms p atoms
      p Source (BuildDir x) = Just x
      p _ _ = Nothing

dataDir :: HasAtoms atoms => FilePath -> atoms -> FilePath
dataDir def atoms =
    fromMaybe def $ foldAtoms from Nothing atoms
    where
      from Source (DataDir path') (Just path) | path /= path' = error $ "Conflicting dataDir atoms: " ++ show path ++ " vs. " ++ show path'
      from Source (DataDir path') _ = Just path'
      from _ _ x = x

setDataDir :: HasAtoms atoms => FilePath -> atoms -> atoms
setDataDir path atoms =
    insertAtom Source (DataDir path) atoms'
    where
      (_, atoms') = partitionAtoms p atoms
      p Source (DataDir x) = Just x
      p _ _ = Nothing

cabalFlagAssignments :: HasAtoms atoms => atoms -> Set (FlagName, Bool)
cabalFlagAssignments atoms =
    foldAtoms from mempty atoms
    where
      from Source (DHCabalFlagAssignments xs) ys = union xs ys
      from _ _ ys = ys

putCabalFlagAssignments :: HasAtoms atoms => Set (FlagName, Bool) -> atoms -> atoms
putCabalFlagAssignments xs atoms =
    insertAtom Source (DHCabalFlagAssignments (unions (xs : toList ys))) atoms'
    where
      (ys, atoms') = partitionAtoms p atoms
      p Source (DHCabalFlagAssignments zs) = Just zs
      p _ _ = Nothing

flags :: HasAtoms atoms => atoms -> Flags
flags atoms =
    fromMaybe defaultFlags $ foldAtoms from Nothing atoms
    where
      from Source (DHFlags _) (Just _) = error "Conflicting Flag atoms"
      from Source (DHFlags fs) _ = Just fs
      from _ _ x = x

mapFlags :: HasAtoms atoms => (Flags -> Flags) -> atoms -> atoms
mapFlags f atoms =
    insertAtom Source (DHFlags (f fs)) atoms'
    where
      fs = case maxView flagss of
             Nothing -> defaultFlags
             Just (x, s) -> if Set.null s then x else error "Conflicting Flag atoms"
      (flagss, atoms') = partitionAtoms p atoms
      p Source (DHFlags x) = Just x
      p _ _ = Nothing
