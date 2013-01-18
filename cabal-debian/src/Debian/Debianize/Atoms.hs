{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Atoms
    ( compiler
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
    , depends
    , conflicts
    , setArchitecture
    , setPriority
    , setSection
    , setDescription
    , doExecutable
    , doServer
    , doWebsite
    , doBackups
    , setSourcePackageName
    , sourceFormat
    , debMaintainer
    , buildDir
    , setBuildDir
    , dataDir
    , setDataDir
    , cabalFlagAssignments
    , putCabalFlagAssignments
    , flags
    , mapFlags
    , watchAtom
    , tightDependencyFixup
    ) where

import Data.List (intersperse)
import Data.Map as Map (insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Set as Set (Set, maxView, toList, null, union, unions)
import Data.Text (Text, pack, unlines)
import Data.Version (Version)
import Debian.Debianize.Types.Atoms (HasAtoms(..), DebAtomKey(..), DebAtom(..), Flags, defaultFlags,
                                     lookupAtom, lookupAtomDef, lookupAtoms, foldAtoms, hasAtom, insertAtom, partitionAtoms)
import Debian.Debianize.Types.Dependencies (DependencyHints(..))
import Debian.Debianize.Types.PackageHints (InstallFile, Server, Site)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat)
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName, Relation(..))
import Distribution.Package (PackageName(..))
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

{-
defaultAtoms :: Map DebAtomKey (Set DebAtom)
defaultAtoms = mempty
-}

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

packageDescription :: HasAtoms atoms => atoms -> Maybe PackageDescription
packageDescription deb =
    lookupAtom Source from deb
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
doDependencyHint = modifyHints
{-
    if Set.null hints
    then insertAtom Source (DHDependencyHints (f defaultDependencyHints)) deb'
    else insertAtoms' Source (map (DHDependencyHints . f) (Set.toList hints)) deb'
    where
      (hints, deb') = partitionAtoms p deb
      p Source (DHDependencyHints x) = Just x
      p (Binary _) (DHDependencyHints _) = error "doDependencyHint"
      p _ _ = Nothing
-}

dependencyHints :: HasAtoms atoms => atoms -> DependencyHints
dependencyHints = getHints
{-
    fromMaybe defaultDependencyHints $ lookupAtom Source from deb
    where
      from (DHDependencyHints x) = Just x
      from _ = Nothing
-}

missingDependency :: HasAtoms atoms => BinPkgName -> atoms -> atoms
missingDependency b deb = doDependencyHint (\ x -> x {missingDependencies = b : missingDependencies x}) deb

setRevision :: HasAtoms atoms => String -> atoms -> atoms
setRevision s deb = doDependencyHint (\ x -> x {revision = s}) deb

putExecMap :: HasAtoms atoms => String -> BinPkgName -> atoms -> atoms
putExecMap cabal debian deb = doDependencyHint (\ x -> x {execMap = Map.insert cabal debian (execMap x)}) deb

putExtraDevDep :: HasAtoms atoms => BinPkgName -> atoms -> atoms
putExtraDevDep bin deb = doDependencyHint (\ x -> x {extraDevDeps = bin : extraDevDeps x}) deb

depends :: HasAtoms atoms => BinPkgName -> Relation -> atoms -> atoms
depends pkg rel deb =
    doDependencyHint (\ x -> x {binaryPackageDeps = (pkg, rel) : binaryPackageDeps x}) deb

conflicts :: HasAtoms atoms => BinPkgName -> Relation -> atoms -> atoms
conflicts pkg rel deb =
    doDependencyHint (\ x -> x {binaryPackageConflicts = (pkg, rel) : binaryPackageConflicts x}) deb

setArchitecture :: HasAtoms atoms => DebAtomKey -> PackageArchitectures -> atoms -> atoms
setArchitecture k x deb = insertAtom k (DHArch x) deb

setPriority :: HasAtoms atoms => DebAtomKey -> PackagePriority -> atoms -> atoms
setPriority k x deb = insertAtom k (DHPriority x) deb

setSection :: HasAtoms atoms => DebAtomKey -> Section -> atoms -> atoms
setSection k x deb = insertAtom k (DHSection x) deb

setDescription :: HasAtoms atoms => DebAtomKey -> Text -> atoms -> atoms
setDescription k x deb = insertAtom k (DHDescription x) deb

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