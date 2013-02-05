-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Data.Char (toLower)
import Data.Lens.Lazy (setL, getL)
import Data.List as List (map)
import Data.Maybe
import Data.Monoid (Monoid, mempty)
import Data.Set as Set (Set, difference, fromList, null, insert, toList, filter, fold, empty, map)
import Data.Text (pack)
import Debian.Debianize.Atoms (HasAtoms(packageDescription), DebAtomKey(..), DebAtom(..),
                                   Atoms, insertAtom, modControl,
                                   setArchitecture, binaryPackageDeps,
                                   binaryPackageConflicts, noProfilingLibrary, noDocumentationLibrary, utilsPackageName, extraDevDeps,
                                   finalizeAtoms, foldAtoms, rulesFragment, installData, installCabalExec, foldCabalDatas, foldCabalExecs)
import Debian.Debianize.Combinators (describe, buildDeps)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                               newBinaryDebDescription, modifyBinaryDeb,
                                               PackageType(Exec, Development, Profiling, Documentation, Utilities))
import Debian.Debianize.Dependencies (debianName)
import Debian.Policy (PackageArchitectures(Any, All), Section(..))
import Debian.Relation (Relation(Rel), BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (init, unlines, writeFile)
import System.FilePath ((</>), (<.>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
finalizeDebianization  :: Atoms -> Atoms
finalizeDebianization deb0 =
    deb'''''
    where
      -- Fixme - makeUtilsPackage does stuff that needs to go through foldAtomsFinalized
      deb' = finalizeAtoms deb0
      deb'' = foldAtoms f deb' deb'
      deb''' = makeUtilsPackage $ librarySpecs $ buildDeps $ deb''
      deb'''' = finalizeAtoms deb'''
      deb''''' = foldAtoms g deb'''' deb'''' -- Apply tweaks to the debianization

      -- Create the binary packages
      f :: DebAtomKey -> DebAtom -> Atoms -> Atoms
      f (Binary b) (DHWebsite _) atoms = cabalExecBinaryPackage b atoms
      f (Binary b) (DHServer _) atoms = cabalExecBinaryPackage b atoms
      f (Binary b) (DHBackups _) atoms = setArchitecture b Any . cabalExecBinaryPackage b $ atoms
      f (Binary b) (DHExecutable _) atoms = cabalExecBinaryPackage b atoms
      f _ _ atoms = atoms

      -- Apply the hints in the atoms to the debianization
      g :: DebAtomKey -> DebAtom -> Atoms -> Atoms
      g Source (DHPriority x) deb =        modControl (\ y -> y {priority = Just x}) deb
      g Source (DHSection x) deb =         modControl (\ y -> y {section = Just x}) deb
      g (Binary b) (DHArch x) deb =        modControl (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y) deb
      g (Binary b) (DHPriority x) deb =    modControl (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) deb
      g (Binary b) (DHSection x) deb =     modControl (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) deb
      g (Binary b) (DHDescription x) deb = modControl (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y) deb
      g _ _ deb = deb

cabalExecBinaryPackage :: BinPkgName -> Atoms -> Atoms
cabalExecBinaryPackage b deb =
    modControl (\ y -> y {binaryPackages = bin : binaryPackages y}) deb
    where
      bin = BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe deb Exec (Cabal.package pkgDesc)
            , relations = binaryPackageRelations b Exec deb
            }
      pkgDesc = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ getL packageDescription deb

binaryPackageRelations :: BinPkgName -> PackageType -> Atoms -> PackageRelations
binaryPackageRelations b typ deb =
    PackageRelations
    { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                (if typ == Development then List.map anyrel' (toList (extraDevDeps deb)) else []) ++
                binaryPackageDeps b deb
    , recommends = [anyrel "${haskell:Recommends}"]
    , suggests = [anyrel "${haskell:Suggests}"]
    , preDepends = []
    , breaks = []
    , conflicts = [anyrel "${haskell:Conflicts}"] ++ binaryPackageConflicts b deb
    , provides = [anyrel "${haskell:Provides}"]
    , replaces = []
    , builtUsing = []
    }

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: Atoms -> Atoms
librarySpecs deb | isNothing (getL packageDescription deb) = deb
librarySpecs deb =
    (if doc then insertAtom (Binary (debName)) (DHLink ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt") ("/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")) else id) $
    modControl
         (\ y -> y { binaryPackages =
                               (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                               (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                               (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                               (binaryPackages y) })
         deb
    where
      doc = dev && not (noDocumentationLibrary deb)
      prof = dev && not (noProfilingLibrary deb)
      dev = isJust (Cabal.library pkgDesc)
      pkgDesc = fromMaybe (error "librarySpecs: no PackageDescription") $ getL packageDescription deb
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      debName :: BinPkgName
      debName = debianName deb Documentation (Cabal.package pkgDesc)
      hoogle = List.map toLower cabal

docSpecsParagraph :: Atoms -> PackageIdentifier -> BinaryDebDescription
docSpecsParagraph atoms pkgId =
          BinaryDebDescription
            { Debian.package = debianName atoms Documentation pkgId
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms Documentation pkgId
            , relations = binaryPackageRelations (debianName atoms Documentation pkgId) Development atoms
            }

librarySpec :: Atoms -> PackageArchitectures -> PackageType -> PackageIdentifier -> BinaryDebDescription
librarySpec atoms arch typ pkgId =
          BinaryDebDescription
            { Debian.package = debianName atoms typ pkgId
            , architecture = arch
            , binarySection = Nothing
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms typ pkgId
            , relations = binaryPackageRelations (debianName atoms typ pkgId) Development atoms
            }

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: Atoms -> Atoms
makeUtilsPackage deb | isNothing (getL packageDescription deb) = deb
makeUtilsPackage deb =
    case (Set.difference availableData installedData, Set.difference availableExec installedExec) of
      (datas, execs) | Set.null datas && Set.null execs -> deb
      (datas, execs) ->
          let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (utilsPackageName deb)
              atoms = setL packageDescription (Just pkgDesc) . makeUtilsAtoms p datas execs $ mempty
              atoms' = finalizeAtoms atoms
              deb' = foldAtoms insertAtom deb atoms' in
          modControl (\ y -> modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) y) deb'
    where
      f _ _ _ (Just bin) = bin
      f deb' p arch Nothing =
          let bin = newBinaryDebDescription p arch in
          bin {binarySection = Just (MainSection "misc"),
               relations = binaryPackageRelations p Utilities deb'}
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ getL packageDescription deb
      availableData = Set.fromList (Cabal.dataFiles pkgDesc)
      availableExec = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc)))
      installedData :: Set FilePath
      installedData = foldCabalDatas Set.insert Set.empty deb
      installedExec :: Set String
      installedExec = foldCabalExecs (Set.insert :: String -> Set String -> Set String) (Set.empty :: Set String) deb

makeUtilsAtoms :: BinPkgName -> Set FilePath -> Set String -> Atoms -> Atoms
makeUtilsAtoms p datas execs atoms0 =
    if Set.null datas && Set.null execs
    then atoms0
    else rulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n")) . g $ atoms0
    where
      g :: Atoms -> Atoms
      g atoms = Set.fold execAtom (Set.fold dataAtom atoms datas) execs
      dataAtom path atoms = installData p path path atoms
      execAtom name atoms = installCabalExec p name "usr/bin" atoms

anyrel :: String -> [Relation]
anyrel x = anyrel' (BinPkgName x)

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]
