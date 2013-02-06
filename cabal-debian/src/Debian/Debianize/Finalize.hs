-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Data.Char (toLower)
import Data.Lens.Lazy (setL, getL, modL)
import Data.List as List (map)
import Data.Map as Map (insertWith, foldWithKey)
import Data.Maybe
import Data.Set as Set (Set, difference, fromList, null, insert, toList, filter, fold, empty, map, union, singleton)
import Data.Text (pack)
import Debian.Debianize.Atoms as Atoms
    (HasAtoms(packageDescription, control, binaryArchitectures, rulesFragments, website, serverInfo, link,
              backups, executable, sourcePriority, sourceSection, binaryPriorities, binarySections, description),
     Atoms, noProfilingLibrary, noDocumentationLibrary, utilsPackageName, extraDevDeps,
     finalizeAtoms, installData, installCabalExec, foldCabalDatas, foldCabalExecs,
     binaryPackageDeps, binaryPackageConflicts)
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
      deb'' = f deb'
      deb''' = makeUtilsPackage $ librarySpecs $ buildDeps $ deb''
      deb'''' = finalizeAtoms deb'''
      deb''''' = g deb'''' -- Apply tweaks to the debianization

      -- Create the binary packages
      f :: Atoms -> Atoms
      f atoms = (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL website atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL serverInfo atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> modL binaryArchitectures (Map.insertWith (\ _ x -> x) b Any) . cabalExecBinaryPackage b $ atoms'') atoms' (getL backups atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL executable atoms)) $ atoms
      -- Apply the hints in the atoms to the debianization
      g :: Atoms -> Atoms
      g atoms = (\ atoms' -> maybe atoms' (\ x -> modL control (\ y -> y {priority = Just x}) atoms') (getL sourcePriority atoms)) .
                (\ atoms' -> maybe atoms' (\ x -> modL control (\ y -> y {section = Just x}) atoms') (getL sourceSection atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binaryArchitectures atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binaryPriorities atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binarySections atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Atoms.description atoms)) $ atoms

cabalExecBinaryPackage :: BinPkgName -> Atoms -> Atoms
cabalExecBinaryPackage b deb =
    modL control (\ y -> y {binaryPackages = bin : binaryPackages y}) deb
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
                (if typ == Development then List.map anyrel' (toList (getL extraDevDeps deb)) else []) ++
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
    (if doc then modL link (Map.insertWith Set.union debName (singleton ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt", "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt"))) else id) $
    modL control
         (\ y -> y { binaryPackages =
                               (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                               (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                               (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                               (binaryPackages y) })
         deb
    where
      doc = dev && not (getL noDocumentationLibrary deb)
      prof = dev && not (getL noProfilingLibrary deb)
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
          let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (getL utilsPackageName deb)
              deb' = setL packageDescription (Just pkgDesc) . makeUtilsAtoms p datas execs $ deb in
          modL control (\ y -> modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) y) deb'
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
    else modL rulesFragments (Set.insert (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) . g $ atoms0
    where
      g :: Atoms -> Atoms
      g atoms = Set.fold execAtom (Set.fold dataAtom atoms datas) execs
      dataAtom path atoms = modL installData (insertWith union p (singleton (path, path))) atoms
      execAtom name atoms = modL installCabalExec (insertWith union p (singleton (name, "usr/bin"))) atoms

anyrel :: String -> [Relation]
anyrel x = anyrel' (BinPkgName x)

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]
