-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Data.Char (toLower)
import Data.List as List (map)
import Data.Maybe
import Data.Monoid (Monoid, mempty)
import Data.Set as Set (Set, difference, fromList, null, insert, toList, filter, fold, empty, map)
import Data.Text (pack)
import Debian.Debianize.AtomsClass (HasAtoms(putAtoms), DebAtomKey(..), DebAtom(..))
import Debian.Debianize.AtomsType (insertAtom,
                                   packageDescription, setArchitecture, setPackageDescription, binaryPackageDeps,
                                   binaryPackageConflicts, noProfilingLibrary, noDocumentationLibrary, utilsPackageName, extraDevDeps,
                                   sourceDebDescription, setSourceDebDescription,
                                   foldAtomsFinalized, rulesFragment, installData, installCabalExec, foldCabalDatas, foldCabalExecs)
import Debian.Debianize.Combinators (describe, buildDeps)
import Debian.Debianize.Dependencies (debianName)
import Debian.Debianize.Types.DebControl as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                                    newBinaryDebDescription, modifyBinaryDeb)
import Debian.Debianize.Types.PackageType (PackageType(Exec, Development, Profiling, Documentation, Utilities))
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
finalizeDebianization  :: HasAtoms deb => deb -> deb
finalizeDebianization deb0 =
    foldAtomsFinalized g deb'' deb'' -- Apply tweaks to the debianization
    where
      -- Fixme - makeUtilsPackage does stuff that needs to go through foldAtomsFinalized
      deb' = foldAtomsFinalized f (putAtoms mempty deb0) deb0
      deb'' = makeUtilsPackage $ librarySpecs $ buildDeps $ deb'

      -- Create the binary packages
      f :: HasAtoms deb => DebAtomKey -> DebAtom -> deb -> deb
      f k@(Binary b) a@(DHWebsite _) = insertAtom k a . cabalExecBinaryPackage b
      f k@(Binary b) a@(DHServer _) = insertAtom k a . cabalExecBinaryPackage b
      f k@(Binary b) a@(DHBackups _) =
          insertAtom k a .
          setArchitecture b Any .
          cabalExecBinaryPackage b
      f k@(Binary b) a@(DHExecutable _) = insertAtom k a . cabalExecBinaryPackage b
      f k a = insertAtom k a

      -- Apply the hints in the atoms to the debianization
      g :: HasAtoms deb => DebAtomKey -> DebAtom -> deb -> deb
      g (Binary b) (DHArch x) deb =        setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {architecture = x}) (sourceDebDescription deb)) deb
      g (Binary b) (DHPriority x) deb =    setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {binaryPriority = Just x}) (sourceDebDescription deb)) deb
      g Source (DHPriority x) deb =        setSourceDebDescription ((sourceDebDescription deb) {priority = Just x}) deb
      g (Binary b) (DHSection x) deb =     setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {binarySection = Just x}) (sourceDebDescription deb)) deb
      g Source (DHSection x) deb =         setSourceDebDescription ((sourceDebDescription deb) {section = Just x}) deb
      g (Binary b) (DHDescription x) deb = setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {Debian.description = x}) (sourceDebDescription deb)) deb
      g _ _ deb = deb

cabalExecBinaryPackage :: HasAtoms deb => BinPkgName -> deb -> deb
cabalExecBinaryPackage b deb =
    setSourceDebDescription ((sourceDebDescription deb) {binaryPackages = bin : binaryPackages (sourceDebDescription deb)}) deb
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
      pkgDesc = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ packageDescription deb

binaryPackageRelations :: HasAtoms atoms => BinPkgName -> PackageType -> atoms -> PackageRelations
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
librarySpecs :: HasAtoms deb => deb -> deb
librarySpecs deb | isNothing (packageDescription deb) = deb
librarySpecs deb =
    (if doc then insertAtom (Binary (debName)) (DHLink ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt") ("/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")) else id) $
    setSourceDebDescription
            ((sourceDebDescription deb)
              { binaryPackages =
                    (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                    (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                    (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                    (binaryPackages (sourceDebDescription deb)) }) deb
    where
      doc = dev && not (noDocumentationLibrary deb)
      prof = dev && not (noProfilingLibrary deb)
      dev = isJust (Cabal.library pkgDesc)
      pkgDesc = fromMaybe (error "librarySpecs: no PackageDescription") $ packageDescription deb
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      debName :: BinPkgName
      debName = debianName deb Documentation (Cabal.package pkgDesc)
      hoogle = List.map toLower cabal

docSpecsParagraph :: HasAtoms atoms => atoms -> PackageIdentifier -> BinaryDebDescription
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

librarySpec :: HasAtoms atoms => atoms -> PackageArchitectures -> PackageType -> PackageIdentifier -> BinaryDebDescription
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
makeUtilsPackage :: forall deb. HasAtoms deb => deb -> deb
makeUtilsPackage deb | isNothing (packageDescription deb) = deb
makeUtilsPackage deb =
    case (Set.difference availableData installedData, Set.difference availableExec installedExec) of
      (datas, execs) | Set.null datas && Set.null execs -> deb
      (datas, execs) ->
          let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (utilsPackageName deb)
              atoms = setPackageDescription pkgDesc . makeUtilsAtoms p datas execs $ (mempty :: deb)
              deb' = foldAtomsFinalized insertAtom deb atoms in
          setSourceDebDescription (modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) (sourceDebDescription deb')) deb'
    where
      f _ _ _ (Just bin) = bin
      f deb' p arch Nothing =
          let bin = newBinaryDebDescription p arch in
          bin {binarySection = Just (MainSection "misc"),
               relations = binaryPackageRelations p Utilities deb'}
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ packageDescription deb
      availableData = Set.fromList (Cabal.dataFiles pkgDesc)
      availableExec = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc)))
      installedData :: Set FilePath
      installedData = foldCabalDatas Set.insert Set.empty deb
      installedExec :: Set String
      installedExec = foldCabalExecs (Set.insert :: String -> Set String -> Set String) (Set.empty :: Set String) (deb :: deb)

makeUtilsAtoms :: HasAtoms atoms => BinPkgName -> Set FilePath -> Set String -> atoms -> atoms
makeUtilsAtoms p datas execs atoms0 =
    if Set.null datas && Set.null execs
    then atoms0
    else rulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n")) . g $ atoms0
    where
      g :: HasAtoms atoms => atoms -> atoms
      g atoms = Set.fold execAtom (Set.fold dataAtom atoms datas) execs
      dataAtom path atoms = installData p path path atoms
      execAtom name atoms = installCabalExec p name "usr/bin" atoms

anyrel :: String -> [Relation]
anyrel x = anyrel' (BinPkgName x)

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]
