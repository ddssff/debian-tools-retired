-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Control.Monad (when)
import Control.Monad.State (modify, get)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (isSpace, toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL, setL)
import Data.List as List (filter, map, minimumBy, nub)
import Data.Map as Map (elems, foldWithKey, insertWith, lookup, Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Set as Set (difference, filter, fold, fromList, insert, map, null, Set, singleton, toList, union)
import qualified Data.Set as Set (member)
import Data.Text as Text (pack, unlines, unpack)
import Data.Version (showVersion)
import Debian.Debianize.Bundled (ghcBuiltIn)
import Debian.Debianize.ControlFile as Debian (BinaryDebDescription(..), modifyBinaryDeb, newBinaryDebDescription, PackageRelations(..), PackageType(..), SourceDebDescription(binaryPackages, buildDependsIndep, priority, section, buildDepends))
import Debian.Debianize.Files2 (debianName, mkPkgName, mkPkgName')
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms)
import qualified Debian.Debianize.Lenses as Lenses (apacheSite, backups, binaryArchitectures, binaryPriorities, binarySections, buildDeps, buildDepsIndep, buildDir, compiler, conflicts, control, dataDir, debianNameMap, depends, description, epochMap, execMap, executable, extraDevDeps, extraLibMap, file, install, installCabalExec, installCabalExecTo, installData, installDir, installTo, intermediateFiles, link, missingDependencies, noDocumentationLibrary, noProfilingLibrary, packageDescription, provides, replaces, rulesFragments, serverInfo, sourcePriority, sourceSection, utilsPackageNames, website)
import Debian.Debianize.Monad as Monad (Atoms, control, DebT, evalDebM, execDebM, link)
import Debian.Debianize.Types (InstallFile(..))
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures(Any, All), Section(..))
import Debian.Relation (BinPkgName, Relation, Relation(Rel), Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Version (parseDebianVersion)
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (allBuildInfo, BuildInfo(buildTools, extraLibs, pkgconfigDepends))
import qualified Distribution.PackageDescription as Cabal (BuildInfo(buildable), Executable(buildInfo, exeName), PackageDescription(buildDepends, dataFiles, executables, library, package))
import Distribution.Version (anyVersion, asVersionIntervals, earlierVersion, foldVersionRange', fromVersionIntervals, intersectVersionRanges, isNoVersion, laterVersion, orEarlierVersion, orLaterVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
import Distribution.Version.Invert (invertVersionRange)
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
finalizeDebianization  :: Monad m => DebT m ()
finalizeDebianization =
    modify (g . finalizeAtoms . makeUtilsPackage . execDebM librarySpecs . putBuildDeps . f . finalizeAtoms)
    where
      -- Create the binary packages for the web sites, servers, backup packges, and other executables
      f :: Atoms -> Atoms
      f atoms = (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> execDebM (cabalExecBinaryPackage b) atoms'') atoms' (getL Lenses.website atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> execDebM (cabalExecBinaryPackage b) atoms'') atoms' (getL Lenses.serverInfo atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> modL Lenses.binaryArchitectures (Map.insertWith (flip const) b Any) . execDebM (cabalExecBinaryPackage b) $ atoms'') atoms' (getL Lenses.backups atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> execDebM (cabalExecBinaryPackage b) atoms'') atoms' (getL Lenses.executable atoms)) $ atoms
      -- Turn atoms related to priority, section, and description into debianization elements
      g :: Atoms -> Atoms
      g atoms = (\ atoms' -> maybe atoms' (\ x -> modL Lenses.control (\ y -> y {priority = Just x}) atoms') (getL Lenses.sourcePriority atoms)) .
                (\ atoms' -> maybe atoms' (\ x -> modL Lenses.control (\ y -> y {section = Just x}) atoms') (getL Lenses.sourceSection atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL Lenses.control (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Lenses.binaryArchitectures atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL Lenses.control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Lenses.binaryPriorities atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL Lenses.control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Lenses.binarySections atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL Lenses.control (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Lenses.description atoms)) $ atoms

      putBuildDeps :: Atoms -> Atoms
      putBuildDeps deb = execDebM (Monad.control (\ y -> y { Debian.buildDepends = debianBuildDeps deb, buildDependsIndep = debianBuildDepsIndep deb })) deb

cabalExecBinaryPackage :: Monad m => BinPkgName -> DebT m ()
cabalExecBinaryPackage b =
    do deb <- get
       rels <- binaryPackageRelations b Exec
       control (\ y -> y {binaryPackages = bin deb rels : binaryPackages y})
    where
      bin deb rels =
          BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe deb Exec (Cabal.package (pkgDesc deb))
            , relations = rels
            }
      pkgDesc deb = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ getL Lenses.packageDescription deb

binaryPackageRelations :: Monad m => BinPkgName -> PackageType -> DebT m PackageRelations
binaryPackageRelations b typ =
    do deb <- get
       return $
         PackageRelations
         { Debian.depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            (if typ == Development then List.map (: []) (toList (getL Lenses.extraDevDeps deb)) else []) ++
                            binaryPackageDeps b deb
         , recommends = [anyrel "${haskell:Recommends}"]
         , suggests = [anyrel "${haskell:Suggests}"]
         , preDepends = []
         , breaks = []
         , conflicts = [anyrel "${haskell:Conflicts}"] ++ binaryPackageConflicts b deb
         , provides_ = [anyrel "${haskell:Provides}"] ++ binaryPackageProvides b deb
         , replaces_ = [anyrel "${haskell:Replaces}"] ++ binaryPackageReplaces b deb
         , builtUsing = []
         }

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: Monad m => DebT m ()
librarySpecs =
    do deb <- get
       maybe (return ()) (librarySpecs' deb)  (getL Lenses.packageDescription deb)
    where
      librarySpecs' deb pkgDesc =
          do when doc (link debName ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt",
                                     "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt"))
             control (\ y -> y { binaryPackages =
                                 (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                                 (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                                 (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                                 (binaryPackages y) })
          where
            doc = dev && not (getL Lenses.noDocumentationLibrary deb)
            prof = dev && not (getL Lenses.noProfilingLibrary deb)
            dev = isJust (Cabal.library pkgDesc)
            PackageName cabal = pkgName (Cabal.package pkgDesc)
            debName :: BinPkgName
            debName = evalDebM (debianName Documentation (Cabal.package pkgDesc)) deb
            hoogle = List.map toLower cabal
{-
librarySpecs deb | isNothing (getL packageDescription deb) = deb
librarySpecs deb =
    (if doc
     then modL link (Map.insertWith Set.union debName (singleton ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt", "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")))
     else id) $
    modL Lenses.control
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
-}

docSpecsParagraph :: Atoms -> PackageIdentifier -> BinaryDebDescription
docSpecsParagraph atoms pkgId =
    let rels = evalDebM (binaryPackageRelations (evalDebM (debianName Documentation pkgId) atoms) Development) atoms in
          BinaryDebDescription
            { Debian.package = evalDebM (debianName Documentation pkgId) atoms
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms Documentation pkgId
            , relations = rels
            }

librarySpec :: Atoms -> PackageArchitectures -> PackageType -> PackageIdentifier -> BinaryDebDescription
librarySpec atoms arch typ pkgId =
    let rels = evalDebM (binaryPackageRelations (evalDebM (debianName typ pkgId) atoms) Development) atoms in
          BinaryDebDescription
            { Debian.package = evalDebM (debianName typ pkgId) atoms
            , architecture = arch
            , binarySection = Nothing
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms typ pkgId
            , relations = rels
            }

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: Atoms -> Atoms
makeUtilsPackage deb | isNothing (getL Lenses.packageDescription deb) = deb
makeUtilsPackage deb =
    case (Set.difference availableData installedData, Set.difference availableExec installedExec) of
      (datas, execs) | Set.null datas && Set.null execs -> deb
      (datas, execs) ->
          let ps = fromMaybe (singleton (evalDebM (debianName Utilities (Cabal.package pkgDesc)) deb)) (getL Lenses.utilsPackageNames deb)
              deb' = Set.fold (h datas execs) deb ps in
              -- deb' = setL packageDescription (Just pkgDesc) (makeUtilsAtoms p datas execs deb) in
          Set.fold (g execs) deb' ps
          -- modL control (\ y -> modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) y) deb'
    where
      h datas execs p deb' = setL Lenses.packageDescription (Just pkgDesc) (makeUtilsAtoms p datas execs deb')
      g execs p deb' = modL Lenses.control (\ y -> modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) y) deb'
      f _ _ _ (Just bin) = bin
      f deb' p arch Nothing =
          let bin = newBinaryDebDescription p arch in
          bin {binarySection = Just (MainSection "misc"),
               relations = evalDebM (binaryPackageRelations p Utilities) deb'}
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ getL Lenses.packageDescription deb
      availableData = Set.fromList (Cabal.dataFiles pkgDesc)
      availableExec = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc)))
      installedData :: Set FilePath
      installedData = Set.fromList ((List.map fst . concat . List.map toList . elems $ getL Lenses.install deb) <>
                                    (List.map fst . concat . List.map toList . elems $ getL Lenses.installTo deb) <>
                                    (List.map fst . concat . List.map toList . elems $ getL Lenses.installData deb))
      installedExec :: Set String
      installedExec = Set.fromList ((List.map fst . concat . List.map toList . elems $ getL Lenses.installCabalExec deb) <>
                                    (List.map fst . concat . List.map toList .  elems $ getL Lenses.installCabalExecTo deb) <>
                                    (List.map ename . elems $ getL Lenses.executable deb))
          where ename i =
                    case sourceDir i of
                      (Nothing) -> execName i
                      (Just s) ->  s </> execName i
      -- installedExec = foldCabalExecs (Set.insert :: String -> Set String -> Set String) (Set.empty :: Set String) deb

makeUtilsAtoms :: BinPkgName -> Set FilePath -> Set String -> Atoms -> Atoms
makeUtilsAtoms p datas execs atoms0 =
    if Set.null datas && Set.null execs
    then atoms0
    else modL Lenses.rulesFragments (Set.insert (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) . g $ atoms0
    where
      g :: Atoms -> Atoms
      g atoms = Set.fold execAtom (Set.fold dataAtom atoms datas) execs
      dataAtom path atoms = modL Lenses.installData (insertWith union p (singleton (path, path))) atoms
      execAtom name atoms = modL Lenses.installCabalExec (insertWith union p (singleton (name, "usr/bin"))) atoms

finalizeAtoms :: Atoms -> Atoms
finalizeAtoms atoms | atoms == mempty = atoms
finalizeAtoms atoms = atoms <> finalizeAtoms (expandAtoms atoms)

expandAtoms :: Atoms -> Atoms
expandAtoms old =
    expandApacheSite .
    expandInstallCabalExec .
    expandInstallCabalExecTo .
    expandInstallData .
    expandInstallTo .
    expandFile .
    expandWebsite .
    expandServer .
    expandBackups .
    expandExecutable $
    mempty
    where
      expandApacheSite :: Atoms -> Atoms
      expandApacheSite new =
          foldWithKey (\ b (dom, log, text) atoms ->
                           modL Lenses.link (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available/" ++ dom, "/etc/apache2/sites-enabled/" ++ dom))) .
                           modL Lenses.installDir (Map.insertWith Set.union b (singleton log)) .
                           modL Lenses.file (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available" </> dom, text))) $
                           atoms)
                      new
                      (getL Lenses.apacheSite old)

      expandInstallCabalExec :: Atoms -> Atoms
      expandInstallCabalExec new =
          foldWithKey (\ b pairs atoms -> Set.fold (\ (name, dst) atoms' -> modL Lenses.install (Map.insertWith Set.union b (singleton (builddir </> name </> name, dst))) atoms')
                                                    atoms
                                                    pairs)
                      new
                      (getL Lenses.installCabalExec old)
          where
            builddir = fromMaybe {-(error "finalizeAtoms: no buildDir")-} "dist-ghc/build" (getL Lenses.buildDir old)

      expandInstallCabalExecTo :: Atoms -> Atoms
      expandInstallCabalExecTo new =
          foldWithKey (\ b pairs atoms ->
                           Set.fold (\ (n, d) atoms' ->
                                         modL Lenses.rulesFragments (Set.insert (Text.unlines
                                                                          [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                                          , "\tinstall -Dps " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) atoms')
                                    atoms
                                    pairs)
                      new
                      (getL Lenses.installCabalExecTo old)
          where
            builddir = fromMaybe {-(error "finalizeAtoms: no buildDir")-} "dist-ghc/build" (getL Lenses.buildDir old)

      expandInstallData :: Atoms -> Atoms
      expandInstallData new =
          foldWithKey (\ b pairs atoms ->
                           Set.fold (\ (s, d) atoms' ->
                                         if takeFileName s == takeFileName d
                                         then modL Lenses.install (Map.insertWith Set.union b (singleton (s, datadir </> makeRelative "/" (takeDirectory d)))) atoms'
                                         else modL Lenses.installTo (Map.insertWith Set.union b (singleton (s, datadir </> makeRelative "/" d))) atoms')
                                    atoms
                                    pairs)
                      new
                      (getL Lenses.installData old)
          where
            datadir = fromMaybe (error "finalizeAtoms: no dataDir") $ getL Lenses.dataDir old

      expandInstallTo :: Atoms -> Atoms
      expandInstallTo new =
          foldWithKey (\ p pairs atoms ->
                           Set.fold (\ (s, d) atoms' ->
                                         modL Lenses.rulesFragments (Set.insert (Text.unlines
                                                                          [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                                          , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) atoms') atoms pairs)
                      new
                      (getL Lenses.installTo old)

      expandFile :: Atoms -> Atoms
      expandFile new =
          foldWithKey (\ p pairs atoms ->
                           Set.fold (\ (path, s) atoms' ->
                                         let (destDir', destName') = splitFileName path
                                             tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
                                             tmpPath = tmpDir </> destName' in
                                         modL Lenses.intermediateFiles (Set.insert (tmpPath, s)) .
                                         modL Lenses.install (Map.insertWith Set.union p (singleton (tmpPath, destDir'))) $
                                         atoms')
                                    atoms
                                    pairs)
                      new
                      (getL Lenses.file old)

      expandWebsite :: Atoms -> Atoms
      expandWebsite new = foldWithKey siteAtoms new (getL Lenses.website old)

      expandServer :: Atoms -> Atoms
      expandServer new = foldWithKey (\ b x atoms -> serverAtoms b x False atoms) new (getL Lenses.serverInfo old)

      expandBackups :: Atoms -> Atoms
      expandBackups new = foldWithKey backupAtoms new (getL Lenses.backups old)

      expandExecutable :: Atoms -> Atoms
      expandExecutable new = foldWithKey execAtoms new (getL Lenses.executable old)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

-- | In cabal a self dependency probably means the library is needed
-- while building the executables.  In debian it would mean that the
-- package needs an earlier version of itself to build, so we use this
-- to filter such dependencies out.
selfDependency :: PackageIdentifier -> Dependency_ -> Bool
selfDependency pkgId (BuildDepends (Dependency name _)) = name == pkgName pkgId
selfDependency _ _ = False

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Atoms -> [Dependency] -> [Dependency] -> [Dependency] -> [String] -> [Dependency_]
allBuildDepends atoms buildDepends' buildTools' pkgconfigDepends' extraLibs' =
    nub $ List.map BuildDepends buildDepends' ++
          List.map BuildTools buildTools' ++
          List.map PkgConfigDepends pkgconfigDepends' ++
          List.map ExtraLibs (fixDeps extraLibs')
    where
      fixDeps :: [String] -> [Relations]
      fixDeps xs = concatMap (\ cab -> maybe [[[D.Rel (D.BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                             Set.toList
                                             (Map.lookup cab (getL Lenses.extraLibMap atoms))) xs

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: Atoms -> D.Relations
debianBuildDeps deb =
    filterMissing deb $
    nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
            concat (Set.toList (getL Lenses.buildDeps deb)) ++
            (if getL Lenses.noProfilingLibrary deb then [] else [anyrel "ghc-prof"]) ++
            cabalDeps (getL Lenses.packageDescription deb)
    where
      cabalDeps Nothing = []
      cabalDeps (Just pkgDesc) =
          (concat $ List.map (buildDependencies deb)
                  $ List.filter (not . selfDependency (Cabal.package pkgDesc))
                  $ allBuildDepends
                          deb (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc))

debianBuildDepsIndep :: Atoms -> D.Relations
debianBuildDepsIndep deb =
    filterMissing deb $
    if getL Lenses.noDocumentationLibrary deb
    then []
    else nub $ [anyrel "ghc-doc"] ++
               concat (Set.toList (getL Lenses.buildDepsIndep deb)) ++
               cabalDeps (getL Lenses.packageDescription deb)
    where
      cabalDeps Nothing = []
      cabalDeps (Just pkgDesc) =
          (concat . List.map (docDependencies deb)
                      $ List.filter (not . selfDependency (Cabal.package pkgDesc))
                      $ allBuildDepends
                            deb (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                            (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: Atoms -> Dependency_ -> D.Relations
docDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Documentation name ranges
docDependencies _ _ = []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: Atoms -> Dependency_ -> D.Relations
buildDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Development name ranges ++
    dependencies atoms Profiling name ranges
buildDependencies atoms dep@(ExtraLibs _) =
    concat (adapt (getL Lenses.execMap atoms) dep)
buildDependencies atoms dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          concat (adapt (getL Lenses.execMap atoms) dep)
      Nothing ->
          []

adapt :: Map.Map String Relations -> Dependency_ -> [Relations]
adapt execMap (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt execMap (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency (PackageName pkg) _)) = [[[D.Rel (D.BinPkgName pkg) Nothing Nothing]]]

-- There are two reasons this may not work, or may work
-- incorrectly: (1) the build environment may be a different
-- distribution than the parent environment (the environment the
-- autobuilder was run from), so the packages in that
-- environment might have different names, and (2) the package
-- we are looking for may not be installed in the parent
-- environment.
aptFile :: String -> [Relations] -- Maybe would probably be more correct
aptFile pkg =
    unsafePerformIO $
    do ret <- readProcessWithExitCode "apt-file" ["-l", "search", pkg ++ ".pc"] ""
       return $ case ret of
                  (ExitSuccess, out, _) ->
                      case takeWhile (not . isSpace) out of
                        "" -> error $ "Unable to locate a debian package containing the build tool " ++ pkg ++
                                      ", try using --exec-map " ++ pkg ++ "=<debname> or execMap " ++ show pkg ++
                                      " [[Rel (BinPkgName \"<debname>\") Nothing Nothing]]"
                        s -> [[[D.Rel (D.BinPkgName s) Nothing Nothing]]]
                  _ -> []

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: Atoms -> PackageType -> PackageName -> VersionRange -> Relations
dependencies atoms typ name cabalRange =
    List.map doBundled $ convert' (canonical (Or (catMaybes (List.map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(BinPkgName, VersionRange)]
      alts = case Map.lookup name (getL Lenses.debianNameMap atoms) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName name typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits' -> List.map (\ (n, r) -> (mkPkgName' n typ, r)) (packageRangesFromVersionSplits splits')

      convert :: (BinPkgName, VersionRange) -> Maybe (Rels Relation)
      convert (dname, range) =
          if isNoVersion range'''
          then Nothing
          else Just $
               foldVersionRange'
                 (Rel' (D.Rel dname Nothing Nothing))
                 (\ v -> Rel' (D.Rel dname (Just (D.EEQ (dv v))) Nothing))
                 (\ v -> Rel' (D.Rel dname (Just (D.SGR (dv v))) Nothing))
                 (\ v -> Rel' (D.Rel dname (Just (D.SLT (dv v))) Nothing))
                 (\ v -> Rel' (D.Rel dname (Just (D.GRE (dv v))) Nothing))
                 (\ v -> Rel' (D.Rel dname (Just (D.LTE (dv v))) Nothing))
                 (\ x y -> And [Rel' (D.Rel dname (Just (D.GRE (dv x))) Nothing), Rel' (D.Rel dname (Just (D.SLT (dv y))) Nothing)])
                 (\ x y -> Or [x, y])
                 (\ x y -> And [x, y])
                 id
                 range'''
          where
            -- Choose the simpler of the two
            range''' = canon (simpler range' range'')
            -- Unrestrict the range for versions that we know don't exist for this debian package
            range'' = canon (unionVersionRanges range' (invertVersionRange range))
            -- Restrict the range to the versions specified for this debian package
            range' = intersectVersionRanges cabalRange' range
            -- When we see a cabal equals dependency we need to turn it into
            -- a wildcard because the resulting debian version numbers have
            -- various suffixes added.
      cabalRange' =
          foldVersionRange'
            anyVersion
            withinVersion  -- <- Here we are turning equals into wildcard
            laterVersion
            earlierVersion
            orLaterVersion
            orEarlierVersion
            (\ lb ub -> intersectVersionRanges (orLaterVersion lb) (earlierVersion ub))
            unionVersionRanges
            intersectVersionRanges
            id
            cabalRange
      -- Convert a cabal version to a debian version, adding an epoch number if requested
      dv v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (getL Lenses.epochMap atoms)) ++ showVersion v)
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

      -- If a package is bundled with the compiler we make the
      -- compiler a substitute for that package.  If we were to
      -- specify the virtual package (e.g. libghc-base-dev) we would
      -- have to make sure not to specify a version number.
      doBundled :: [D.Relation] -> [D.Relation]
      doBundled rels | ghcBuiltIn (fromMaybe (error "dependencies") $ getL Lenses.compiler atoms) name = rels ++ [D.Rel (compilerPackageName typ) Nothing Nothing]
      doBundled rels = rels

      compilerPackageName Documentation = D.BinPkgName "ghc-doc"
      compilerPackageName Profiling = D.BinPkgName "ghc-prof"
      compilerPackageName Development = D.BinPkgName "ghc"
      compilerPackageName _ = D.BinPkgName "ghc" -- whatevs

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel' {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = List.map (List.map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel' rel) = And [Or [Rel' rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . List.map Or $ sequence $ List.map (concat . List.map unOr . unAnd . canonical) $ rels

filterMissing :: Atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    List.filter (/= []) (List.map (List.filter (\ (Rel name _ _) -> not (Set.member name (getL Lenses.missingDependencies atoms)))) rels)

binaryPackageDeps :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageDeps b atoms = maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.depends atoms))

binaryPackageConflicts :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageConflicts b atoms = maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.conflicts atoms))

binaryPackageReplaces :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageReplaces b atoms = maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.replaces atoms))

binaryPackageProvides :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageProvides b atoms = maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.provides atoms))
