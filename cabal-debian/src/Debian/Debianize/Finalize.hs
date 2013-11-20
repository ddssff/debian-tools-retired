-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Control.Monad (when)
import Control.Monad as List (mapM_)
import Control.Monad.State (modify, get)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (isSpace, toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.Lens.Lazy (getL)
import Data.List as List (filter, map, minimumBy, nub)
import Data.Map as Map (Map, elems, lookup, toList)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Set as Set (difference, filter, fromList, map, null, Set, singleton, toList, unions)
import qualified Data.Set as Set (member)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (pack, unlines, unpack)
import Data.Version (Version, showVersion)
import Debian.Debianize.Bundled (ghcBuiltIn)
import Debian.Debianize.ControlFile as Debian (BinaryDebDescription(..), newBinaryDebDescription, PackageRelations(..), PackageType(..), SourceDebDescription(binaryPackages, buildDependsIndep, priority, section, buildDepends))
import Debian.Debianize.Files2 (debianName, mkPkgName, mkPkgName')
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms)
import qualified Debian.Debianize.Lenses as Lenses (apacheSite, backups, binaryArchitectures, binaryPriorities, binarySections, buildDeps, buildDepsIndep, buildDir, compiler, conflicts, dataDir, debianNameMap, depends, description, epochMap, execMap, executable, extraDevDeps, extraLibMap, file, install, installCabalExec, installCabalExecTo, installData, installTo, missingDependencies, noDocumentationLibrary, noProfilingLibrary, provides, replaces, serverInfo, sourcePriority, sourceSection, utilsPackageNames, website)
import Debian.Debianize.Monad as Monad (Atoms, control, DebT, evalDebM, link, binaryArchitectures, rulesFragment, installData, installCabalExec, installDir, file, install, installTo, intermediateFile)
import Debian.Debianize.Types (InstallFile(..))
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures(Any, All), Section(..))
import Debian.Relation (BinPkgName, Relation, Relation(Rel), Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (PackageDescription)
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
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization  :: Monad m => PackageDescription -> DebT m ()
finalizeDebianization pkgDesc =
    do expandAtoms
       f
       putBuildDeps pkgDesc
       librarySpecs pkgDesc
       makeUtilsPackages pkgDesc
       expandAtoms
       g
    where
      -- Create the binary packages for the web sites, servers, backup packges, and other executables
      f :: Monad m => DebT m ()
      f = do get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.executable
             get >>= List.mapM_ (\ (b, _) -> binaryArchitectures b Any >> cabalExecBinaryPackage pkgDesc b) . Map.toList . getL Lenses.backups
             get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.serverInfo
             get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.website
      -- Turn atoms related to priority, section, and description into debianization elements
      g :: Monad m  => DebT m ()
      g = do get >>= maybe (return ()) (\ x -> control (\ y -> y {priority = Just x})) . getL Lenses.sourcePriority
             get >>= maybe (return ()) (\ x -> control (\ y -> y {section = Just x})) . getL Lenses.sourceSection
             get >>= List.mapM_ (\ (b, x) -> control (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binaryArchitectures
             get >>= List.mapM_ (\ (b, x) -> control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binaryPriorities
             get >>= List.mapM_ (\ (b, x) -> control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binarySections
             get >>= List.mapM_ (\ (b, x) -> control (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.description

putBuildDeps :: Monad m => PackageDescription -> DebT m ()
putBuildDeps pkgDesc =
    do deps <- debianBuildDeps pkgDesc
       depsIndep <- debianBuildDepsIndep pkgDesc
       Monad.control (\ y -> y { Debian.buildDepends = deps, buildDependsIndep = depsIndep })

cabalExecBinaryPackage :: Monad m => PackageDescription -> BinPkgName -> DebT m ()
cabalExecBinaryPackage pkgDesc b =
    do rels <- binaryPackageRelations b Exec
       desc <- describe Exec (Cabal.package pkgDesc)
       control (\ y -> y {binaryPackages = bin rels desc : binaryPackages y})
    where
      bin rels desc =
          BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = desc
            , relations = rels
            }

binaryPackageRelations :: Monad m => BinPkgName -> PackageType -> DebT m PackageRelations
binaryPackageRelations b typ =
    do deb <- get
       return $
         PackageRelations
         { Debian.depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            (if typ == Development then List.map (: []) (Set.toList (getL Lenses.extraDevDeps deb)) else []) ++
                            (evalDebM (binaryPackageDeps b) deb)
         , recommends = [anyrel "${haskell:Recommends}"]
         , suggests = [anyrel "${haskell:Suggests}"]
         , preDepends = []
         , breaks = []
         , conflicts = [anyrel "${haskell:Conflicts}"] ++ evalDebM (binaryPackageConflicts b) deb
         , provides_ = [anyrel "${haskell:Provides}"] ++ evalDebM (binaryPackageProvides b) deb
         , replaces_ = [anyrel "${haskell:Replaces}"] ++ evalDebM (binaryPackageReplaces b) deb
         , builtUsing = []
         }

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: Monad m => PackageDescription -> DebT m ()
librarySpecs pkgDesc =
    do debName <- debianName Documentation (Cabal.package pkgDesc)
       let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . getL Lenses.noDocumentationLibrary
       prof <- get >>= return . not . getL Lenses.noProfilingLibrary
       when (dev && doc)
            (link debName ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt",
                           "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt"))
       devSpec <- librarySpec Any Development (Cabal.package pkgDesc)
       profSpec <- librarySpec Any Profiling (Cabal.package pkgDesc)
       docSpec <- docSpecsParagraph (Cabal.package pkgDesc)
       control (\ y -> y { binaryPackages =
                               (if dev then [devSpec] else []) ++
                               (if dev && prof then [profSpec] else []) ++
                               (if dev && doc then [docSpec] else []) ++
                               (binaryPackages y) })
    where
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      hoogle = List.map toLower cabal

docSpecsParagraph :: Monad m => PackageIdentifier -> DebT m BinaryDebDescription
docSpecsParagraph pkgId =
    do name <- debianName Documentation pkgId
       rels <- binaryPackageRelations name Development
       desc <- describe Documentation pkgId
       return $
          BinaryDebDescription
            { Debian.package = name
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = desc
            , relations = rels
            }

librarySpec :: Monad m => PackageArchitectures -> PackageType -> PackageIdentifier -> DebT m BinaryDebDescription
librarySpec arch typ pkgId =
    do name <- debianName typ pkgId
       rels <- binaryPackageRelations name Development
       desc <- describe typ pkgId
       return $
          BinaryDebDescription
            { Debian.package = name
            , architecture = arch
            , binarySection = Nothing
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = desc
            , relations = rels
            }

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackages :: Monad m => PackageDescription -> DebT m ()
makeUtilsPackages pkgDesc =
    do installedData <-
           get >>= \ deb -> return $ Set.map fst ((unions . elems . getL Lenses.install $ deb) <>
                                                  (unions . elems . getL Lenses.installTo $ deb) <>
                                                  (unions . elems . getL Lenses.installData $ deb))
       installedExec <-
           get >>= \ deb -> return $ Set.map fst ((unions . elems . getL Lenses.installCabalExec $ deb) <>
                                                  (unions . elems . getL Lenses.installCabalExecTo $ deb)) <>
                                     Set.map ename (Set.fromList . elems . getL Lenses.executable $ deb)
       case (Set.difference availableData installedData, Set.difference availableExec installedExec) of
         (datas, execs) | Set.null datas && Set.null execs -> return ()
         (datas, execs) ->
             do name <- debianName Utilities (Cabal.package pkgDesc)
                uname <- get >>= return . getL Lenses.utilsPackageNames
                Set.mapM_ (makeUtilsPackage datas execs) (fromMaybe (singleton name) uname)
    where
      makeUtilsPackage datas execs p =
          do makeUtilsAtoms p datas execs
             rels <- binaryPackageRelations p Utilities
             let bin = newBinaryDebDescription p (if Set.null execs then All else Any)
                 newbin = bin {binarySection = Just (MainSection "misc"), relations = rels}
             control (\ y -> modifyBinaryDeb p (maybe newbin id) y)

      availableData = Set.fromList (Cabal.dataFiles pkgDesc)
      availableExec = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc)))
      ename i =
          case sourceDir i of
            (Nothing) -> execName i
            (Just s) ->  s </> execName i

-- | Modify the description of one of the binary debs without changing
-- the package order.
modifyBinaryDeb :: BinPkgName -> (Maybe BinaryDebDescription -> BinaryDebDescription) -> SourceDebDescription -> SourceDebDescription
modifyBinaryDeb bin f deb =
    deb {binaryPackages = bins'}
    where
      bins' = if any (\ x -> package x == bin) bins
             then List.map g (binaryPackages deb)
             else binaryPackages deb ++ [f Nothing]
      g x = if package x == bin then f (Just x) else x
      bins = binaryPackages deb

makeUtilsAtoms :: Monad m => BinPkgName -> Set FilePath -> Set String -> DebT m ()
makeUtilsAtoms p datas execs =
    if Set.null datas && Set.null execs
    then return ()
    else Set.mapM_ (\ path -> installData p (path, path)) datas >>
         Set.mapM_ (\ name -> installCabalExec p (name, "usr/bin")) execs >>
         rulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))

-- finalizeAtoms :: Atoms -> Atoms
-- finalizeAtoms atoms = execDebM expandAtoms atoms
-- finalizeAtoms atoms | atoms == mempty = atoms
-- finalizeAtoms atoms = atoms <> finalizeAtoms (execDebM expandAtoms atoms)

expandAtoms :: Monad m => DebT m ()
expandAtoms =
    do builddir <- get >>= return . fromMaybe "dist-ghc/build" . getL Lenses.buildDir
       datadir <- get >>= return . fromMaybe (error "finalizeAtoms: no dataDir")  . getL Lenses.dataDir
       expandApacheSites
       expandInstallCabalExecs builddir
       expandInstallCabalExecTo builddir
       expandInstallData datadir
       expandInstallTo
       expandFile
       expandWebsite
       expandServer
       expandBackups
       expandExecutable
    where
      expandApacheSites :: Monad m => DebT m ()
      expandApacheSites =
          do mp <- get >>= return . getL Lenses.apacheSite
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do link b ("/etc/apache2/sites-available/" ++ dom, "/etc/apache2/sites-enabled/" ++ dom)
                   installDir b log
                   file b ("/etc/apache2/sites-available" </> dom, text)

      expandInstallCabalExecs :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecs builddir =
          do mp <- get >>= return . getL Lenses.installCabalExec
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (name, dst) -> install b (builddir </> name </> name, dst)) pairs) (Map.toList mp)

      expandInstallCabalExecTo :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecTo builddir =
          do mp <- get >>= return . getL Lenses.installCabalExecTo
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (n, d) -> rulesFragment (Text.unlines
                                                                                 [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                                                 , "\tinstall -Dps " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandInstallData :: Monad m => FilePath -> DebT m ()
      expandInstallData datadir =
          do mp <- get >>= return . getL Lenses.installData
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (s, d) ->
                                                        if takeFileName s == takeFileName d
                                                        then install b (s, datadir </> makeRelative "/" (takeDirectory d))
                                                        else installTo b (s, datadir </> makeRelative "/" d)) pairs) (Map.toList mp)

      expandInstallTo :: Monad m => DebT m ()
      expandInstallTo =
          do mp <- get >>= return . getL Lenses.installTo
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (s, d) -> rulesFragment (Text.unlines
                                                                                 [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                                                 , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandFile :: Monad m => DebT m ()
      expandFile =
          do mp <- get >>= return . getL Lenses.file
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (path, s) ->
                                                        do let (destDir', destName') = splitFileName path
                                                               tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
                                                               tmpPath = tmpDir </> destName'
                                                           intermediateFile (tmpPath, s)
                                                           install p (tmpPath, destDir')) pairs) (Map.toList mp)

      expandWebsite :: Monad m => DebT m ()
      expandWebsite =
          do mp <- get >>= return . getL Lenses.website
             List.mapM_ (\ (b, site) -> modify (siteAtoms b site)) (Map.toList mp)

      expandServer :: Monad m => DebT m ()
      expandServer =
          do mp <- get >>= return . getL Lenses.serverInfo
             List.mapM_ (\ (b, x) -> modify (serverAtoms b x False)) (Map.toList mp)

      expandBackups :: Monad m => DebT m ()
      expandBackups =
          do mp <- get >>= return . getL Lenses.backups
             List.mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)

      expandExecutable :: Monad m => DebT m ()
      expandExecutable =
          do mp <- get >>= return . getL Lenses.executable
             List.mapM_ (\ (b, f) -> modify (execAtoms b f)) (Map.toList mp)

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
allBuildDepends :: Monad m => [Dependency] -> [Dependency] -> [Dependency] -> [String] -> DebT m [Dependency_]
allBuildDepends buildDepends' buildTools' pkgconfigDepends' extraLibs' =
    do atoms <- get
       return $ nub $ List.map BuildDepends buildDepends' ++
                      List.map BuildTools buildTools' ++
                      List.map PkgConfigDepends pkgconfigDepends' ++
                      List.map ExtraLibs (fixDeps atoms extraLibs')
    where
      fixDeps :: Atoms -> [String] -> [Relations]
      fixDeps atoms xs =
          concatMap (\ cab -> maybe [[[D.Rel (D.BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                    Set.toList
                                    (Map.lookup cab (getL Lenses.extraLibMap atoms))) xs

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: Monad m => PackageDescription -> DebT m D.Relations
debianBuildDeps pkgDesc =
    do deb <- get
       cDeps <- cabalDeps
       let bDeps = concat (Set.toList (getL Lenses.buildDeps deb))
           prof = not $ getL Lenses.noProfilingLibrary deb
       let xs = nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                       [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                       anyrel "cdbs",
                       anyrel "ghc"] ++
                       bDeps ++
                       (if prof then [anyrel "ghc-prof"] else []) ++
                       cDeps
       filterMissing xs
    where
      cabalDeps =
          do deps <- allBuildDepends
                          (Cabal.buildDepends pkgDesc)
                          (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc)
             mapM buildDependencies (List.filter (not . selfDependency (Cabal.package pkgDesc)) deps) >>= return . concat

debianBuildDepsIndep :: Monad m => PackageDescription -> DebT m D.Relations
debianBuildDepsIndep pkgDesc =
    do deb <- get
       doc <- get >>= return . not . getL Lenses.noDocumentationLibrary
       bDeps <- get >>= return . getL Lenses.buildDepsIndep
       let cDeps = cabalDeps deb
       let xs = if doc
                then nub $ [anyrel "ghc-doc"] ++ concat (Set.toList bDeps) ++ cDeps
                else []
       filterMissing xs
    where
      cabalDeps deb =
          concat . List.map (\ x -> evalDebM (docDependencies x) deb)
                     $ List.filter (not . selfDependency (Cabal.package pkgDesc))
                     $ evalDebM
                         (allBuildDepends
                           (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                           (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))
                         deb

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: Monad m => Dependency_ -> DebT m D.Relations
docDependencies (BuildDepends (Dependency name ranges)) = dependencies Documentation name ranges
docDependencies _ = return []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: Monad m => Dependency_ -> DebT m D.Relations
buildDependencies (BuildDepends (Dependency name ranges)) =
    do dev <- dependencies Development name ranges
       prof <- dependencies Profiling name ranges
       return $ dev ++ prof
buildDependencies dep@(ExtraLibs _) =
    do mp <- get >>= return . getL Lenses.execMap
       return $ concat $ adapt mp dep
buildDependencies dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          do mp <- get >>= return . getL Lenses.execMap
             return $ concat $ adapt mp dep
      Nothing ->
          return []

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
dependencies :: Monad m => PackageType -> PackageName -> VersionRange -> DebT m Relations
dependencies typ name cabalRange =
    do atoms <- get
       -- Compute a list of alternative debian dependencies for
       -- satisfying a cabal dependency.  The only caveat is that
       -- we may need to distribute any "and" dependencies implied
       -- by a version range over these "or" dependences.
       let alts = case Map.lookup name (getL Lenses.debianNameMap atoms) of
                    -- If there are no splits for this package just return the single dependency for the package
                    Nothing -> [(mkPkgName name typ, cabalRange')]
                    -- If there are splits create a list of (debian package name, VersionRange) pairs
                    Just splits' -> List.map (\ (n, r) -> (mkPkgName' n typ, r)) (packageRangesFromVersionSplits splits')
       mapM convert alts >>= mapM (doBundled typ name) . convert' . canonical . Or . catMaybes
    where
      convert :: Monad m => (BinPkgName, VersionRange) -> DebT m (Maybe (Rels Relation))
      convert (dname, range) =
          case isNoVersion range''' of
            True -> return Nothing
            False ->
                foldVersionRange'
                          (return $ Rel' (D.Rel dname Nothing Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.EEQ dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.SGR dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.SLT dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.GRE dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.LTE dv)) Nothing))
                          (\ x y -> debianVersion' name x >>= \ dvx ->
                                    debianVersion' name y >>= \ dvy ->
                                    return $ And [Rel' (D.Rel dname (Just (D.GRE dvx)) Nothing),
                                                  Rel' (D.Rel dname (Just (D.SLT dvy)) Nothing)])
                          (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ Or [x', y'])
                          (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ And [x', y'])
                          id
                          range''' >>= return . Just
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
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

-- If a package is bundled with the compiler we make the
-- compiler a substitute for that package.  If we were to
-- specify the virtual package (e.g. libghc-base-dev) we would
-- have to make sure not to specify a version number.
doBundled :: Monad m => PackageType -> PackageName -> [D.Relation] -> DebT m [D.Relation]
doBundled typ name rels =
    do atoms <- get
       case getL Lenses.compiler atoms of
         Nothing -> error "Compiler package name not set - can't decide what libaries are built in"
         Just compiler ->
             case ghcBuiltIn compiler name of
               True -> return $ rels ++ [D.Rel (compilerPackageName typ) Nothing Nothing]
               False -> return rels
    where
      compilerPackageName Documentation = D.BinPkgName "ghc-doc"
      compilerPackageName Profiling = D.BinPkgName "ghc-prof"
      compilerPackageName Development = D.BinPkgName "ghc"
      compilerPackageName _ = D.BinPkgName "ghc" -- whatevs

-- Convert a cabal version to a debian version, adding an epoch number if requested
debianVersion' :: Monad m => PackageName -> Version -> DebT m DebianVersion
debianVersion' name v =
    do atoms <- get
       return $ parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (getL Lenses.epochMap atoms)) ++ showVersion v)

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel' {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = List.map (List.map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel' rel) = And [Or [Rel' rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . List.map Or $ sequence $ List.map (concat . List.map unOr . unAnd . canonical) $ rels

filterMissing :: Monad m => [[Relation]] -> DebT m [[Relation]]
filterMissing rels =
    get >>= \ atoms -> return $
    List.filter (/= []) (List.map (List.filter (\ (Rel name _ _) -> not (Set.member name (getL Lenses.missingDependencies atoms)))) rels)

binaryPackageDeps :: Monad m => BinPkgName -> DebT m [[Relation]]
binaryPackageDeps b = get >>= \ atoms -> return $ maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.depends atoms))

binaryPackageConflicts :: Monad m => BinPkgName -> DebT m [[Relation]]
binaryPackageConflicts b = get >>= \ atoms -> return $ maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.conflicts atoms))

binaryPackageReplaces :: Monad m => BinPkgName -> DebT m [[Relation]]
binaryPackageReplaces b = get >>= \ atoms -> return $ maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.replaces atoms))

binaryPackageProvides :: Monad m => BinPkgName -> DebT m [[Relation]]
binaryPackageProvides b = get >>= \ atoms -> return $ maybe [] (List.map (: []) . Set.toList) (Map.lookup b (getL Lenses.provides atoms))
