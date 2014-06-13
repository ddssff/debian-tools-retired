-- | Compute the debianization of a cabal package.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( debianization
    , finalizeDebianization' -- external use deprecated - used in test script
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad as List (mapM_)
import Control.Monad.State (get, modify)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Lens.Lazy (access, getL)
import Data.List as List (intercalate, map, nub, unlines)
import Data.Map as Map (delete, elems, lookup, map, Map, toList, unionsWith)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Set as Set (difference, filter, fromList, map, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (pack, unlines, unpack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.BuildDependencies (debianBuildDeps, debianBuildDepsIndep)
import Debian.Debianize.Changelog (dropFutureEntries)
import Debian.Debianize.DebianName (debianName)
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms, watchAtom)
import Debian.Debianize.Input (dataDir, inputCabalization, inputChangeLog, inputMaintainer)
import Debian.Debianize.Monad as Monad (DebT)
import Debian.Debianize.Options (compileCommandlineArgs, compileEnvironmentArgs)
import Debian.Debianize.Prelude ((%=), (+++=), (+=), foldEmpty, fromEmpty, fromSingleton, (~=), (~?=))
import Debian.Debianize.Types (Top)
import qualified Debian.Debianize.Types as T (apacheSite, backups, binaryArchitectures, binaryPackages, binarySection, breaks, buildDepends, buildDependsIndep, buildDir, builtUsing, changelog, comments, compat, conflicts, debianDescription, debVersion, depends, epochMap, executable, extraDevDeps, extraLibMap, file, install, installCabalExec, installCabalExecTo, installData, installDir, installTo, intermediateFiles, license, link, maintainer, noDocumentationLibrary, noProfilingLibrary, noHoogle, packageDescription, packageType, preDepends, provides, recommends, replaces, revision, rulesFragments, serverInfo, source, sourcePackageName, sourcePriority, sourceSection, suggests, utilsPackageNames, verbosity, watch, website)
import qualified Debian.Debianize.Types.Atoms as A (InstallFile(execName, sourceDir), showAtoms, ghcVersion)
import qualified Debian.Debianize.Types.BinaryDebDescription as B (BinaryDebDescription, package, PackageType(Development, Documentation, Exec, Profiling, Source', Utilities))
import Debian.Orphans ()
import Debian.Policy (getDebhelperCompatLevel, haskellMaintainer, PackageArchitectures(Any, All), PackagePriority(Optional), Section(..))
import Debian.Pretty (pretty)
import Debian.Relation (BinPkgName, BinPkgName(BinPkgName), Relation(Rel), Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..))
import Debian.Release (parseReleaseName)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (buildDebianVersion, DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription as Cabal (allBuildInfo, BuildInfo(buildable, extraLibs), Executable(buildInfo, exeName))
import qualified Distribution.PackageDescription as Cabal (PackageDescription(dataDir, dataFiles, executables, library, license, package))
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.FilePath ((<.>), (</>), makeRelative, splitFileName, takeDirectory, takeFileName)

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianization :: Top -> DebT IO () -> DebT IO () -> DebT IO ()
debianization top init customize =
    do compileEnvironmentArgs
       compileCommandlineArgs
       inputCabalization top
       inputChangeLog top
       inputMaintainer
       init
       customize
       finalizeDebianization'

-- | Do some light IO and call finalizeDebianization.
finalizeDebianization' :: (MonadIO m, Functor m) => DebT m ()
finalizeDebianization' =
    do date <- liftIO getCurrentLocalRFC822Time
       debhelperCompat <- liftIO getDebhelperCompatLevel
       finalizeDebianization date debhelperCompat
       access T.verbosity >>= \ vb -> when (vb >= 3) (get >>= liftIO . A.showAtoms)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization  :: (MonadIO m, Functor m) => String -> Maybe Int -> DebT m ()
finalizeDebianization date debhelperCompat =
    do addExtraLibDependencies
       Just pkgDesc <- access T.packageDescription
       T.watch ~?= Just (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       T.sourceSection ~?= Just (MainSection "haskell")
       T.sourcePriority ~?= Just Optional
       T.compat ~?= debhelperCompat
       finalizeChangelog date
       finalizeControl
       T.license ~?= Just (Cabal.license pkgDesc)
       expandAtoms
       -- Create the binary packages for the web sites, servers, backup packges, and other executables
       access T.executable >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.backups >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.serverInfo >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.website >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       putBuildDeps pkgDesc
       librarySpecs pkgDesc
       makeUtilsPackages pkgDesc
       -- Sketchy - I think more things that need expanded could be generated by the code
       -- executed since the last expandAtoms.  Anyway, should be idempotent.
       expandAtoms
       -- Turn atoms related to priority, section, and description into debianization elements
       finalizeDescriptions

-- | Compute the final values of the BinaryDebDescription record
-- description fields from the cabal descriptions and the values that
-- have already been set.
finalizeDescriptions :: (Monad m, Functor m) => DebT m ()
finalizeDescriptions = access T.binaryPackages >>= List.mapM_ finalizeDescription

finalizeDescription :: (Monad m, Functor m) => B.BinaryDebDescription -> DebT m ()
finalizeDescription bdd =
    do let b = getL B.package bdd
       cabDesc <- describe b
       T.debianDescription b ~?= Just cabDesc

-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
debianVersion :: Monad m => DebT m DebianVersion
debianVersion =
    do pkgDesc <- access T.packageDescription >>= maybe (error "debianVersion: no PackageDescription") return
       let pkgId = Cabal.package pkgDesc
       epoch <- debianEpoch (pkgName pkgId)
       debVer <- access T.debVersion
       case debVer of
         Just override
             | override < parseDebianVersion (show (pretty (pkgVersion pkgId))) ->
                 error ("Version from --deb-version (" ++ show (pretty override) ++
                        ") is older than hackage version (" ++ show (pretty (pkgVersion pkgId)) ++
                        "), maybe you need to unpin this package?")
         Just override -> return override
         Nothing ->
             do let ver = show (pretty (pkgVersion pkgId))
                rev <- get >>= return . getL T.revision >>= return . foldEmpty Nothing Just . fromMaybe ""
                return $ buildDebianVersion epoch ver rev

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> DebT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . getL T.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
finalizeSourceName :: Monad m => DebT m ()
finalizeSourceName =
    do debName <- debianName B.Source'
       T.sourcePackageName ~?= Just debName

finalizeMaintainer :: Monad m => DebT m ()
finalizeMaintainer =
    T.maintainer ~?= Just haskellMaintainer

finalizeControl :: Monad m => DebT m ()
finalizeControl =
    do finalizeSourceName
       finalizeMaintainer
       Just src <- access T.sourcePackageName
       maint <- access T.maintainer >>= return . fromMaybe (error "No maintainer")
       T.source ~= Just src
       T.maintainer ~= Just maint
       -- control %= (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: Monad m => String -> DebT m ()
finalizeChangelog date =
    do finalizeSourceName
       finalizeMaintainer
       ver <- debianVersion
       src <- access T.sourcePackageName
       Just maint <- access T.maintainer
       cmts <- access T.comments
       T.changelog %= fmap (dropFutureEntries ver)
       T.changelog %= fixLog src ver cmts maint
    where
      -- Ensure that the package name is correct in the first log entry.
      fixLog src ver cmts _maint (Just (ChangeLog (entry : older))) | logVersion entry == ver =
          Just (ChangeLog (entry { logPackage = show (pretty src)
                                 , logComments = logComments entry ++ "\n" ++
                                                 (List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack) (fromMaybe [] cmts))
                                 } : older))
      -- The newest log entry isn't exactly ver, build a new entry.
      fixLog src ver cmts maint log =
          Just (ChangeLog (Entry { logPackage = show (pretty src)
                                 , logVersion = ver
                                 , logDists = [parseReleaseName "unstable"]
                                 , logUrgency = "low"
                                 , logComments = List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack)
                                                 (fromMaybe [["Debianization generated by cabal-debian"]] cmts)
                                 , logWho = show (pretty maint)
                                 , logDate = date } : maybe [] (\ (ChangeLog entries) -> entries) log))

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: (Monad m, Functor m) => DebT m ()
addExtraLibDependencies =
    do pkgDesc <- access T.packageDescription >>= maybe (error "addExtraLibDependencies: no PackageDescription") return
       devName <- debianName B.Development
       libMap <- access T.extraLibMap
       binNames <- List.map (getL B.package) <$> access T.binaryPackages
       when (any (== devName) binNames) (T.depends devName %= \ deps -> deps ++ g pkgDesc libMap)
    where
      g :: PackageDescription -> Map String Relations -> Relations
      g pkgDesc libMap = concatMap (devDep libMap) (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)
      devDep :: Map String Relations -> String -> Relations
      devDep libMap cab = maybe [[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]] id (Map.lookup cab libMap)

putBuildDeps :: MonadIO m => PackageDescription -> DebT m ()
putBuildDeps pkgDesc =
    do deps <- debianBuildDeps pkgDesc
       depsIndep <- debianBuildDepsIndep pkgDesc
       T.buildDepends ~= deps
       T.buildDependsIndep ~= depsIndep

cabalExecBinaryPackage :: Monad m => BinPkgName -> DebT m ()
cabalExecBinaryPackage b =
    do T.packageType b ~?= Just B.Exec
       T.binaryArchitectures b ~?= Just Any
       T.binarySection b ~?= Just (MainSection "misc")
       desc <- describe b
       T.debianDescription b ~?= Just desc
       binaryPackageRelations b B.Exec
    where

binaryPackageRelations :: Monad m => BinPkgName -> B.PackageType -> DebT m ()
binaryPackageRelations b typ =
    do edds <- access T.extraDevDeps
       T.depends b %= \ rels -> [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                                (if typ == B.Development then edds else []) ++ rels
       T.recommends b %= \ rels -> [anyrel "${haskell:Recommends}"] ++ rels
       T.suggests b %= \ rels -> [anyrel "${haskell:Suggests}"] ++ rels
       T.preDepends b ~= []
       T.breaks b ~= []
       T.conflicts b %= \ rels -> [anyrel "${haskell:Conflicts}"] ++ rels
       T.provides b %= \ rels -> [anyrel "${haskell:Provides}"] ++ rels
       T.replaces b %= \ rels -> [anyrel "${haskell:Replaces}"] ++ rels
       T.builtUsing b ~= []

librarySpecs :: Monad m => PackageDescription -> DebT m ()
librarySpecs pkgDesc =
    do debName <- debianName B.Documentation
       let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . getL T.noDocumentationLibrary
       prof <- get >>= return . not . getL T.noProfilingLibrary
       (CompilerId cfl _ _) <- access A.ghcVersion
       hoogle <- get >>= return . not . getL T.noHoogle
       when dev (librarySpec Any B.Development)
       when (dev && prof && cfl == GHC) (librarySpec Any B.Profiling)
       when (dev && doc && hoogle)
            (do docSpecsParagraph
                T.link +++= (debName, singleton ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt",
                                                 "/usr/lib/ghc-doc/hoogle" </> List.map toLower cabal <.> "txt")))
    where
      PackageName cabal = pkgName (Cabal.package pkgDesc)

docSpecsParagraph :: Monad m => DebT m ()
docSpecsParagraph =
    do b <- debianName B.Documentation
       binaryPackageRelations b B.Development -- not sure why this isn't Documentation, but I think there's a "good" reason
       T.packageType b ~?= Just B.Documentation
       desc <- describe b
       T.packageType b ~?= Just B.Documentation
       T.binaryArchitectures b ~= Just All
       T.binarySection b ~?= Just (MainSection "doc")
       T.debianDescription b ~?= Just desc

librarySpec :: Monad m => PackageArchitectures -> B.PackageType -> DebT m ()
librarySpec arch typ =
    do b <- debianName typ
       binaryPackageRelations b B.Development
       T.packageType b ~?= Just typ
       desc <- describe b
       T.packageType b ~?= Just typ
       T.binaryArchitectures b ~?= Just arch
       T.debianDescription b ~?= Just desc

-- | Make sure all data and executable files are assigned to at least
-- one binary package and make sure all binary packages are in the
-- package list in the source deb description.  If there are left over
-- files, assign them to the packages returned by the
-- utilsPackageNames lens, and make sure those packages are in the
-- source deb description.
makeUtilsPackages :: forall m. (Monad m, Functor m) => PackageDescription -> DebT m ()
makeUtilsPackages pkgDesc =
    do -- Files the cabal package expects to be installed
       -- Files that are already assigned to any binary deb
       installedDataMap <- Map.unionsWith Set.union
                           <$> (sequence [(Map.map (Set.map fst) <$> access T.install),
                                          (Map.map (Set.map fst) <$> access T.installTo),
                                          (Map.map (Set.map fst) <$> access T.installData)]) :: DebT m (Map BinPkgName (Set FilePath))
       installedExecMap <- Map.unionsWith Set.union
                           <$> (sequence [(Map.map (Set.map fst) <$> access T.installCabalExec),
                                          (Map.map (Set.map fst) <$> access T.installCabalExecTo)]) :: DebT m (Map BinPkgName (Set String))

       -- The names of cabal executables that go into eponymous debs
       insExecPkg <- access T.executable >>= return . Set.map ename . Set.fromList . elems

       let installedData = Set.map (\ a -> (a, a)) $ Set.unions (Map.elems installedDataMap)
           installedExec = Set.unions (Map.elems installedExecMap)

       let prefixPath = Cabal.dataDir pkgDesc
       let dataFilePaths = Set.fromList (zip (List.map (prefixPath </>) (Cabal.dataFiles pkgDesc)) (Cabal.dataFiles pkgDesc)) :: Set (FilePath, FilePath)
           execFilePaths = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc))) :: Set FilePath
       let availableData = Set.union installedData dataFilePaths
           availableExec = Set.union installedExec execFilePaths

       access T.utilsPackageNames >>= \ names ->
           when (Set.null names) (debianName B.Utilities >>= \ name -> T.utilsPackageNames ~= singleton name)
       utilsPackages <- access T.utilsPackageNames

       -- Files that are installed into packages other than the utils packages
       let installedDataOther = Set.map (\ a -> (a, a)) $ Set.unions $ Map.elems $ foldr (Map.delete) installedDataMap (Set.toList utilsPackages)
           installedExecOther =
               Set.union (tr "insExecPkg: " insExecPkg) $
                                Set.unions $ Map.elems $ foldr (Map.delete) (tr "installedExec: " installedExecMap) (Set.toList utilsPackages)

       -- Files that will be in utils packages
       let utilsData = Set.difference availableData installedDataOther
           utilsExec = Set.difference (tr "availableExec: " availableExec) (tr "installedExecOther: " installedExecOther)
       -- Files that still need to be assigned to the utils packages
       let utilsDataMissing = Set.difference utilsData installedData
           utilsExecMissing = Set.difference utilsExec installedExec
       -- If any files belong in the utils packages, make sure they exist
       when (not (Set.null utilsData && Set.null utilsExec))
            (Set.mapM_ (\ p -> do -- This is really for all binary debs except the libraries - I'm not sure why
                                  T.rulesFragments += (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp"))
                                  T.binaryArchitectures p ~?= Just (if Set.null utilsExec then All else Any)
                                  T.binarySection p ~?= Just (MainSection "misc")
                                  binaryPackageRelations p B.Utilities) utilsPackages)
       -- Add the unassigned files to the utils packages
       Set.mapM_ (\ p -> Set.mapM_ (\ pair -> T.installData +++= (p, singleton pair)) utilsDataMissing) utilsPackages
       Set.mapM_ (\ p -> Set.mapM_ (\ name -> T.installCabalExec +++= (p, singleton (name, "usr/bin"))) (tr "utilsExecMissing: " utilsExecMissing)) utilsPackages
    where
      ename i =
          case A.sourceDir i of
            (Nothing) -> A.execName i
            (Just s) ->  s </> A.execName i

tr :: Show a => String -> a -> a
tr _label x = {- trace ("(trace " ++ _label ++ show x ++ ")") -} x

expandAtoms :: Monad m => DebT m ()
expandAtoms =
    do builddir <- get >>= return . fromEmpty (singleton "dist-ghc/build") . getL T.buildDir
       dDir <- access T.packageDescription >>= maybe (error "expandAtoms") (return . dataDir)
       expandApacheSites
       expandInstallCabalExecs (fromSingleton (error "no builddir") (\ xs -> error $ "multiple builddirs:" ++ show xs) builddir)
       expandInstallCabalExecTo (fromSingleton (error "no builddir") (\ xs -> error $ "multiple builddirs:" ++ show xs) builddir)
       expandInstallData dDir
       expandInstallTo
       expandFile
       expandWebsite
       expandServer
       expandBackups
       expandExecutable
    where
      expandApacheSites :: Monad m => DebT m ()
      expandApacheSites =
          do mp <- get >>= return . getL T.apacheSite
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do T.link +++= (b, singleton ("/etc/apache2/sites-available/" ++ dom, "/etc/apache2/sites-enabled/" ++ dom))
                   T.installDir +++= (b, singleton log)
                   T.file +++= (b, singleton ("/etc/apache2/sites-available" </> dom, text))

      expandInstallCabalExecs :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecs builddir =
          do mp <- get >>= return . getL T.installCabalExec
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (name, dst) -> T.install +++= (b, singleton (builddir </> name </> name, dst))) pairs) (Map.toList mp)

      expandInstallCabalExecTo :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecTo builddir =
          do mp <- get >>= return . getL T.installCabalExecTo
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (n, d) -> T.rulesFragments += (Text.unlines
                                                                                     [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                                                     , "\tinstall -Dps " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandInstallData :: Monad m => FilePath -> DebT m ()
      expandInstallData dDir =
          do mp <- get >>= return . getL T.installData
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (s, d) ->
                                                        if takeFileName s == takeFileName d
                                                        then T.install +++= (b, singleton (s, (dDir </> makeRelative "/" (takeDirectory d))))
                                                        else T.installTo +++= (b, singleton (s, (dDir </> makeRelative "/" d)))) pairs) (Map.toList mp)

      expandInstallTo :: Monad m => DebT m ()
      expandInstallTo =
          do mp <- get >>= return . getL T.installTo
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (s, d) -> T.rulesFragments += (Text.unlines
                                                                                     [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                                                     , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandFile :: Monad m => DebT m ()
      expandFile =
          do mp <- get >>= return . getL T.file
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (path, s) ->
                                                        do let (destDir', destName') = splitFileName path
                                                               tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
                                                               tmpPath = tmpDir </> destName'
                                                           T.intermediateFiles += (tmpPath, s)
                                                           T.install +++= (p, singleton (tmpPath, destDir'))) pairs) (Map.toList mp)

      expandWebsite :: Monad m => DebT m ()
      expandWebsite =
          do mp <- get >>= return . getL T.website
             List.mapM_ (\ (b, site) -> modify (siteAtoms b site)) (Map.toList mp)

      expandServer :: Monad m => DebT m ()
      expandServer =
          do mp <- get >>= return . getL T.serverInfo
             List.mapM_ (\ (b, x) -> modify (serverAtoms b x False)) (Map.toList mp)

      expandBackups :: Monad m => DebT m ()
      expandBackups =
          do mp <- get >>= return . getL T.backups
             List.mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)

      expandExecutable :: Monad m => DebT m ()
      expandExecutable =
          do mp <- get >>= return . getL T.executable
             List.mapM_ (\ (b, f) -> modify (execAtoms b f)) (Map.toList mp)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]
