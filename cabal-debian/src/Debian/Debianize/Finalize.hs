-- | Compute the debianization of a cabal package.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( debianization
    , finalizeDebianization -- external use deprecated - used in test script
    ) where

import Control.Monad (when)
import Control.Monad as List (mapM_)
import Control.Monad.State (get, modify, lift)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (isSpace, toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL, access)
import Data.List as List (filter, intercalate, map, minimumBy, nub, unlines)
import Data.Map as Map (elems, lookup, Map, toList)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Set as Set (difference, filter, fromList, map, null, Set, singleton, toList, unions)
import qualified Data.Set as Set (member)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (pack, Text, unlines, unpack)
import Data.Version (showVersion, Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Bundled (ghcBuiltIn)
import Debian.Debianize.Facts.Types as Debian (BinaryDebDescription(..), newBinaryDebDescription, PackageRelations(..), PackageType(..), SourceDebDescription(binaryPackages, buildDependsIndep, priority, section, buildDepends))
import qualified Debian.Debianize.Facts.Types as D (BinaryDebDescription(..), PackageRelations(..), PackageType(..), SourceDebDescription(..))
import Debian.Debianize.Files2 (debianName, mkPkgName, mkPkgName')
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms, watchAtom)
import Debian.Debianize.Input (inputChangeLog, inputLicenseFile, inputMaintainer, inputCompiler, inputCabalization, dataDir)
import Debian.Debianize.Facts.Lenses as Lenses (apacheSite, backups, binaryArchitectures, binaryPriorities, binarySections, buildDeps, buildDepsIndep, buildDir, changelog, comments, compat, conflicts, control, copyright, debianNameMap, debVersion, depends, description, epochMap, execMap, executable, extraDevDeps, extraLibMap, file, install, installCabalExec, installCabalExecTo, installData, installTo, maintainer, missingDependencies, noDocumentationLibrary, noProfilingLibrary, provides, replaces, revision, serverInfo, sourcePackageName, sourcePriority, sourceSection, utilsPackageNames, website, binaryArchitectures, control, file, install, installCabalExec, installData, installDir, installTo, intermediateFiles, link, rulesFragments, changelog, compat, maintainer, sourcePackageName, sourcePriority, sourceSection, watch, verbosity)
import Debian.Debianize.Facts.Monad as Monad (Atoms, DebT, evalDebM, askTop)
import Debian.Debianize.Facts.Types (showAtoms)
import Debian.Debianize.Options (compileCommandlineArgs, compileEnvironmentArgs)
import Debian.Debianize.Facts.Types (InstallFile(..))
import Debian.Debianize.Utility (foldEmpty, withCurrentDirectory, (+=), (~=), (+++=), (++=), (%=))
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.Orphans ()
import Debian.Policy (getDebhelperCompatLevel, PackageArchitectures(Any, All), PackagePriority(Optional), Section(..))
import Debian.Relation (BinPkgName, BinPkgName(BinPkgName), Relation, Relation(Rel), Relations, SrcPkgName)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Release (parseReleaseName)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (buildDebianVersion, DebianVersion, parseDebianVersion)
import Distribution.License (License(AllRightsReserved))
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription as Cabal (allBuildInfo, BuildInfo(buildTools, extraLibs, pkgconfigDepends))
import qualified Distribution.PackageDescription as Cabal (BuildInfo(buildable), Executable(buildInfo, exeName), PackageDescription(buildDepends, dataFiles, executables, library, package), PackageDescription(license))
import Distribution.Version (anyVersion, asVersionIntervals, earlierVersion, foldVersionRange', fromVersionIntervals, intersectVersionRanges, isNoVersion, laterVersion, orEarlierVersion, orLaterVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
import Distribution.Version.Invert (invertVersionRange)
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianization :: DebT IO () -> DebT IO () -> DebT IO ()
debianization init customize =
    do top <- askTop
       init
       compileEnvironmentArgs
       compileCommandlineArgs
       customize
       log <- inputChangeLog
       pkgDesc <- inputCabalization >>= either (error $ "cabalToDebianization: Failed to read cabal file in " ++ show top) return
       date <- lift getCurrentLocalRFC822Time
       maint <- inputMaintainer
       level <- get >>= return. getL Lenses.compat >>= lift . maybe getDebhelperCompatLevel (return . Just)
       copyrt <- lift . withCurrentDirectory top . inputLicenseFile $ pkgDesc
       debianization' date copyrt maint level log

debianization' :: MonadIO m =>
                  String              -- ^ current date
               -> Maybe Text          -- ^ copyright
               -> NameAddr            -- ^ maintainer
               -> Maybe Int	      -- ^ Default standards version
               -> Either IOError ChangeLog
               -> DebT m ()
debianization' date copy maint level log =
    do deb <- get
       pkgDesc <- inputCabalization >>= either (error "debianization") return
       addExtraLibDependencies
       copyright ~= (case getL Lenses.copyright deb of
                       Just x -> Just x
                       Nothing -> case copy of
                                    Just x -> Just (Right x)
                                    Nothing -> Just (Left (Cabal.license pkgDesc)))
       watch ~= Just (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       -- FIXME - sourceSection and sourcePriority should be settable
       sourceSection ~= Just (MainSection "haskell")
       sourcePriority ~= Just Optional
       either (\ _ -> return ()) ((changelog ~=) . Just) log
       maybe (return ()) ((compat ~=) . Just) level
       finalizeChangelog maint date
       finalizeControl
       modify (modL Lenses.copyright (maybe (Just (Left AllRightsReserved)) Just))
       finalizeDebianization pkgDesc
       vb <- access verbosity
       when (vb >= 3) (get >>= liftIO . showAtoms)

dropFutureChangelogEntries :: MonadIO m => DebT m ()
dropFutureChangelogEntries =
    do deb <- get
       ver <- debianVersion
       maybe (return ())
             (\ (ChangeLog entries) -> changelog ~= (Just (ChangeLog (dropWhile (\ entry -> logVersion entry > ver) entries))))
             (getL Lenses.changelog deb)


-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
debianVersion :: MonadIO m => DebT m DebianVersion
debianVersion =
    do pkgDesc <- inputCabalization >>= either (error "versionInfo: no PackageDescription") return
       let pkgId = Cabal.package pkgDesc
       epoch <- debianEpoch (pkgName pkgId)
       debVer <- get >>= return . getL Lenses.debVersion
       case debVer of
         Just override
             | override < parseDebianVersion (show (pretty (pkgVersion pkgId))) ->
                 error ("Version from --deb-version (" ++ show (pretty override) ++
                        ") is older than hackage version (" ++ show (pretty (pkgVersion pkgId)) ++
                        "), maybe you need to unpin this package?")
         Just override -> return override
         Nothing ->
             do let ver = show (pretty (pkgVersion pkgId))
                rev <- get >>= return . getL Lenses.revision >>= return . foldEmpty Nothing Just . fromMaybe ""
                return $ buildDebianVersion epoch ver rev

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> DebT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . getL Lenses.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
sourceName :: MonadIO m => DebT m SrcPkgName
sourceName =
    do deb <- get
       pkgDesc <- inputCabalization >>= either (error "versionInfo: no PackageDescription") return
       let pkgId = Cabal.package pkgDesc
           name = maybe (evalDebM (debianName D.Source' pkgId) deb) id (getL Lenses.sourcePackageName deb)
       sourcePackageName ~= Just name
       return name

finalizeControl :: Monad m => DebT m ()
finalizeControl =
    do Just src <- get >>= return . getL Lenses.sourcePackageName
       Just maint <- get >>= return . getL Lenses.maintainer
       control %= (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: MonadIO m => NameAddr -> String -> DebT m ()
finalizeChangelog maint date =
    do deb <- get
       ver <- debianVersion
       src <- sourceName
       maintainer ~= Just maint
       dropFutureChangelogEntries
       let newEntry = Entry { logPackage = show (pretty src)
                            , logVersion = ver
                            , logDists = [parseReleaseName "unstable"]
                            , logUrgency = "low"
                            , logComments = List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack)
                                            (fromMaybe [["Debianization generated by cabal-debian"]] (getL Lenses.comments deb))
                            , logWho = show (pretty maint)
                            , logDate = date }
       case getL Lenses.changelog deb of
         -- If there is already a changelog entry with the exact
         -- version number we need to create, modify it.
         Just (ChangeLog (oldEntry@(Entry {logVersion = d}) : older))
             | d == ver -> changelog ~= Just (ChangeLog (mergeChangelogEntries oldEntry newEntry : older))
         -- Otherwise create a new log entry
         Just (ChangeLog entries) -> changelog ~= Just (ChangeLog (newEntry : entries))
         Nothing -> changelog ~= Just (ChangeLog [newEntry])

mergeChangelogEntries :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
mergeChangelogEntries old new =
    old { logComments = logComments old ++ logComments new
        , logDate = logDate new }

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: MonadIO m => DebT m ()
addExtraLibDependencies =
    do deb <- get
       pkgDesc <- inputCabalization >>= either (error "addExtraLibDependencies: no PackageDescription") return
       control %= (\ y -> y {D.binaryPackages = List.map (f deb pkgDesc) (D.binaryPackages (getL Lenses.control deb))})
    where
      f deb pkgDesc bin
          | evalDebM (debianName D.Development (Cabal.package pkgDesc)) deb == D.package bin
              = bin { D.relations = g deb pkgDesc (D.relations bin) }
      f _ _ bin = bin
      g deb pkgDesc rels =
          rels { D.depends =
                     concat $
                          [D.depends rels] ++
                          concatMap (\ cab -> maybe [[[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                                    Set.toList
                                                    (Map.lookup cab (getL Lenses.extraLibMap deb)))
                                    (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc) }

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization  :: MonadIO m => PackageDescription -> DebT m ()
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
      f :: MonadIO m => DebT m ()
      f = do get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.executable
             get >>= List.mapM_ (\ (b, _) -> binaryArchitectures ++= (b, Any) >>
                                             cabalExecBinaryPackage pkgDesc b) . Map.toList . getL Lenses.backups
             get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.serverInfo
             get >>= List.mapM_ (cabalExecBinaryPackage pkgDesc . fst) . Map.toList . getL Lenses.website
      -- Turn atoms related to priority, section, and description into debianization elements
      g :: MonadIO m  => DebT m ()
      g = do get >>= maybe (return ()) (\ x -> control %= (\ y -> y {priority = Just x})) . getL Lenses.sourcePriority
             get >>= maybe (return ()) (\ x -> control %= (\ y -> y {section = Just x})) . getL Lenses.sourceSection
             get >>= List.mapM_ (\ (b, x) -> control %= (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binaryArchitectures
             get >>= List.mapM_ (\ (b, x) -> control %= (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binaryPriorities
             get >>= List.mapM_ (\ (b, x) -> control %= (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.binarySections
             get >>= List.mapM_ (\ (b, x) -> control %= (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y)) . Map.toList . getL Lenses.description

putBuildDeps :: MonadIO m => PackageDescription -> DebT m ()
putBuildDeps pkgDesc =
    do deps <- debianBuildDeps pkgDesc
       depsIndep <- debianBuildDepsIndep pkgDesc
       control %= (\ y -> y { Debian.buildDepends = deps, buildDependsIndep = depsIndep })

cabalExecBinaryPackage :: MonadIO m => PackageDescription -> BinPkgName -> DebT m ()
cabalExecBinaryPackage pkgDesc b =
    do rels <- binaryPackageRelations b Exec
       desc <- describe Exec (Cabal.package pkgDesc)
       control %= (\ y -> y {binaryPackages = bin rels desc : binaryPackages y})
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
         , D.conflicts = [anyrel "${haskell:Conflicts}"] ++ evalDebM (binaryPackageConflicts b) deb
         , provides_ = [anyrel "${haskell:Provides}"] ++ evalDebM (binaryPackageProvides b) deb
         , replaces_ = [anyrel "${haskell:Replaces}"] ++ evalDebM (binaryPackageReplaces b) deb
         , builtUsing = []
         }

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: MonadIO m => PackageDescription -> DebT m ()
librarySpecs pkgDesc =
    do debName <- debianName Documentation (Cabal.package pkgDesc)
       let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . getL Lenses.noDocumentationLibrary
       prof <- get >>= return . not . getL Lenses.noProfilingLibrary
       when (dev && doc)
            (link +++= (debName, ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt",
                                  "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")))
       devSpec <- librarySpec Any Development (Cabal.package pkgDesc)
       profSpec <- librarySpec Any Profiling (Cabal.package pkgDesc)
       docSpec <- docSpecsParagraph (Cabal.package pkgDesc)
       control %= (\ y -> y { binaryPackages =
                               (if dev then [devSpec] else []) ++
                               (if dev && prof then [profSpec] else []) ++
                               (if dev && doc then [docSpec] else []) ++
                               (binaryPackages y) })
    where
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      hoogle = List.map toLower cabal

docSpecsParagraph :: MonadIO m => PackageIdentifier -> DebT m BinaryDebDescription
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

librarySpec :: MonadIO m => PackageArchitectures -> PackageType -> PackageIdentifier -> DebT m BinaryDebDescription
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
                Lenses.utilsPackageNames %= (\ s -> if Set.null s then singleton name else s)
                Set.mapM_ (makeUtilsPackage datas execs) =<< access Lenses.utilsPackageNames
    where
      makeUtilsPackage datas execs p =
          do makeUtilsAtoms p datas execs
             rels <- binaryPackageRelations p Utilities
             let bin = newBinaryDebDescription p (if Set.null execs then All else Any)
                 newbin = bin {binarySection = Just (MainSection "misc"), relations = rels}
             control %= (\ y -> modifyBinaryDeb p (maybe newbin id) y)

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
    else Set.mapM_ (\ path -> installData +++= (p, (path, path))) datas >>
         Set.mapM_ (\ name -> installCabalExec +++= (p, (name, "usr/bin"))) execs >>
         rulesFragments += (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))

-- finalizeAtoms :: Atoms -> Atoms
-- finalizeAtoms atoms = execDebM expandAtoms atoms
-- finalizeAtoms atoms | atoms == mempty = atoms
-- finalizeAtoms atoms = atoms <> finalizeAtoms (execDebM expandAtoms atoms)

expandAtoms :: MonadIO m => DebT m ()
expandAtoms =
    do builddir <- get >>= return . fromMaybe "dist-ghc/build" . getL Lenses.buildDir
       dDir <- inputCabalization >>= either (error "expandAtoms") (return . dataDir)
       expandApacheSites
       expandInstallCabalExecs builddir
       expandInstallCabalExecTo builddir
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
          do mp <- get >>= return . getL Lenses.apacheSite
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do link +++= (b, ("/etc/apache2/sites-available/" ++ dom, "/etc/apache2/sites-enabled/" ++ dom))
                   installDir +++= (b, log)
                   file +++= (b, ("/etc/apache2/sites-available" </> dom, text))

      expandInstallCabalExecs :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecs builddir =
          do mp <- get >>= return . getL Lenses.installCabalExec
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (name, dst) -> install +++= (b, (builddir </> name </> name, dst))) pairs) (Map.toList mp)

      expandInstallCabalExecTo :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecTo builddir =
          do mp <- get >>= return . getL Lenses.installCabalExecTo
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (n, d) -> rulesFragments += (Text.unlines
                                                                                   [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                                                   , "\tinstall -Dps " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandInstallData :: Monad m => FilePath -> DebT m ()
      expandInstallData dDir =
          do mp <- get >>= return . getL Lenses.installData
             List.mapM_ (\ (b, pairs) -> Set.mapM_ (\ (s, d) ->
                                                        if takeFileName s == takeFileName d
                                                        then install +++= (b, (s, (dDir </> makeRelative "/" (takeDirectory d))))
                                                        else installTo +++= (b, (s, (dDir </> makeRelative "/" d)))) pairs) (Map.toList mp)

      expandInstallTo :: Monad m => DebT m ()
      expandInstallTo =
          do mp <- get >>= return . getL Lenses.installTo
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (s, d) -> rulesFragments += (Text.unlines
                                                                                   [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                                                   , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) pairs) (Map.toList mp)

      expandFile :: Monad m => DebT m ()
      expandFile =
          do mp <- get >>= return . getL Lenses.file
             List.mapM_ (\ (p, pairs) -> Set.mapM_ (\ (path, s) ->
                                                        do let (destDir', destName') = splitFileName path
                                                               tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
                                                               tmpPath = tmpDir </> destName'
                                                           intermediateFiles += (tmpPath, s)
                                                           install +++= (p, (tmpPath, destDir'))) pairs) (Map.toList mp)

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
debianBuildDeps :: MonadIO m => PackageDescription -> DebT m D.Relations
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

debianBuildDepsIndep :: MonadIO m => PackageDescription -> DebT m D.Relations
debianBuildDepsIndep pkgDesc =
    do doc <- get >>= return . not . getL Lenses.noDocumentationLibrary
       bDeps <- get >>= return . getL Lenses.buildDepsIndep
       cDeps <- cabalDeps
       let xs = if doc
                then nub $ [anyrel "ghc-doc"] ++ concat (Set.toList bDeps) ++ concat cDeps
                else []
       filterMissing xs
    where
      cabalDeps =
          do deps <- allBuildDepends
                           (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                           (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc)
             let deps' = List.filter (not . selfDependency (Cabal.package pkgDesc)) deps
             mapM docDependencies deps'
{-
      cabalDeps deb =
          concat . List.map (\ x -> evalDebM (docDependencies x) deb)
                     $ List.filter (not . selfDependency (Cabal.package pkgDesc))
                     $ evalDebM
                         (allBuildDepends
                           (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                           (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))
                         deb
-}

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: MonadIO m => Dependency_ -> DebT m D.Relations
docDependencies (BuildDepends (Dependency name ranges)) = dependencies Documentation name ranges
docDependencies _ = return []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: MonadIO m => Dependency_ -> DebT m D.Relations
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
adapt mp (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg mp)
adapt mp (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg mp)
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
dependencies :: MonadIO m => PackageType -> PackageName -> VersionRange -> DebT m Relations
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
doBundled :: MonadIO m => PackageType -> PackageName -> [D.Relation] -> DebT m [D.Relation]
doBundled typ name rels =
    do comp <- inputCompiler
       case ghcBuiltIn comp name of
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
