{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.State.OSImage
    ( buildArchOfOS
    , osBinaryPackages
    , osSourcePackages
    , prepareOS
    , updateOS
    , syncOS
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (SomeException, throw)
import Control.Monad.Catch (MonadCatch, catch, try)
import Control.Monad.State (MonadState(get, put))
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Lens.Lazy (getL)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.OSImage (_pbuilderBuild', aptGetInstall, buildArchOfRoot, buildOS', createOSImage, localeGen, MonadOS, neuterEnv, osArch, osBaseDistro, osFullDistro, osLocalCopy, osLocalMaster, osRoot, syncLocalPool, syncOS', updateLists, osSourcePackageCache, osBinaryPackageCache)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (access, replaceFile, (~=))
import Debian.Repo.Prelude.SSH (sshCopy)
import Debian.Repo.Slice (NamedSliceList(sliceListName), Slice(sliceSource), SliceList(slices), SourcesChangedAction(SourcesChangedError), UpdateError(..))
import Debian.Repo.State (evalMonadOS, findOSKey, MonadRepos, OSKey, putOSImage)
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (verifySourcesList)
import Debian.Repo.Top (distDir, MonadTop, sourcesPath)
import Debian.Sources (DebSource(sourceUri), parseSourcesList)
import Debian.URI (URI(uriScheme))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode, shell)
import System.Process.Progress (ePutStrLn, oneResult, readProcessChunks)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

buildArchOfOS :: (MonadIO m, MonadOS m) => m Arch
buildArchOfOS = do
  root <- rootPath <$> access osRoot
  liftIO $ do
    setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
    a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
    b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
    case (code1, lines out1, code2, lines out2) of
      (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
          return $ Binary (ArchOS os) (ArchCPU cpu)
      _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)

osSourcePackages :: (MonadRepos m, MonadOS m) => m [SourcePackage]
osSourcePackages = do
  mpkgs <- access osSourcePackageCache
  maybe osSourcePackages' return mpkgs
    where
      osSourcePackages' = do
        root <- access osRoot
        arch <- access osArch
        dist <- osFullDistro
        pkgs <- sourcePackagesFromSources root arch dist
        osSourcePackageCache ~= Just pkgs
        return pkgs

osBinaryPackages :: (MonadRepos m, MonadOS m) => m [BinaryPackage]
osBinaryPackages = do
  mpkgs <- access osBinaryPackageCache
  maybe osBinaryPackages' return mpkgs
    where
      osBinaryPackages' = do
        root <- access osRoot
        arch <- access osArch
        dist <- osFullDistro
        pkgs <- binaryPackagesFromSources root arch dist
        osBinaryPackageCache ~= Just pkgs
        return pkgs

-- |Find or create and update an OS image.
prepareOS :: (MonadRepos m, MonadTop m) =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ More packages to install, but these may not be available
                                        -- immediately - e.g seereason-keyring.  Ignore exceptions.
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m OSKey
prepareOS root distro repo flush ifSourcesChanged include optional exclude components =
    do key <- findOSKey root >>= maybe (createOSImage root distro repo >>= putOSImage) return
       if flush then evalMonadOS (recreate Flushed) key else evalMonadOS updateOS key `catch` (\ (e :: UpdateError) -> evalMonadOS (recreate e) key)
       evalMonadOS (doInclude >> doLocales >> syncLocalPool) key
       return key
    where
      recreate :: (MonadOS m, MonadRepos m, MonadTop m) => UpdateError -> m ()
      recreate (Changed name path computed installed)
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       show (pretty computed) ++ "\ninstalled:\n" ++
                       show (pretty installed)
      recreate reason =
          do base <- access osBaseDistro
             sources <- sourcesPath (sliceListName base)
             dist <- distDir (sliceListName base)
             liftIO $ do ePutStrLn $ "Removing and recreating build environment at " ++ rootPath root ++ ": " ++ show reason
                         -- ePutStrLn ("removeRecursiveSafely " ++ rootPath root)
                         removeRecursiveSafely (rootPath root)
                         -- ePutStrLn ("createDirectoryIfMissing True " ++ show dist)
                         createDirectoryIfMissing True dist
                         -- ePutStrLn ("writeFile " ++ show sources ++ " " ++ show (show . osBaseDistro $ os))
                         replaceFile sources (show . pretty $ base)
             rebuildOS root distro include exclude components

      doInclude =
          do aptGetInstall (map (\ s -> (BinPkgName s, Nothing)) include)
             aptGetInstall (map (\ s -> (BinPkgName s, Nothing)) optional) `catch` (\ (e :: IOError) -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales :: (MonadOS m, MonadIO m) => m ()
      doLocales =
          do localeName <- liftIO $ try (getEnv "LANG")
             localeGen (either (\ (_ :: IOError) -> "en_US.UTF-8") id localeName)

-- | Not used, but could be a substitute for buildOS.
_pbuilderBuild :: (MonadRepos m, MonadTop m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSKey
_pbuilderBuild root distro arch repo copy _extraEssential _omitEssential _extra =
    do os <- _pbuilderBuild' root distro arch repo copy _extraEssential _omitEssential _extra
       key <- putOSImage os
       try (evalMonadOS updateOS key) >>= either (\ (e :: SomeException) -> error (show e)) return
       return key

rebuildOS :: (MonadOS m, MonadRepos m, MonadTop m) =>
             EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m ()
rebuildOS root distro include exclude components =
          do arch <- liftIO buildArchOfRoot -- This should be stored in os, but it is a Maybe - why?
             master <- access osLocalMaster
             copy <- access osLocalCopy
             _key <- buildOS root distro arch master copy include exclude components
             syncLocalPool

-- | Create a new clean build environment in root.clean FIXME: create
-- an ".incomplete" flag and remove it when build-env succeeds
buildOS :: (MonadRepos m, MonadTop m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSKey
buildOS root distro arch repo copy include exclude components =
    do os <- buildOS' root distro arch repo copy include exclude components
       key <- putOSImage os
       evalMonadOS updateOS key
       liftIO $ neuterEnv os
       return key

-- | Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateOS :: (MonadOS m, MonadRepos m) => m ()
updateOS = do
  root <- (rootPath . getL osRoot) <$> get
  liftIO $ createDirectoryIfMissing True (root </> "etc")
  liftIO $ readFile "/etc/resolv.conf" >>= writeFile (root </> "etc/resolv.conf")
  verifySources
  liftIO $ prepareDevs root
  syncLocalPool
  _elapsed <- updateLists
  code <- liftIO $ sshCopy root
  case code of
    ExitSuccess -> return ()
    _ -> error $ "sshCopy -> " ++ show code
    where
      verifySources :: (MonadOS m, MonadRepos m) => m ()
      verifySources =
          do root <- getL osRoot <$> get
             computed <- remoteOnly <$> osFullDistro
             let sourcesPath' = rootPath root </> "etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> access osBaseDistro >>= \ sources -> throw $ Missing (sliceListName sources) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       (access osBaseDistro) >>= \ sources -> throw $ Changed (sliceListName sources) sourcesPath' computed installed'
               _ -> return ()
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"
{-
    get >>= updateOS' >>= either throw put
    where
      updateOS' :: MonadRepos m => OSImage -> m (Either UpdateError OSImage)
      updateOS' os =
          do let root = getL osRoot os
             liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc")
             liftIO $ readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
             verified <- verifySources os
             case verified of
               Left x -> return $ Left x
               Right _ ->
                   do liftIO $ prepareDevs (rootPath root)
                      evalMonadOS (syncLocalPool >> updateLists) os
                      _ <- liftIO $ sshCopy (rootPath root)
                      source' <- evalMonadOS getSourcePackages' os'
                      binary <- evalMonadOS getBinaryPackages' os'
                      return . Right $ setL osSourcePackages source' $ setL osBinaryPackages binary $ os'
      verifySources :: MonadRepos m => OSImage -> m (Either UpdateError OSImage)
      verifySources os =
          do let root = getL osRoot os
             computed <- remoteOnly <$> evalMonadOS osFullDistro os
             let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (sliceListName (getL osBaseDistro os)) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       return $ Left $ Changed (sliceListName (getL osBaseDistro os)) sourcesPath' computed installed'
               _ -> return $ Right os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"
-}

-- |Prepare a minimal \/dev directory
{-# WARNING prepareDevs "This function should check all the result codes" #-}
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev devices
  where
    devices :: [(FilePath, String, Int, Int)]
    devices = [(root ++ "/dev/null", "c", 1, 3),
               (root ++ "/dev/zero", "c", 1, 5),
               (root ++ "/dev/full", "c", 1, 7),
               (root ++ "/dev/console", "c", 5, 1),
               (root ++ "/dev/random", "c", 1, 8),
               (root ++ "/dev/urandom", "c", 1, 9)] ++
              (map (\ n -> (root ++ "/dev/loop" ++ show n, "b", 7, n)) [0..7]) ++
              (map (\ n -> (root ++ "/dev/loop/" ++ show n, "b", 7, n)) [0..7])
    prepareDev (path, typ, major, minor) = do
                     createDirectoryIfMissing True (fst (splitFileName path))
                     let cmd = "mknod " ++ path ++ " " ++ typ ++ " " ++ show major ++ " " ++ show minor ++ " 2> /dev/null"
                     exists <- doesFileExist path
                     case exists of
                       False -> readProcessChunks (shell cmd) L.empty >>= return . oneResult
                       True -> return ExitSuccess

syncOS :: (MonadOS m, MonadRepos m) => OSKey -> m ()
syncOS srcKey = do
  dstOS <- get
  srcOS <- evalMonadOS get srcKey
  dstOS' <- liftIO $ syncOS' srcOS dstOS
  put dstOS'
