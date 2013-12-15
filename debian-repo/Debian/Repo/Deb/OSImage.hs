{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Deb.OSImage
    ( prepareEnv
    , updateEnv
    , syncPool
    ) where

import Control.Exception (evaluate, SomeException, try)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.List (intercalate)
import Debian.Arch (Arch)
import Debian.Release (ReleaseName(..))
import Debian.Repo.Cache (buildArchOfRoot, distDir, sliceIndexes, SourcesChangedAction(SourcesChangedError), sourcesPath)
import Debian.Repo.Monads.Apt (MonadApt)
import Debian.Repo.Monads.Deb (MonadDeb)
import Debian.Repo.Monads.Top (MonadTop(askTop))
import Debian.Repo.OSImage (neuterEnv, OSImage(..), updateLists)
import Debian.Repo.Package (binaryPackagesOfIndex', sourcePackagesOfIndex')
import Debian.Repo.SSH (sshCopy)
import Debian.Repo.Slice (binarySlices, sourceSlices, verifySourcesList)
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Repo.Types.AptImage (AptBuildCache(..))
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(rootPath), rootEnvPath)
import Debian.Repo.Types.LocalRepository (copyLocalRepo, LocalRepository, prepareLocalRepository)
import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Types.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(..))
import Debian.Sources (DebSource(sourceDist, sourceUri), SliceName(sliceName))
import Debian.URI (URI(uriScheme), uriToString')
import Extra.Files (replaceFile)
import "Extra" Extra.List (isSublistOf)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import System.Process (shell)
import System.Process.Progress (collectOutputs, doOutput, ePutStr, ePutStrLn, foldOutputsL, oneResult, quieter, readProcessChunks, runProcess)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

getSourcePackages :: MonadApt m => OSImage -> m [SourcePackage]
getSourcePackages os =
    do indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages :: MonadApt m => OSImage -> m [BinaryPackage]
getBinaryPackages os =
    do indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- |Create or update an OS image in which packages can be built.
prepareEnv :: MonadDeb m =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> FilePath                  -- ^ The location of the local upload repository
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call (unimplemented)
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m OSImage
prepareEnv root distro local flush ifSourcesChanged include exclude components =
    do top <- askTop
       repo <- prepareLocalRepository (rootEnvPath local) Nothing
       copy <- copyLocalRepo (EnvPath {envRoot = root, envPath = "/work/localpool"}) repo
       ePutStrLn ("Preparing clean " ++ sliceName (sliceListName distro) ++ " build environment at " ++ rootPath root ++ ", osLocalRepoMaster: " ++ show repo)
       arch <- liftIO buildArchOfRoot
       let os = OS { osGlobalCacheDir = top
                   , osRoot = root
                   , osBaseDistro = sliceList distro
                   , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                   , osArch = arch
                   , osLocalMaster = repo
                   , osLocalCopy = copy
                   , osSourcePackages = []
                   , osBinaryPackages = [] }
       update os >>= recreate arch os >>= syncPool
    where
      update _ | flush = return (Left Flushed)
      update os = updateEnv os
      recreate :: MonadDeb m => Arch -> OSImage -> Either UpdateError OSImage -> m OSImage
      recreate _ _ (Right os) = return os
      recreate _arch _os (Left (Changed name path computed installed))
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       show (pretty computed) ++ "\ninstalled:\n" ++
                       show (pretty installed)
      recreate arch os (Left reason) =
          do ePutStrLn $ "Removing and recreating build environment at " ++ rootPath root ++ ": " ++ show reason
             -- ePutStrLn ("removeRecursiveSafely " ++ rootPath root)
             liftIO (removeRecursiveSafely (rootPath root))
             -- ePutStrLn ("createDirectoryIfMissing True " ++ show (distDir os))
             liftIO (createDirectoryIfMissing True (distDir os))
             -- ePutStrLn ("writeFile " ++ show (sourcesPath os) ++ " " ++ show (show . osBaseDistro $ os))
             liftIO (replaceFile (sourcesPath os) (show . pretty . osBaseDistro $ os))
             liftIO (try (getEnv "LANG") :: IO (Either SomeException String)) >>= \ localeName ->
                 buildEnv root distro arch (osLocalMaster os) (osLocalCopy os) include exclude components >>=
                     liftIO . (localeGen (either (const "en_US.UTF-8") id localeName)) >>=
                         liftIO . neuterEnv >>= syncPool

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

_pbuilderBuild :: MonadApt m =>
            FilePath
         -> EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
_pbuilderBuild cacheDir root distro arch repo copy _extraEssential _omitEssential _extra =
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
    do ePutStrLn ("Creating clean build environment (" ++ sliceName (sliceListName distro) ++ ")")
       ePutStrLn ("# " ++ cmd)
       liftIO (runProcess (shell cmd) L.empty) >>= liftIO . doOutput >>= foldOutputsL codefn outfn errfn exnfn (return ())
       ePutStrLn "done."
       let os = OS { osGlobalCacheDir = cacheDir
                   , osRoot = root
                   , osBaseDistro = sliceList distro
                   , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                   , osArch = arch
                   , osLocalMaster = repo
                   , osLocalCopy = copy
                   , osSourcePackages = []
                   , osBinaryPackages = [] }
       let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
       liftIO $ replaceFile sourcesPath' (show . pretty . aptSliceList $ os)
       updateEnv os >>= either (error . show) return
    where
      codefn _ ExitSuccess = return ()
      codefn _ failure = error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)
      outfn _ _ = return ()
      errfn _ _ = return ()
      exnfn _ _ = return ()
{-
      (output, result) <-
          do output <- liftIO (lazyCommandV cmd L.empty)
             ePutStrLn ("Creating clean build environment (" ++ sliceName (sliceListName distro) ++ ")")
             ePutStrLn ("# " ++ cmd)
             return . keepStderr . mergeToStderr $ output
      case result of
        -- It is fatal if we can't build the environment
        [Result ExitSuccess] ->
            do ePutStrLn "done."
               let os = OS { osGlobalCacheDir = cacheDir
                           , osRoot = root
                           , osBaseDistro = sliceList distro
                           , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                           , osArch = arch
                           , osLocalRepoMaster = repo
                           , osSourcePackages = []
                           , osBinaryPackages = [] }
               let sourcesPath = rootPath root ++ "/etc/apt/sources.list"
               -- Rewrite the sources.list with the local pool added.
               liftIO $ replaceFile sourcesPath (show . pretty . aptSliceList $ os)
               updateEnv os >>= either (error . show) return
        failure ->
            (ePutStr . L.unpack $ output) >>
            error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)
    where
-}
      cmd = intercalate " " $ [ "pbuilder"
                              , "--create"
                              , "--distribution", (sliceName . sliceListName $ distro)
                              , "--basetgz", cacheDir </> "pbuilderBase"
                              , "--buildplace", rootPath root
                              , "--preserve-buildplace"
                              ]
{-
      cmd = ("build-env --allow-missing-indexes --immediate-configure-false " ++
             " -o " ++ rootPath root ++ " -s " ++ cacheSourcesPath cacheDir (ReleaseName (sliceName (sliceListName distro))) ++
             " --with '" ++ intercalate " " extra ++ "'" ++
             " --with-essential '" ++ intercalate " " extraEssential ++ "'" ++
             " --omit-essential '" ++ intercalate " " omitEssential ++ "'")
 -}

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
buildEnv :: MonadDeb m =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
buildEnv root distro arch repo copy include exclude components =
    quieter (-1) $
    do
      top <- askTop
      ePutStr (unlines [ "Creating clean build environment (" ++ sliceName (sliceListName distro) ++ ")"
                       , "  root: " ++ show root
                       , "  baseDist: " ++ show baseDist
                       , "  mirror: " ++ show mirror ])
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      runProcess (shell cmd) L.empty >>= foldOutputsL codefn outfn errfn exnfn (return ())
      ePutStrLn "done."
      let os = OS { osGlobalCacheDir = top
                  , osRoot = root
                  , osBaseDistro = sliceList distro
                  , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                  , osArch = arch
                  , osLocalMaster = repo
                  , osLocalCopy = copy
                  , osSourcePackages = []
                  , osBinaryPackages = [] }
      let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
      liftIO $ replaceFile sourcesPath' (show . pretty . aptSliceList $ os)
      updateEnv os >>= either (error . show) return
    where
      codefn _ ExitSuccess = return ()
      codefn _ failure = error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)
      outfn _ _ = return ()
      errfn _ _ = return ()
      exnfn _ _ = return ()

      woot = rootPath root
      wootNew = woot ++ ".new"
      baseDist = either id (relName . fst) . sourceDist . sliceSource . head . slices . sliceList $ distro
      mirror = uriToString' . sourceUri . sliceSource . head . slices . sliceList $ distro
      cmd = intercalate " && "
              ["set -x",
               "rm -rf " ++ wootNew,
               ("debootstrap " ++
                (if include /= [] then "--include=" ++ intercalate "," include ++ " " else "") ++
                (if exclude /= [] then "--exclude=" ++ intercalate "," exclude ++ " " else "") ++
                "--variant=buildd " ++
                "--components=" ++ intercalate "," components ++ " " ++
                baseDist ++ " " ++
                wootNew ++ " " ++
                mirror),
               "cat " ++ wootNew ++ "/etc/apt/sources.list | sed -e 's/^deb /deb-src /' >>" ++ wootNew ++ "/etc/apt/sources.list",
               "mkdir -p " ++ woot,
               "rm -rf " ++ woot,
               "mv " ++ wootNew ++ " " ++ woot]

-- |Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateEnv :: MonadApt m => OSImage -> m (Either UpdateError OSImage)
updateEnv os =
    do liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc") >> readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
       verified <- verifySources
       case verified of
         Left x -> return $ Left x
         Right _ ->
             do liftIO $ prepareDevs (rootPath root)
                os' <- syncPool os
                _ <- liftIO $ updateLists os'
                _ <- liftIO $ sshCopy (rootPath root)
                source <- getSourcePackages os'
                binary <- getBinaryPackages os'
                return . Right $ os' {osSourcePackages = source, osBinaryPackages = binary}
    where
      verifySources :: MonadApt m => m (Either UpdateError OSImage)
      verifySources =
          do let computed = remoteOnly (aptSliceList os)
                 sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (osReleaseName os) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       return $ Left $ Changed (osReleaseName os) sourcesPath' computed installed'
               _ -> return $ Right os
      root = osRoot os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"

localeGen :: String -> OSImage -> IO OSImage
localeGen locale os =
    do
      ePutStr ("Generating locale " ++  locale ++ " (" ++ stripDist (rootPath root) ++ ")...")
      result <- try $ useEnv (rootPath root) forceList (runProcess (shell cmd) L.empty) >>= return . collectOutputs
      either (\ (e :: SomeException) -> error $ "Failed to generate locale " ++ rootPath root ++ ": " ++ show e)
             (\ _ -> return os)
             result
    where
      root = osRoot os
      cmd = "locale-gen " ++ locale

-----------------------------------

-- |Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the environment
-- where apt can see and install the packages.
syncPool :: {- MonadRepoCache k r -} MonadApt m => OSImage -> m OSImage
syncPool os =
    do repo' <- copyLocalRepo (EnvPath {envRoot = osRoot os, envPath = "/work/localpool"}) (osLocalMaster os)
       return (os {osLocalCopy = repo'})
{-
    where
      rsync' repo =
          do result <- rsync [] (outsidePath (repoRoot repo)) (rootPath root ++ "/work/localpool")
             case result of
               ExitFailure n -> return (Left $ "*** FAILURE syncing local pool from " ++ outsidePath (repoRoot repo) ++ ": " ++ show n)
               _ -> return (Right ())
      root = osRoot os
-}

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)
