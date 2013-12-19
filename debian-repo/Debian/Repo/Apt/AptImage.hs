{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.Apt.AptImage
    ( prepareAptEnv
    , prepareOSEnv
    , prepareSource
    , updateOSEnv
    , syncLocalPool
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Exception as E (catch)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL)
import Data.List (intercalate, sortBy)
import Data.List as List (map, partition)
import Data.Map as Map (insert, lookup)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T (Text, unpack)
import Debian.Arch (Arch, Arch(..), prettyArch)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Control (ControlFunctions(stripWS), formatParagraph)
import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(lookupP), ControlFunctions(parseControlFromHandle), Field, Field'(Field), fieldValue, Paragraph)
import Debian.Relation (SrcPkgName(..), BinPkgName(BinPkgName))
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (ReleaseName(..), releaseName', sectionName')
import Debian.Repo.Apt (aptImageMap, binaryPackageMap, MonadApt(getApt, putApt), foldRepository, sourcePackageMap, MonadDeb)
import Debian.Repo.Apt.Slice (updateCacheSources, verifySourcesList)
import Debian.Repo.AptImage (OSCache(..), AptCache(..), AptImage(..), localeGen, neuterEnv, OSImage(..), updateLists, buildArchOfRoot, cacheRootDir, distDir, SourcesChangedAction, SourcesChangedAction(SourcesChangedError), sourcesPath, aptGetUpdate, aptGetSource, aptGetInstall)
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(..))
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID, PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage, BinaryPackage(..), PackageIndex(..), PackageIndex(packageIndexArch, packageIndexComponent), packageIndexPath, SourceControl(..), SourceFileSpec(SourceFileSpec), SourcePackage(..), SourcePackage(sourcePackageID))
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey, repoKeyURI)
import Debian.Repo.SSH (sshCopy)
import Debian.Repo.Slice (binarySlices, NamedSliceList(sliceList, sliceListName), Slice(..), Slice(sliceSource), SliceList(slices), sourceSlices)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), entry, findDebianBuildTrees, SourceTree(dir'))
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Repo.Top (MonadTop(askTop))
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SliceName(sliceName), SourceType(..))
import Debian.URI (URI(uriScheme), uriToString')
import Debian.Version (DebianVersion, parseDebianVersion)
import Extra.Files (replaceFile, writeFileIfMissing)
import Network.URI (escapeURIString, URI(uriAuthority, uriPath), URIAuth(uriPort, uriRegName, uriUserInfo))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName, takeDirectory)
import qualified System.IO as IO (hClose, IOMode(ReadMode), openBinaryFile)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)
import System.Process (shell)
import System.Process.Progress (doOutput, ePutStr, ePutStrLn, foldOutputsL, oneResult, qPutStr, qPutStrLn, quieter, readProcessChunks, runProcess)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptEnv :: MonadApt m =>
                 FilePath		-- Put environment in a subdirectory of this
              -> SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptEnv cacheDir sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (sliceName (sliceListName sources))) >> quieter 2 x) $
    getApt >>= return . Map.lookup (sliceListName sources) . getL aptImageMap >>=
    maybe (prepareAptEnv' cacheDir sourcesChangedAction sources) return

prepareAptEnv' :: MonadApt m => FilePath -> SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptEnv' cacheDir sourcesChangedAction sources =
    do let root = rootPath (cacheRootDir cacheDir (ReleaseName (sliceName (sliceListName sources))))
       --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/cache/apt/archives/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/dpkg")
       liftIO $ createDirectoryIfMissing True (root ++ "/etc/apt")
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/status") ""
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/diversions") ""
       -- We need to create the local pool before updating so the
       -- sources.list will be valid.
       let sourceListText = show (pretty (sliceList sources))
       -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
       liftIO $ replaceFile (root ++ "/etc/apt/sources.list") sourceListText
       arch <- liftIO $ buildArchOfRoot
       let os = AptImage { aptGlobalCacheDir = cacheDir
                         , aptImageRoot = EnvRoot root
                         , aptImageArch = arch
                         , aptImageReleaseName = ReleaseName . sliceName . sliceListName $ sources
                         , aptImageSliceList = sliceList sources
                         , aptImageSourcePackages = []
                         , aptImageBinaryPackages = [] }
       os' <- updateCacheSources sourcesChangedAction os >>= updateAptEnv
       getApt >>= putApt . modL aptImageMap (Map.insert (sliceListName sources) os')
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: MonadApt m => AptImage -> m AptImage
updateAptEnv os =
    do liftIO $ aptGetUpdate os
       sourcePackages <- getSourcePackages os >>= return . sortBy cmp
       binaryPackages <- getBinaryPackages os
       return $ os { aptImageSourcePackages = sourcePackages
                   , aptImageBinaryPackages = binaryPackages }
    where
      -- Flip args to get newest version first
      cmp = flip (compare `on` (packageVersion . sourcePackageID))
{-
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

    putStrLn ("> " ++ cmd) >> system cmd >>= \ code ->
    case code of
      ExitSuccess -> return ()
      ExitFailure n -> error $ cmd ++ " -> ExitFailure " ++ show n
-}

getSourcePackages :: MonadApt m => AptImage -> m [SourcePackage]
getSourcePackages os =
    do qPutStrLn "AptImage.getSourcePackages"
       indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptImageSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages :: MonadApt m => AptImage -> m [BinaryPackage]
getBinaryPackages os =
    do qPutStrLn "AptImage.getBinaryPackages"
       indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptImageSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

getSourcePackages' :: MonadApt m => OSImage -> m [SourcePackage]
getSourcePackages' os =
    do indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages' :: MonadApt m => OSImage -> m [BinaryPackage]
getBinaryPackages' os =
    do indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: forall m a. (MonadApt m, AptCache a) => a -> Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes cache slice =
    foldRepository f f (sliceRepoKey slice)
    where
      f :: Repo r => r -> m [(RepoKey, Release, PackageIndex)]
      f repo =
          case (sourceDist (sliceSource slice)) of
            Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
            Right (release, sections) -> return $ map (makeIndex repo release) sections
      makeIndex repo release section =
          (repoKey repo,
           findReleaseInfo repo release,
           PackageIndex { packageIndexComponent = section
                        , packageIndexArch = case (sourceType (sliceSource slice)) of
                                               DebSrc -> Source
                                               Deb -> aptArch cache })
      findReleaseInfo repo release =
          case filter ((==) release . releaseName) (repoReleaseInfo repo) of
            [x] -> x
            [] -> error $ ("sliceIndexes: Invalid release name: " ++ releaseName' release ++
                           "\n  You may need to remove ~/.autobuilder/repoCache." ++
                           "\n  Available: " ++ (show . map releaseName . repoReleaseInfo $ repo)) ++
                           "\n repoKey: " ++ show (repoKey repo) ++
                           "\n repoReleaseInfo: " ++ show (repoReleaseInfo repo) ++
                           "\n slice: " ++ show slice
            xs -> error $ "Internal error 5 - multiple releases named " ++ releaseName' release ++ "\n" ++ show xs

-- |Retrieve a source package via apt-get.
prepareSource :: (AptCache t)
             => FilePath			-- Where to put the source
             -> t				-- Where to apt-get from
             -> SrcPkgName			-- The name of the package
             -> Maybe DebianVersion		-- The desired version, if Nothing get newest
             -> IO DebianBuildTree		-- The resulting source tree
prepareSource dir os package version =
    do liftIO $ createDirectoryIfMissing True dir
       ready <- findDebianBuildTrees dir
       let newest = listToMaybe . map (packageVersion . sourcePackageID) . filter ((== package) . packageName . sourcePackageID) . aptSourcePackages $ os
       let version' = maybe newest Just version
       case (version', ready) of
         (Nothing, _) ->
             fail $ "No available versions of " ++ unSrcPkgName package ++ " in " ++ rootPath (rootDir os)
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do aptGetSource os dir [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed in " ++ dir ++ " (1): trees=" ++ show (map (dir' . tree' . debTree') trees)
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                qPutStr $ "Retrieving APT source for " ++ unSrcPkgName package
                aptGetSource os dir [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed (2): trees=" ++ show (map (dir' . tree' . debTree') trees)

{-
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
-}

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- |Create or update an OS image in which packages can be built.
prepareOSEnv :: MonadDeb m =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call (unimplemented)
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ More packages to install, but these may not be available
                                        -- immediately - e.g seereason-keyring.  Ignore exceptions.
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m OSImage
prepareOSEnv root distro repo flush ifSourcesChanged include optional exclude components =
    do top <- askTop
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
       -- update os >>= recreate arch os >>= doInclude >>= doLocales >>= syncLocalPool
       os' <- update os
       os'' <- recreate arch os os'
       doInclude os''
       doLocales os''
       syncLocalPool os''
    where
      update _ | flush = return (Left Flushed)
      update os = updateOSEnv os
      recreate :: MonadDeb m => Arch -> OSImage -> Either UpdateError OSImage -> m OSImage
      recreate _ _ (Right os) = return os
      recreate _arch _os (Left (Changed name path computed installed))
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       show (pretty computed) ++ "\ninstalled:\n" ++
                       show (pretty installed)
      recreate arch os (Left reason) =
          do liftIO $ do ePutStrLn $ "Removing and recreating build environment at " ++ rootPath root ++ ": " ++ show reason
                         -- ePutStrLn ("removeRecursiveSafely " ++ rootPath root)
                         removeRecursiveSafely (rootPath root)
                         -- ePutStrLn ("createDirectoryIfMissing True " ++ show (distDir os))
                         createDirectoryIfMissing True (distDir os)
                         -- ePutStrLn ("writeFile " ++ show (sourcesPath os) ++ " " ++ show (show . osBaseDistro $ os))
                         replaceFile (sourcesPath os) (show . pretty . osBaseDistro $ os)
             os' <- buildEnv root distro arch (osLocalMaster os) (osLocalCopy os) include exclude components
             liftIO $ do doLocales os'
                         neuterEnv os'
             syncLocalPool os'
      doInclude os = liftIO $
          do aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) include)
             aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) optional) `catchIOError` (\ e -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales os =
          do localeName <- liftIO (try (getEnv "LANG") :: IO (Either SomeException String))
             liftIO $ localeGen (either (const "en_US.UTF-8") id localeName) os

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
       updateOSEnv os >>= either (error . show) return
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
      updateOSEnv os >>= either (error . show) return
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
updateOSEnv :: MonadApt m => OSImage -> m (Either UpdateError OSImage)
updateOSEnv os =
    do liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc") >> readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
       verified <- verifySources
       case verified of
         Left x -> return $ Left x
         Right _ ->
             do liftIO $ prepareDevs (rootPath root)
                os' <- syncLocalPool os
                _ <- liftIO $ updateLists os'
                _ <- liftIO $ sshCopy (rootPath root)
                source' <- getSourcePackages' os'
                binary <- getBinaryPackages' os'
                return . Right $ os' {osSourcePackages = source', osBinaryPackages = binary}
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

-- |Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the environment
-- where apt can see and install the packages.
syncLocalPool :: {- MonadRepoCache k r -} MonadApt m => OSImage -> m OSImage
syncLocalPool os =
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

{-
stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)
-}

-- FIXME: assuming the index is part of the cache
sourcePackagesOfIndex' :: (AptCache a, MonadApt m) => a -> RepoKey -> Release -> PackageIndex -> m [SourcePackage]
sourcePackagesOfIndex' cache repo release index =
    do -- state <- getApt
       -- let cached = lookupSourcePackages path state <$> getApt
       cached <- (Map.lookup path . getL sourcePackageMap) <$> getApt
       status <- liftIO $ getFileStatus path `E.catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory (rootPath (rootDir cache)) ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toSourcePackage index) paragraphs
                 getApt >>= putApt . modL sourcePackageMap (Map.insert path (status, packages))
                 -- sourcePackageMap %= Map.insert path (status, packages)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . T.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case (parseSourcesFileList files, parseSourceParagraph package) of
            (Right files', Right para) ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID (T.unpack name) version
                , sourceParagraph = package
                , sourceControl = para
                , sourceDirectory = T.unpack directory
                , sourcePackageFiles = files' }
            (Left messages, _) -> error $ "Invalid file list: " ++ show messages
            (_, Left messages) -> error $ "Error in source paragraph\n package=" ++ show package ++ "\n  index=" ++ show index ++ "\n  messages:\n   " ++ intercalate "\n   " messages
      x -> error $ "Missing info in source package control information in " ++ show index ++ " -> " ++ show x ++ " :\n" ++ T.unpack (formatParagraph package)
    where
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: T.Text -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . List.map parseSourcesFiles . lines . T.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . List.map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . List.map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Either [String] SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    case (B.fieldValue "Package" p,
          B.fieldValue "Maintainer" p) of
      (Just source', Just maintainer') ->
          -- The optional fields can be parsed as pure values
          Right (SourceControl
                  { source = source'
                  , maintainer = maintainer'
                  , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
                  , packageSection = fmap stripWS $ B.fieldValue "Section" p
                  , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
                  , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
                  , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
                  , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
                  , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
                  , standardsVersion = fmap stripWS $ B.fieldValue "Standards-Version" p
                  , homepage = fmap stripWS $ B.fieldValue "Homepage" p })
      _x -> Left ["parseSourceParagraph - One or more required fields (Package, Maintainer, Standards-Version) missing: " ++ show p]

-- FIXME: assuming the index is part of the cache 
binaryPackagesOfIndex' :: (MonadApt m, AptCache a) => a -> RepoKey -> Release -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfIndex' cache repo release index =
    do cached <- (Map.lookup path . getL binaryPackageMap) <$> getApt
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toBinaryPackage release index) paragraphs
                 getApt >>= putApt . modL binaryPackageMap (Map.insert path (status, packages))
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

toBinaryPackage :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage 
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)			-- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")	-- Debugging output
       return paragraphs

indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> FilePath
indexPrefix repo release index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distro = releaseName $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user'') Nothing (Just host) port' path' =
          user'' ++ host ++ port' ++ escape path'
      prefix "http:" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix "ftp:" _ _ (Just host) _ path' =
          host ++ escape path'
      prefix "file:" Nothing Nothing Nothing "" path' =
          escape path'
      prefix "ssh:" (Just user'') Nothing (Just host) port' path' =
          user'' ++ host ++ port' ++ escape path'
      prefix "ssh" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s', []) -> [s']
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b
