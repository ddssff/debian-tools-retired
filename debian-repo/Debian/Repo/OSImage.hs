{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Debian.Repo.OSImage 
    ( OSImage(..)
    , prepareEnv
    , updateEnv
    , syncPool
    , updateLists
    , chrootEnv
    , syncEnv
    , neuterEnv
    , restoreEnv
    , removeEnv
    , buildEssential
    , withProc
    ) where

import Control.Exception (evaluate, bracket)
import Control.Exception ( SomeException, try )
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List ( intercalate )
import Data.Time ( NominalDiffTime )
import Debian.Release ( ReleaseName(..), Arch, archName, parseReleaseName, parseSection' )
import Debian.Sources ( SourceType(..), SliceName(sliceName), DebSource(DebSource, sourceDist, sourceUri) )
import Debian.Repo.Cache ( SourcesChangedAction(SourcesChangedError), distDir, sourcesPath, sliceIndexes, buildArchOfRoot )
import Debian.Repo.Monads.Apt (MonadApt)
import Debian.Repo.Monads.Top (MonadTop(askTop))
import Debian.Repo.Package ( sourcePackagesOfIndex', binaryPackagesOfIndex' )
import Debian.Relation ( ParseRelations(..), Relations )
import Debian.Repo.Slice ( sourceSlices, binarySlices, verifySourcesList )
import Debian.Repo.SourcesList ( parseSourcesList )
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types ( AptBuildCache(..), AptCache(..), SourcePackage, BinaryPackage, NamedSliceList(sliceList, sliceListName), SliceList(..), Slice(..),
                           Repo(repoURI), LocalRepository(repoRoot), Repository(LocalRepo), EnvPath(EnvPath, envRoot), EnvRoot(rootPath), outsidePath )
import Debian.URI ( uriToString', URI(uriScheme) )
import Extra.Files ( replaceFile )
import "Extra" Extra.List ( isSublistOf )
import Extra.Misc ( sameInode, sameMd5sum )
import Extra.SSH ( sshCopy )
import System.FilePath ( (</>), splitFileName )
import System.Directory ( createDirectoryIfMissing, doesFileExist, removeFile, renameFile )
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Posix.Files ( createLink )
import System.Process (readProcess, shell, proc)
import System.Process.Progress (ePutStr, ePutStrLn, qPutStr, qPutStrLn, readProcessChunks, doOutput,
                                runProcess, runProcessF, timeTask, collectOutputs, oneResult, quieter, foldOutputsL, keepResult)
import System.Unix.Chroot ( useEnv )
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount ( umountBelow )
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex ( matchRegex, mkRegex )

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { osGlobalCacheDir :: FilePath
         , osRoot :: EnvRoot
         , osBaseDistro :: SliceList
         , osReleaseName :: ReleaseName
         , osArch :: Arch
	 -- | The associated local repository, where packages we
         -- build inside this image are first uploaded to.
         , osLocalRepoMaster :: Maybe LocalRepository
         -- |A copy of osLocalRepo which is inside the changeroot
         --, osLocalRepoCopy :: Maybe LocalRepo
         -- | Update and return a copy of the local repository
         -- which is inside the changeroot.
         , osSourcePackages :: [SourcePackage]
         , osBinaryPackages :: [BinaryPackage]
         }

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               rootPath (osRoot os),
                               relName (osReleaseName os),
                               archName (osArch os),
                               show (osLocalRepoMaster os)]

instance Ord OSImage where
    compare a b = case compare (osRoot a) (osRoot b) of
                    EQ -> case compare (osBaseDistro a) (osBaseDistro b) of
                            EQ -> compare (osArch a) (osArch b)
                            x -> x
                    x -> x

instance Eq OSImage where
    a == b = compare a b == EQ

instance AptCache OSImage where
    globalCacheDir = osGlobalCacheDir
    rootDir = osRoot
    aptArch = osArch
    -- aptSliceList = osFullDistro
    aptBaseSliceList = osBaseDistro
    aptSourcePackages = osSourcePackages
    aptBinaryPackages = osBinaryPackages
    aptReleaseName = osReleaseName

instance AptBuildCache OSImage where
    aptSliceList = osFullDistro

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os = SliceList { slices = slices (osBaseDistro os) ++ slices (localSources os) }

localSources :: OSImage -> SliceList
localSources os =
    case osLocalRepoMaster os of
      Nothing -> SliceList { slices = [] }
      Just repo ->
          let repo' = repoCD (EnvPath (envRoot (repoRoot repo)) "/work/localpool") repo in
          let name = relName (osReleaseName os) in
          let src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
              bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"])) in
          SliceList { slices = [Slice { sliceRepo = LocalRepo repo', sliceSource = src }, 
                                Slice { sliceRepo = LocalRepo repo', sliceSource = bin }] }

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
repoCD :: EnvPath -> LocalRepository -> LocalRepository
repoCD path repo = repo { repoRoot = path }

getSourcePackages :: MonadApt m => OSImage -> m [SourcePackage]
getSourcePackages os =
    mapM (sourcePackagesOfIndex' os) indexes >>= return . concat
    where indexes = concat . map (sliceIndexes os) . slices . sourceSlices . aptSliceList $ os

getBinaryPackages :: MonadApt m => OSImage -> m [BinaryPackage]
getBinaryPackages os =
    mapM (binaryPackagesOfIndex' os) indexes >>= return . concat
    where indexes = concat . map (sliceIndexes os) . slices . binarySlices . aptSliceList $ os

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- |Create or update an OS image in which packages can be built.
prepareEnv :: (MonadApt m, MonadTop m) =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list
           -> Maybe LocalRepository	-- ^ The associated local repository, where newly
					-- built packages are stored.  This repository is
					-- periodically copied into the build environment
					-- so apt can access the packages in it.
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call (unimplemented)
           -> [String]			-- ^ Extra packages to include
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m OSImage
prepareEnv root distro repo flush ifSourcesChanged include exclude components =
    do top <- askTop
       ePutStrLn ("Preparing clean " ++ sliceName (sliceListName distro) ++ " build environment at " ++ rootPath root)
       arch <- liftIO buildArchOfRoot
       let os = OS { osGlobalCacheDir = top
                   , osRoot = root
                   , osBaseDistro = sliceList distro
                   , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                   , osArch = arch
                   , osLocalRepoMaster = repo
                   , osSourcePackages = []
                   , osBinaryPackages = [] }
       update os >>= recreate arch os >>= liftIO . syncPool
    where
      update _ | flush = return (Left Flushed)
      update os = updateEnv os
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
                 buildEnv root distro arch repo include exclude components >>=
                     liftIO . (localeGen (either (const "en_US.UTF-8") id localeName)) >>=
                         liftIO . neuterEnv >>= liftIO . syncPool

-- |Prepare a minimal \/dev directory
{-# WARNING prepareDevs "This function should check all the result codes" #-}
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev devices
  where
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

pbuilderBuild :: MonadApt m =>
            FilePath
         -> EnvRoot
         -> NamedSliceList
         -> Arch
         -> Maybe LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
pbuilderBuild cacheDir root distro arch repo extraEssential omitEssential extra =
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
                   , osLocalRepoMaster = repo
                   , osSourcePackages = []
                   , osBinaryPackages = [] }
       let sourcesPath = rootPath root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
       liftIO $ replaceFile sourcesPath (show . pretty . aptSliceList $ os)
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
buildEnv :: (MonadApt m, MonadTop m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> Maybe LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
buildEnv root distro arch repo include exclude components =
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
                  , osLocalRepoMaster = repo
                  , osSourcePackages = []
                  , osBinaryPackages = [] }
      let sourcesPath = rootPath root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
      liftIO $ replaceFile sourcesPath (show . pretty . aptSliceList $ os)
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
       verified <- verifySources os
       case verified of
         Left x -> return $ Left x
         Right _ ->
             do liftIO $ prepareDevs (rootPath root)
                os' <- liftIO $ syncPool os
                liftIO $ updateLists os'
                liftIO $ sshCopy (rootPath root)
                source <- getSourcePackages os'
                binary <- getBinaryPackages os'
                return . Right $ os' {osSourcePackages = source, osBinaryPackages = binary}
    where
      verifySources :: MonadApt m => OSImage -> m (Either UpdateError OSImage)
      verifySources os =
          do let computed = remoteOnly (aptSliceList os)
                 sourcesPath = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath)
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (osReleaseName os) sourcesPath
               Just installed
                   | installed /= computed ->
                       return $ Left $ Changed (osReleaseName os) sourcesPath computed installed
               _ -> return $ Right os
      root = osRoot os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r x = (uriScheme . sourceUri . sliceSource $ x) == "file:"

chrootEnv :: OSImage -> EnvRoot -> OSImage
chrootEnv os dst = os {osRoot=dst}

-- Sync the environment from the clean copy.  All this does besides
-- performing the proper rsync command is to make sure the destination
-- directory exists, otherwise rsync will fail.  Not sure why the 'work'
-- subdir is appended.  There must have been a reason at one point.
syncEnv :: OSImage -> OSImage -> IO OSImage
syncEnv src dst =
    mkdir >> umount >> rsync ["--exclude=/work/build/*"] (rootPath (osRoot src)) (rootPath (osRoot dst)) >> return dst
    where
      mkdir = createDirectoryIfMissing True (rootPath (osRoot dst) ++ "/work")
      umount =
          do qPutStrLn "syncEnv: umount"
             srcResult <- umountBelow False (rootPath (osRoot src))
             dstResult <- umountBelow False (rootPath (osRoot dst))
             case filter (\ (_, (code, _, _)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return ()
               failed -> fail $ "umount failure(s): " ++ show failed

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


-- |To "neuter" an executable is to replace it with a hard link to
-- \/bin\/true in such a way that the operation can be reversed.  This
-- is done in order to make it safe to install files into it when it
-- isn't "live".  If this operation fails it is assumed that the
-- image is damaged, so it is removed.
neuterEnv :: OSImage -> IO OSImage
neuterEnv os =
    do
      ePutStr ("Neutering OS image (" ++ stripDist (rootPath root) ++ ")...")
      result <- try $ mapM_ (neuterFile os) neuterFiles
      either (\ (e :: SomeException) -> error $ "Failed to neuter environment " ++ rootPath root ++ ": " ++ show e)
             (\ _ -> return os)
             result
    where
      root = osRoot os

neuterFiles :: [(FilePath, Bool)]
neuterFiles = [("/sbin/start-stop-daemon", True),
	       ("/usr/sbin/invoke-rc.d", True),
	       ("/sbin/init",False),
	       ("/usr/sbin/policy-rc.d", False)]

-- neuter_file from build-env.ml
neuterFile :: OSImage -> (FilePath, Bool) -> IO ()
neuterFile os (file, mustExist) =
    do
      -- putStrBl ("Neutering file " ++ file)
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          neuterExistantFile else
          if mustExist then
              error ("Can't neuter nonexistant file: " ++ outsidePath fullPath) else
              return () -- putStrBl "File doesn't exist, nothing to do"

    where
      neuterExistantFile =
          do
            sameFile <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            if sameFile then
                return () else -- putStrBl "File already neutered"
                neuterUnneuteredFile
      neuterUnneuteredFile =
          do
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            if hasReal then
                neuterFileWithRealVersion else
                neuterFileWithoutRealVersion
            createLink (outsidePath binTrue) (outsidePath fullPath)
      neuterFileWithRealVersion =
          do
            sameCksum <- sameMd5sum (outsidePath fullPath) (outsidePath fullPath ++ ".real")
            if sameCksum then
                removeFile (outsidePath fullPath) else
                error (file ++ " and " ++ file ++ ".real differ (in " ++ rootPath root ++ ")")

      neuterFileWithoutRealVersion = renameFile (outsidePath fullPath) (outsidePath fullPath ++ ".real")

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-- |Reverse the neuterEnv operation.
restoreEnv :: OSImage -> IO OSImage
restoreEnv os =
    do
      qPutStr "De-neutering OS image..."
      result <- try $ mapM_ (restoreFile os) neuterFiles
      either (\ (e :: SomeException) -> error $ "damaged environment " ++ rootPath root ++ ": " ++ show e ++ "\n  please remove it.")
                 (\ _ -> return os) result
    where
      root = osRoot os

-- check_and_restore from build-env.ml
restoreFile :: OSImage -> (FilePath, Bool) -> IO ()
restoreFile os (file, mustExist) =
    do
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          restoreExistantFile else
          if mustExist then
              error ("Can't restore nonexistant file: " ++ outsidePath fullPath) else
              return ()
    where
      restoreExistantFile =
          do
            isTrue <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            case (isTrue, hasReal) of
              (True, True) ->
                  do
                    removeFile (outsidePath fullPath)
                    renameFile (outsidePath fullPath ++ ".real") (outsidePath fullPath)
              (False, _) -> error "Can't restore file not linked to /bin/true"
              (_, False) -> error "Can't restore file with no .real version"

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-----------------------------------

-- |Build the dependency relations for the build essential packages.
-- For this to work the build-essential package must be installed in
-- the OSImage.
buildEssential :: OSImage -> IO Relations
buildEssential os =
    (\ x -> qPutStrLn "Computing build essentials" >> quieter 2 x) $
    do
      essential <-
          readFile (rootPath root ++ "/usr/share/build-essential/essential-packages-list") >>=
          return . lines >>= return . dropWhile (/= "") >>= return . tail >>= return . filter (/= "sysvinit") >>=
          return . parseRelations . (intercalate ", ") >>=
          return . (either (error "parse error in /usr/share/build-essential/essential-packages-list") id)
      let re = mkRegex "^[^ \t]"
      relationText <-
          readFile (rootPath root ++ "/usr/share/build-essential/list") >>=
          return . lines >>=
          return . dropWhile (/= "BEGIN LIST OF PACKAGES") >>= return . tail >>=
          return . takeWhile (/= "END LIST OF PACKAGES") >>=
          return . filter ((/= Nothing) . (matchRegex re))
      -- ePut ("buildEssentialText: " ++ intercalate ", " relationText)
      let buildEssential = parseRelations (intercalate ", " relationText)
      let buildEssential' = either (\ l -> error ("parse error in /usr/share/build-essential/list:\n" ++ show l)) id buildEssential
      return (essential ++ buildEssential')
    where
      root = osRoot os

-- |Remove an image.  The removeRecursiveSafely function is used to
-- ensure that any file systems mounted inside the image are unmounted
-- instead of destroyed.
removeEnv :: OSImage -> IO ()
removeEnv os =
    do
      ePutStr "Removing build environment..."
      removeRecursiveSafely (rootPath root)
      ePutStrLn "done."
    where
      root = osRoot os

-- |Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the environment
-- where apt can see and install the packages.
syncPool :: OSImage -> IO OSImage
syncPool os =
    case osLocalRepoMaster os of
      Nothing -> return os
      Just repo ->
          qPutStrLn ("Syncing local pool from " ++ outsidePath (repoRoot repo) ++ " -> " ++ rootPath root) >>
          try (createDirectoryIfMissing True (rootPath root ++ "/work")) >>=
          either (\ (e :: SomeException) -> return . Left . show $ e) (const (rsync' repo)) >>=
          -- either (return . Left) (const (updateLists os)) >>=
          either (error . show) (const (return os))
    where
      rsync' repo =
          do result <- rsync [] (outsidePath (repoRoot repo)) (rootPath root ++ "/work/localpool")
             case result of
               ExitFailure n -> return (Left $ "*** FAILURE syncing local pool from " ++ outsidePath (repoRoot repo) ++ ": " ++ show n)
               _ -> return (Right ())
      root = osRoot os

updateLists :: OSImage -> IO NominalDiffTime
updateLists os =
    withProc os $ quieter 1 $ do
      qPutStrLn ("Updating OSImage " ++ root)
      out <- useEnv root forceList (runProcess update L.empty)
      case keepResult out of
        [ExitFailure _] ->
            do useEnv root forceList (runProcessF configure L.empty)
               useEnv root forceList (runProcessF update L.empty)
        _ -> return []
      (_, elapsed) <- timeTask (useEnv root forceList (runProcessF upgrade L.empty))
      return elapsed
    where
       root = rootPath (osRoot os)
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)

withProc :: OSImage -> IO a -> IO a
withProc buildOS task =
    bracket (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/proc", dir] "")
            (\ _ -> readProcess "umount" [dir] "")
            (\ _ -> task)
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"
