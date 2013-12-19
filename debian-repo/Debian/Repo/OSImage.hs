{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.OSImage
    ( OSImage
    , osBaseDistro
    , osFullDistro
    , osLocalMaster
    , osLocalCopy
    , osSourcePackages
    , osBinaryPackages
    , osRoot
    , osArch

    , chrootEnv
    , syncEnv
    , localeGen
    , neuterEnv
    , restoreEnv
    , buildEssential
    , removeEnv
    , updateLists
    , withProc
    , aptGetInstall

    , prepareOSEnv'
    , _pbuilderBuild'
    , buildEnv'
    , syncLocalPool
    ) where

import Control.DeepSeq (force, NFData)
import Control.Exception (bracket, evaluate, SomeException, try)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.Data (Data)
import Data.Lens.Lazy (getL, setL)
import Data.Lens.Template (makeLenses)
import Data.List (intercalate, sortBy)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (BinPkgName, ParseRelations(..), PkgName(..), Relations, SrcPkgName(..))
import Debian.Release (parseReleaseName, parseSection', ReleaseName(ReleaseName, relName))
import Debian.Repo.AptCache (AptCache(..))
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath), envRoot, EnvRoot(rootPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList, SliceList(..))
import Debian.Repo.Sync (rsync)
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SourceType(..), SourceType(..))
import Debian.URI (uriToString')
import Debian.Version (DebianVersion, prettyDebianVersion)
import Extra.Files (replaceFile, writeFileIfMissing)
import "Extra" Extra.List (isSublistOf)
import Extra.Misc (sameInode, sameMd5sum)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Env (setEnv)
import System.Posix.Files (createLink)
import System.Process (CreateProcess(cwd), proc, readProcess, readProcessWithExitCode, shell)
import System.Process.Progress (collectOutputs, doOutput, ePutStr, ePutStrLn, foldOutputsL, keepResult, qPutStr, qPutStrLn, quieter, runProcess, runProcessF, timeTask)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (matchRegex, mkRegex)

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { _osGlobalCacheDir :: FilePath
         , _osRoot :: EnvRoot
         , _osBaseDistro :: NamedSliceList
         , _osArch :: Arch
	 -- | The associated local repository, where packages we
         -- build inside this image are first uploaded to.
         , _osLocalMaster :: LocalRepository
	 -- | A copy of osLocalMaster located inside the os root
	 -- environment.
         , _osLocalCopy :: LocalRepository
         -- | A copy of osLocalMaster which is inside the changeroot
         , _osSourcePackages :: [SourcePackage]
         , _osBinaryPackages :: [BinaryPackage]
         }

$(makeLenses [''OSImage])

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

-- | A directory which will hold all the cached files for this
-- NamedSliceList.
cacheDistDir :: FilePath -> ReleaseName -> FilePath
cacheDistDir cacheDir release = cacheDir ++ "/dists/" ++ relName release

cacheRootDir :: FilePath -> ReleaseName -> EnvRoot
cacheRootDir cacheDir release = EnvRoot (cacheDistDir cacheDir release ++ "/aptEnv")

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               rootPath (getL osRoot os),
                               relName (sliceListName (getL osBaseDistro os)),
                               show (getL osArch os),
                               show (getL osLocalCopy os)]

instance Ord OSImage where
    compare a b = case compare (getL osRoot a) (getL osRoot b) of
                    EQ -> case compare (getL osBaseDistro a) (getL osBaseDistro b) of
                            EQ -> compare (getL osArch a) (getL osArch b)
                            x -> x
                    x -> x

instance Eq OSImage where
    a == b = compare a b == EQ

instance AptCache OSImage where
    globalCacheDir = getL osGlobalCacheDir
    rootDir = getL osRoot
    aptArch = getL osArch
    -- aptSliceList = getL osFullDistro
    aptBaseSources = getL osBaseDistro
    aptSourcePackages = getL osSourcePackages
    aptBinaryPackages = getL osBinaryPackages

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os =
    SliceList { slices = slices (sliceList (getL osBaseDistro os)) ++ slices localSources }
    where
      localSources :: SliceList
      localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey repo', sliceSource = src},
                                          Slice {sliceRepoKey = repoKey repo', sliceSource = bin}]}
      src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
      bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
      name = relName (sliceListName (getL osBaseDistro os))
      repo' = getL osLocalCopy os
      -- repo' = repoCD (EnvPath {envRoot = osRoot os, envPath = "/work/localpool"}) repo

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- | Set the location of the OSImage's root directory - where you
-- would cd to before running chroot.
chrootEnv :: OSImage -> EnvRoot -> OSImage
chrootEnv os dst = setL osRoot dst os

-- Sync the environment from the clean copy.  All this does besides
-- performing the proper rsync command is to make sure the destination
-- directory exists, otherwise rsync will fail.  Not sure why the 'work'
-- subdir is appended.  There must have been a reason at one point.
syncEnv :: OSImage -> OSImage -> IO OSImage
syncEnv src dst =
    mkdir >> umount >> rsync ["--exclude=/work/build/*"] (rootPath (getL osRoot src)) (rootPath (getL osRoot dst)) >> return dst
    where
      mkdir = createDirectoryIfMissing True (rootPath (getL osRoot dst) ++ "/work")
      umount =
          do qPutStrLn "syncEnv: umount"
             srcResult <- umountBelow False (rootPath (getL osRoot src))
             dstResult <- umountBelow False (rootPath (getL osRoot dst))
             case filter (\ (_, (code, _, _)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return ()
               failed -> fail $ "umount failure(s): " ++ show failed

-- | FIXME - we should notice the locale problem and run this.
localeGen :: String -> OSImage -> IO ()
localeGen locale os =
    do
      qPutStr ("Generating locale " ++  locale ++ " (" ++ stripDist (rootPath root) ++ ")...")
      result <- try $ useEnv (rootPath root) forceList (runProcess (shell cmd) L.empty) >>= return . collectOutputs
      either (\ (e :: SomeException) -> error $ "Failed to generate locale " ++ rootPath root ++ ": " ++ show e)
             (\ _ -> qPutStr ("done"))
             result
    where
      root = getL osRoot os
      cmd = "locale-gen " ++ locale


-- |To "neuter" an executable is to replace it with a hard link to
-- \/bin\/true in such a way that the operation can be reversed.  This
-- is done in order to make it safe to install files into it when it
-- isn't "live".  If this operation fails it is assumed that the
-- image is damaged, so it is removed.
neuterEnv :: OSImage -> IO ()
neuterEnv os =
    do qPutStr ("Neutering OS image (" ++ stripDist root ++ ")...")
       result <- try $ mapM_ (neuterFile os) neuterFiles
       either (\ (e :: SomeException) -> error $ "Failed to neuter environment " ++ root ++ ": " ++ show e)
              (\ _ -> qPutStrLn "done.")
              result
    where
      root = rootPath (getL osRoot os)

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
      root = getL osRoot os

-- |Reverse the neuterEnv operation.
restoreEnv :: OSImage -> IO OSImage
restoreEnv os =
    do
      qPutStr "De-neutering OS image..."
      result <- try $ mapM_ (restoreFile os) neuterFiles
      either (\ (e :: SomeException) -> error $ "damaged environment " ++ rootPath root ++ ": " ++ show e ++ "\n  please remove it.")
                 (\ _ -> return os) result
    where
      root = getL osRoot os

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
      root = getL osRoot os

-- | Build the dependency relations for the build essential packages.
-- For this to work the @build-essential@ package must be installed in
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
      let buildEssential'' = parseRelations (intercalate ", " relationText)
      let buildEssential' = either (\ l -> error ("parse error in /usr/share/build-essential/list:\n" ++ show l)) id buildEssential''
      return (essential ++ buildEssential')
    where
      root = getL osRoot os

-- |Remove an image.  The removeRecursiveSafely function is used to
-- ensure that any file systems mounted inside the image are unmounted
-- and not destroyed.
removeEnv :: OSImage -> IO ()
removeEnv os =
    do
      ePutStr "Removing build environment..."
      removeRecursiveSafely (rootPath root)
      ePutStrLn "done."
    where
      root = getL osRoot os

prefixes :: Maybe (L.ByteString, L.ByteString)
prefixes = Just (" 1> ", " 2> ")

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: OSImage -> IO NominalDiffTime
updateLists os =
    withProc os $ quieter 2 $ do
      qPutStrLn ("Updating OSImage " ++ root)
      out <- useEnv root forceList (runProcess update L.empty)
      _ <- case keepResult out of
             [ExitFailure _] ->
                 do _ <- useEnv root forceList (runProcessF prefixes configure L.empty)
                    useEnv root forceList (runProcessF prefixes update L.empty)
             _ -> return []
      (_, elapsed) <- timeTask (useEnv root forceList (runProcessF prefixes upgrade L.empty))
      return elapsed
    where
       root = rootPath (getL osRoot os)
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)

-- | Do an IO task in the build environment with /proc mounted.
withProc :: OSImage -> IO a -> IO a
withProc buildOS task =
    bracket (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/proc", dir] "")
            (\ _ -> readProcess "umount" [dir] "")
            (\ _ -> task)
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: PkgName n => OSImage -> [(n, Maybe DebianVersion)] -> IO ()
aptGetInstall os packages =
    useEnv (rootPath (rootDir os)) (return . force) $
           do _ <- runProcessF (Just (" 1> ", " 2> ")) p L.empty
              return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- |Create or update an OS image in which packages can be built.
prepareOSEnv' :: MonadIO m =>
              FilePath
           -> EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> m OSImage
prepareOSEnv' top root distro repo =
    do copy <- copyLocalRepo (EnvPath {envRoot = root, envPath = "/work/localpool"}) repo
       ePutStrLn ("Preparing clean " ++ relName (sliceListName distro) ++ " build environment at " ++ rootPath root ++ ", osLocalRepoMaster: " ++ show repo)
       arch <- liftIO buildArchOfRoot
       let os = OS { _osGlobalCacheDir = top
                   , _osRoot = root
                   , _osBaseDistro = distro
                   , _osArch = arch
                   , _osLocalMaster = repo
                   , _osLocalCopy = copy
                   , _osSourcePackages = []
                   , _osBinaryPackages = [] }
       return os
{-
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
                         replaceFile (sourcesPath os) (show . pretty . getL osBaseDistro $ os)
             os' <- buildEnv root distro arch (getL osLocalMaster os) (getL osLocalCopy os) include exclude components
             liftIO $ do doLocales os'
                         neuterEnv os'
             syncLocalPool os'
      doInclude os = liftIO $
          do aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) include)
             aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) optional) `catchIOError` (\ e -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales os =
          do localeName <- liftIO (try (getEnv "LANG") :: IO (Either SomeException String))
             liftIO $ localeGen (either (const "en_US.UTF-8") id localeName) os
-}

_pbuilderBuild' :: MonadIO m =>
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
_pbuilderBuild' cacheDir root distro arch repo copy _extraEssential _omitEssential _extra =
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
    do ePutStrLn ("Creating clean build environment (" ++ relName (sliceListName distro) ++ ")")
       ePutStrLn ("# " ++ cmd)
       liftIO (runProcess (shell cmd) L.empty) >>= liftIO . doOutput >>= foldOutputsL codefn outfn errfn exnfn (return ())
       ePutStrLn "done."
       let os = OS { _osGlobalCacheDir = cacheDir
                   , _osRoot = root
                   , _osBaseDistro = distro
                   , _osArch = arch
                   , _osLocalMaster = repo
                   , _osLocalCopy = copy
                   , _osSourcePackages = []
                   , _osBinaryPackages = [] }
       let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
       liftIO $ replaceFile sourcesPath' (show . pretty . osFullDistro $ os)
       return os
    where
      codefn _ ExitSuccess = return ()
      codefn _ failure = error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)
      outfn _ _ = return ()
      errfn _ _ = return ()
      exnfn _ _ = return ()
      cmd = intercalate " " $ [ "pbuilder"
                              , "--create"
                              , "--distribution", (relName . sliceListName $ distro)
                              , "--basetgz", cacheDir </> "pbuilderBase"
                              , "--buildplace", rootPath root
                              , "--preserve-buildplace"
                              ]

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
buildEnv' :: MonadIO m =>
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
buildEnv' top root distro arch repo copy include exclude components =
    quieter (-1) $
    do
      ePutStr (unlines [ "Creating clean build environment (" ++ relName (sliceListName distro) ++ ")"
                       , "  root: " ++ show root
                       , "  baseDist: " ++ show baseDist
                       , "  mirror: " ++ show mirror ])
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      runProcess (shell cmd) L.empty >>= foldOutputsL codefn outfn errfn exnfn (return ())
      ePutStrLn "done."
      let os = OS { _osGlobalCacheDir = top
                  , _osRoot = root
                  , _osBaseDistro = distro
                  , _osArch = arch
                  , _osLocalMaster = repo
                  , _osLocalCopy = copy
                  , _osSourcePackages = []
                  , _osBinaryPackages = [] }
      let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
      liftIO $ replaceFile sourcesPath' (show . pretty . osFullDistro $ os)
      return os
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

-- |Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the environment
-- where apt can see and install the packages.
syncLocalPool :: MonadIO m => OSImage -> m OSImage
syncLocalPool os =
    do repo' <- copyLocalRepo (EnvPath {envRoot = getL osRoot os, envPath = "/work/localpool"}) (getL osLocalMaster os)
       return $ setL osLocalCopy repo' os
