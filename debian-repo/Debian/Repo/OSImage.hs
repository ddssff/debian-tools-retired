{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.OSImage
    ( OSImage
    , osBaseDistro
    , osFullDistro
    , osLocalMaster
    , osLocalCopy
    , osRoot
    , osArch
    , osSourcePackageCache
    , osBinaryPackageCache
    , MonadOS

    , buildArchOfRoot

    , chrootEnv
    , localeGen
    , neuterEnv
    , restoreEnv
    , buildEssential
    , removeEnv
    , updateLists
    , withProc
    , withTmp
    , aptGetInstall

    , createOSImage
    , _pbuilderBuild'
    , buildOS'
    , syncLocalPool
    , osFlushPackageCache
    , syncOS'
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (evaluate, SomeException)
import Control.Monad.Catch (bracket, MonadCatch, throwM, try)
import Control.Monad.State (evalStateT, MonadState, StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Data (Data)
import Data.Lens.Lazy (getL, setL)
import Data.Lens.Template (makeLenses)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Pretty (pretty)
import Debian.Relation (ParseRelations(parseRelations), PkgName, Relations)
import Debian.Release (parseReleaseName, parseSection', ReleaseName(relName))
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath), envRoot, EnvRoot, EnvRoot(rootPath), outsidePath)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (access, (~=), rsync, runProc, readProc, symbol, sameInode, sameMd5sum, replaceFile, isSublistOf)
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList, SliceList(..))
import Debian.Repo.Top (askTop, MonadTop)
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SourceType(..), SourceType(..))
import Debian.URI (uriToString')
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Files (createLink)
import System.Process (proc, readProcess, readProcessWithExitCode, shell)
import System.Process.Progress (collectOutputs, doOutput, ePutStr, ePutStrLn, foldOutputsL, keepResult, qPutStr, qPutStrLn, timeTask)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.Regex (matchRegex, mkRegex)

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { _osRoot :: EnvRoot
         , _osBaseDistro :: NamedSliceList
         , _osArch :: Arch
         , _osLocalMaster :: LocalRepository
	 -- ^ The associated local repository, where packages we build
	 -- inside this image are first uploaded to.
         , _osLocalCopy :: LocalRepository
	 -- ^ A copy of osLocalMaster located inside the os root environment.
         , _osSourcePackageCache :: Maybe [SourcePackage]
         , _osBinaryPackageCache :: Maybe [BinaryPackage]
         }

$(makeLenses [''OSImage])

class (MonadState OSImage m, Monad m, Functor m) => MonadOS m

instance (Monad m, Functor m) => MonadOS (StateT OSImage m)

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

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

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: MonadOS m => m SliceList
osFullDistro =
    do base <- access osBaseDistro
       repo' <- access osLocalCopy
       let name = relName (sliceListName base)
           localSources :: SliceList
           localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey repo', sliceSource = src},
                                               Slice {sliceRepoKey = repoKey repo', sliceSource = bin}]}
           src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
           bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
       return $ SliceList { slices = slices (sliceList base) ++ slices localSources }

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

syncOS' :: OSImage -> OSImage -> IO OSImage
syncOS' src dst =
    mkdir >> umount >> rsync ["--exclude=/work/build/*"] (rootPath (getL osRoot src)) (rootPath (getL osRoot dst)) >> return dst
    where
      mkdir = createDirectoryIfMissing True (rootPath (getL osRoot dst) ++ "/work")
      umount =
          do srcResult <- umountBelow False (rootPath (getL osRoot src))
             dstResult <- umountBelow False (rootPath (getL osRoot dst))
             case filter (\ (_, (code, _, _)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return ()
               failed -> fail $ "umount failure(s): " ++ show failed

-- | FIXME - we should notice the locale problem and run this.
localeGen :: (MonadOS m, MonadIO m) => String -> m ()
localeGen locale =
    do root <- access osRoot
       qPutStr ("Generating locale " ++  locale ++ " (" ++ stripDist (rootPath root) ++ ")...")
       result <- liftIO $ try $ useEnv (rootPath root) forceList (readProc (shell cmd)) >>= return . collectOutputs
       either (\ (e :: SomeException) -> error $ "Failed to generate locale " ++ rootPath root ++ ": " ++ show e)
              (\ _ -> qPutStrLn "done")
              result
    where
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
buildEssential :: (MonadOS m, MonadIO m) => m Relations
buildEssential =
    access osRoot >>= \ root ->
    liftIO $ do
      -- qPutStrLn "Computing build essentials"
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
updateLists :: (MonadOS m, MonadIO m, MonadCatch m) => m NominalDiffTime
updateLists =
    do root <-rootPath <$> access osRoot
       withProc $ liftIO $ do
         qPutStrLn ($(symbol 'updateLists) <> ": updating OSImage " ++ root)
         out <- useEnv root forceList (readProc update)
         _ <- case keepResult out of
                [ExitFailure _] ->
                    do _ <- useEnv root forceList (runProc configure)
                       useEnv root forceList (runProc update)
                _ -> return []
         (_, elapsed) <- timeTask (useEnv root forceList (runProc upgrade))
         return elapsed
    where
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)

-- | Do an IO task in the build environment with /proc mounted.
withProc :: forall m c. (MonadOS m, MonadIO m, MonadCatch m) => m c -> m c
withProc task =
    do root <- rootPath <$> access osRoot
       let dir = root </> "proc"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/proc", dir] "")
           post :: String -> m String
           post _s = liftIO $ readProcess "umount" [dir] ""
           task' :: String -> m c
           task' _s = task
       bracket pre post task'

-- | Do an IO task in the build environment with /proc mounted.
withTmp :: forall m c. (MonadOS m, MonadIO m, MonadCatch m) => m c -> m c
withTmp task =
    do root <- rootPath <$> access osRoot
       let dir = root </> "tmp"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/tmp", dir] "")
           post :: String -> m String
           post _ = liftIO $ readProcess "umount" [dir] ""
           task' :: String -> m c
           task' _ = try task >>= either (\ (e :: SomeException) -> throwM e) return
       bracket pre post task'

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- rootPath <$> access osRoot
       liftIO $ useEnv root (return . force) $ do
         _ <- runProc p
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
createOSImage :: (MonadIO m, MonadTop m) =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> m OSImage
createOSImage root distro repo =
    do ePutStrLn ("Preparing clean " ++ relName (sliceListName distro) ++ " build environment at " ++ rootPath root)
       copy <- copyLocalRepo (EnvPath {envRoot = root, envPath = "/work/localpool"}) repo
       arch <- liftIO buildArchOfRoot
       let os = OS { _osRoot = root
                   , _osBaseDistro = distro
                   , _osArch = arch
                   , _osLocalMaster = repo
                   , _osLocalCopy = copy
                   , _osSourcePackageCache = Nothing
                   , _osBinaryPackageCache = Nothing }
       return os

_pbuilderBuild' :: (MonadIO m, MonadTop m, Functor m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
_pbuilderBuild' root distro arch repo copy _extraEssential _omitEssential _extra =
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
    do top <- askTop
       ePutStrLn ("Creating clean build environment (" ++ relName (sliceListName distro) ++ ")")
       ePutStrLn ("# " ++ cmd top)
       let codefn _ ExitSuccess = return ()
           codefn _ failure = error ("Could not create build environment:\n " ++ cmd top ++ " -> " ++ show failure)
       liftIO (readProc (shell (cmd top))) >>= liftIO . doOutput >>= foldOutputsL codefn outfn errfn exnfn (return ())
       ePutStrLn "done."
       let os = OS { _osRoot = root
                   , _osBaseDistro = distro
                   , _osArch = arch
                   , _osLocalMaster = repo
                   , _osLocalCopy = copy
                  , _osSourcePackageCache = Nothing
                  , _osBinaryPackageCache = Nothing }
       let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
       sources <- (show . pretty) <$> evalStateT osFullDistro os
       liftIO $ replaceFile sourcesPath' sources
       return os
    where
      outfn _ _ = return ()
      errfn _ _ = return ()
      exnfn _ _ = return ()
      cmd top =
          intercalate " " $ [ "pbuilder"
                            , "--create"
                            , "--distribution", (relName . sliceListName $ distro)
                            , "--basetgz", top </> "pbuilderBase"
                            , "--buildplace", rootPath root
                            , "--preserve-buildplace"
                            ]

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
buildOS' :: (MonadIO m, MonadTop m, Functor m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
buildOS' root distro arch repo copy include exclude components =
    do
      ePutStr (unlines [ "Creating clean build environment (" ++ relName (sliceListName distro) ++ ")"
                       , "  root: " ++ show root
                       , "  baseDist: " ++ show baseDist
                       , "  mirror: " ++ show mirror ])
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      readProc (shell cmd) >>= foldOutputsL codefn outfn errfn exnfn (return ())
      ePutStrLn "done."
      let os = OS { _osRoot = root
                  , _osBaseDistro = distro
                  , _osArch = arch
                  , _osLocalMaster = repo
                  , _osLocalCopy = copy
                  , _osSourcePackageCache = Nothing
                  , _osBinaryPackageCache = Nothing }
      let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
      sources <- (show . pretty) <$> evalStateT osFullDistro os
      liftIO $ replaceFile sourcesPath' sources
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

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: (MonadIO m, MonadOS m) => m ()
syncLocalPool =
    do root <- access osRoot
       dist <- access osLocalMaster
       repo' <- copyLocalRepo (EnvPath {envRoot = root, envPath = "/work/localpool"}) dist
       osLocalCopy ~= repo'
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache
       return ()

osFlushPackageCache :: MonadOS m => m ()
osFlushPackageCache = do
    osSourcePackageCache ~= Nothing
    osBinaryPackageCache ~= Nothing
