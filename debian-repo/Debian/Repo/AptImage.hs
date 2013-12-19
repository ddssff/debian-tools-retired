{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( AptCache(..)
    , AptImage(..)
    , cacheRootDir
    , distDir
    , aptDir
    , sourcesPath
    , aptSourcePackagesSorted
    , buildArchOfEnv
    , buildArchOfRoot
    , SourcesChangedAction(..)
    , sourcePackages
    , binaryPackages
    , aptGetSource
    , aptGetUpdate

    , OSCache(..)
    , OSImage(..)
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
    ) where

import Control.DeepSeq (force, NFData)
import Control.Exception (bracket, evaluate, SomeException, try)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.Data (Data)
import Data.List (intercalate, sortBy)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (BinPkgName, ParseRelations(..), Relations, SrcPkgName(..))
import Debian.Release (parseReleaseName, parseSection', ReleaseName(relName))
import Debian.Repo.EnvPath (EnvPath(EnvPath), EnvRoot(rootPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (Slice(Slice, sliceRepoKey, sliceSource), SliceList, SliceList(..))
import Debian.Repo.Sync (rsync)
import Debian.Sources (DebSource(..), SourceType(..))
import "Extra" Extra.List (isSublistOf)
import Extra.Misc (sameInode, sameMd5sum)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Env (setEnv)
import System.Posix.Files (createLink)
import System.Process (proc, readProcess, readProcessWithExitCode, shell)
import System.Process.Progress (collectOutputs, ePutStr, ePutStrLn, keepResult, qPutStr, qPutStrLn, quieter, runProcess, runProcessF, timeTask)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (matchRegex, mkRegex)

import Debian.Relation (PkgName(..))
import System.Process (CreateProcess(cwd))
import Debian.Version (DebianVersion, prettyDebianVersion)

instance NFData ExitCode

-- | The AptCache class abstracts the basic properties of an apt-get
-- environment.  This represents some of the properties of an OSImage,
-- a complete build environment.  It is enough to run apt-get, and
-- thus obtain repository info and download source code packages from
-- a remote repository.
class (Ord t, Eq t, Show t) => AptCache t where
    globalCacheDir :: t -> FilePath
    -- | The directory you might chroot to.
    rootDir :: t -> EnvRoot
    -- | The sources.list without the local repository
    aptBaseSliceList :: t -> SliceList
    -- | The build architecture
    aptArch :: t -> Arch
    -- | Return the all source packages in this AptCache.
    aptSourcePackages :: t -> [SourcePackage]
    -- | Return the all binary packages for the architecture of this AptCache.
    aptBinaryPackages :: t -> [BinaryPackage]
    -- | Name of release
    aptReleaseName :: t -> ReleaseName

-- | The AptImage object is an instance of AptCache.
data AptImage =
    AptImage { aptGlobalCacheDir :: FilePath
             , aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSliceList :: SliceList
             , aptImageReleaseName :: ReleaseName
             , aptImageSourcePackages :: [SourcePackage]
             , aptImageBinaryPackages :: [BinaryPackage]
             }

instance Show AptImage where
    show apt = "AptImage " ++ relName (aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = aptGlobalCacheDir
    rootDir = aptImageRoot
    aptArch = aptImageArch
    aptBaseSliceList = aptImageSliceList
    aptSourcePackages = aptImageSourcePackages
    aptBinaryPackages = aptImageBinaryPackages
    aptReleaseName = aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (aptImageReleaseName a) (aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ

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

distDir :: AptCache c => c -> FilePath
distDir cache = cacheDistDir (globalCacheDir cache) (aptReleaseName cache)

aptDir :: AptCache c => c -> SrcPkgName -> FilePath
aptDir cache package = distDir cache ++ "/apt/" ++ unSrcPkgName package

-- | The path where a text of the SliceList is stored.
cacheSourcesPath :: FilePath -> ReleaseName -> FilePath
cacheSourcesPath cacheDir release = cacheDistDir cacheDir release </> "sources"

sourcesPath :: AptCache c => c -> FilePath
sourcesPath cache = cacheSourcesPath (globalCacheDir cache) (aptReleaseName cache)

-- |Return all the named source packages sorted by version
aptSourcePackagesSorted :: AptCache t => t -> [SrcPkgName] -> [SourcePackage]
aptSourcePackagesSorted os names =
    sortBy cmp . filterNames names . aptSourcePackages $ os
    where
      filterNames names' packages =
          filter (flip elem names' . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

buildArchOfEnv :: EnvRoot -> IO Arch
buildArchOfEnv (EnvRoot root)  =
    do setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
       a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (ArchOS os) (ArchCPU cpu)
         _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)

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

-- | Return a sorted list of available source packages, newest version first.
sourcePackages :: AptCache a => a -> [SrcPkgName] -> [SourcePackage]
sourcePackages os names =
    sortBy cmp . filterNames . aptSourcePackages $ os
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          filter (flip elem names . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

binaryPackages :: AptCache a => a -> [BinPkgName] -> [BinaryPackage]
binaryPackages os names =
    sortBy cmp . filterNames . aptBinaryPackages $ os
    where
      filterNames :: [BinaryPackage] -> [BinaryPackage]
      filterNames packages =
          filter (flip elem names . packageName . packageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . packageID $ p1
            v2 = packageVersion . packageID $ p2

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetSource :: (PkgName n, AptCache t) => t -> FilePath -> [(n, Maybe DebianVersion)] -> IO ()
aptGetSource os dir packages =
    createDirectoryIfMissing True dir >> runProcessF (Just (" 1> ", " 2> ")) p L.empty >> return ()
    where
      p = (proc "apt-get" args') {cwd = Just dir}
      args' = aptOpts os ++ ["source"] ++ map formatPackage packages
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

aptGetUpdate :: AptCache t => t -> IO ()
aptGetUpdate os =
    do _ <- runProcessF (Just (" 1> ", " 2> ")) (proc "apt-get" (aptOpts os ++ ["update"])) L.empty
       return ()

aptOpts :: AptCache t => t -> [String]
aptOpts os =
    [ "-o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status"
    , "-o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists"
    , "-o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives"
    , "-o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list"
    , "-o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d" ]
    where root = rootPath . rootDir $ os

class AptCache t => OSCache t where
    -- | The sources.list
    aptSliceList :: t -> SliceList

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
         , osLocalMaster :: LocalRepository
	 -- | A copy of osLocalMaster located inside the os root
	 -- environment.
         , osLocalCopy :: LocalRepository
         -- | A copy of osLocalMaster which is inside the changeroot
         , osSourcePackages :: [SourcePackage]
         , osBinaryPackages :: [BinaryPackage]
         }

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               rootPath (osRoot os),
                               relName (osReleaseName os),
                               show (osArch os),
                               show (osLocalCopy os)]

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

instance OSCache OSImage where
    aptSliceList = osFullDistro

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os =
    SliceList { slices = slices (osBaseDistro os) ++ slices localSources }
    where
      localSources :: SliceList
      localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey repo', sliceSource = src},
                                          Slice {sliceRepoKey = repoKey repo', sliceSource = bin}]}
      src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
      bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
      name = relName (osReleaseName os)
      repo' = osLocalCopy os
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
      root = osRoot os
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
      root = rootPath (osRoot os)

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
      root = osRoot os

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
      root = osRoot os

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
       root = rootPath (osRoot os)
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
aptGetInstall :: (PkgName n, OSCache t) => t -> [(n, Maybe DebianVersion)] -> IO ()
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
