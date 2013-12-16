{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.OSImage 
    ( OSImage(..)
    , updateLists
    , neuterEnv
    , chrootEnv
    , syncEnv
    , localeGen
    , restoreEnv
    , removeEnv
    , buildEssential
    , withProc
    ) where

import Control.Exception (bracket, evaluate, SomeException, try)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.List (intercalate)
import Data.Time (NominalDiffTime)
import Debian.Arch (Arch)
import Debian.Relation (ParseRelations(..), Relations)
import Debian.Release (parseReleaseName, parseSection', ReleaseName(..))
import Debian.Repo.AptImage (AptBuildCache(..), AptCache(..))
import Debian.Repo.EnvPath (EnvPath(EnvPath), EnvRoot(rootPath), outsidePath)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Repository (fromLocalRepository)
import Debian.Repo.Slice (Slice(Slice, sliceRepoKey, sliceSource), SliceList(..))
import Debian.Repo.Sync (rsync)
import Debian.Sources (DebSource(DebSource), SourceType(Deb, DebSrc))
import "Extra" Extra.List (isSublistOf)
import Extra.Misc (sameInode, sameMd5sum)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Posix.Files (createLink)
import System.Process (proc, readProcess, shell)
import System.Process.Progress (collectOutputs, ePutStr, ePutStrLn, keepResult, qPutStr, qPutStrLn, quieter, runProcess, runProcessF, timeTask)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (matchRegex, mkRegex)

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

instance AptBuildCache OSImage where
    aptSliceList = osFullDistro

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os =
    SliceList { slices = slices (osBaseDistro os) ++ slices localSources }
    where
      localSources :: SliceList
      localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey (fromLocalRepository repo'), sliceSource = src},
                                          Slice {sliceRepoKey = repoKey (fromLocalRepository repo'), sliceSource = bin}]}
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

{-
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
-}

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
