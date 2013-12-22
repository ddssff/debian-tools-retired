{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptCache
    ( MonadCache(rootDir, aptBaseSources, aptArch, aptSourcePackages, aptBinaryPackages)
    , aptReleaseName
    , distDir
    , sourcesPath
    , aptSourcePackagesSorted
    , buildArchOfEnv
    , buildArchOfRoot
    , SourcesChangedAction(..)
    , sourcePackages
    , binaryPackages
    , aptGetSource
    , aptGetUpdate
    ) where

import Control.Applicative ((<$>))
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
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath), envRoot, EnvRoot(rootPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList, SliceList(..))
import Debian.Repo.Sync (rsync)
import Debian.Repo.Top (MonadTop, askTop)
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

instance NFData ExitCode

-- | The AptCache class abstracts the basic properties of an apt-get
-- environment.  This represents some of the properties of an OSImage,
-- a complete build environment.  It is enough to run apt-get, and
-- thus obtain repository info and download source code packages from
-- a remote repository.
class (Monad m, Functor m) => MonadCache m where
    rootDir :: m EnvRoot
    -- ^ The directory you might chroot to.
    aptBaseSources :: m NamedSliceList
    -- ^ The sources.list excluding lines for the local repository
    aptArch :: m Arch
    -- ^ The build architecture
    aptSourcePackages :: m [SourcePackage]
    -- ^ Return the all source packages in this AptCache.
    aptBinaryPackages :: m [BinaryPackage]
    -- ^ Return the all binary packages for the architecture of this AptCache.

aptReleaseName :: MonadCache m => m ReleaseName
aptReleaseName = sliceListName <$> aptBaseSources

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

-- | The directory in a repository where the package index files for a
-- particular dist or release is stored.  (Wait, that's not right.)
distDir :: (MonadTop m, MonadCache m) => m FilePath
distDir =
    do top <- askTop
       rel <- relName <$> aptReleaseName
       return $ top </> "dists" </> rel

-- | The path of the text file containing the sources.list (aka SliceList)
sourcesPath :: (MonadTop m, MonadCache m) => m FilePath
sourcesPath =
    do dir <- distDir
       return $ dir </> "sources"

-- |Return all the named source packages sorted by version
aptSourcePackagesSorted :: MonadCache m => [SrcPkgName] -> m [SourcePackage]
aptSourcePackagesSorted names =
    (sortBy cmp . filterNames names) <$> aptSourcePackages
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
sourcePackages :: MonadCache m => [SrcPkgName] -> m [SourcePackage]
sourcePackages names =
    (sortBy cmp . filterNames) <$> aptSourcePackages
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          filter (flip elem names . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

binaryPackages :: MonadCache m => [BinPkgName] -> m [BinaryPackage]
binaryPackages names =
    (sortBy cmp . filterNames) <$> aptBinaryPackages
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
aptGetSource :: (PkgName n, MonadIO m, MonadCache m) => FilePath -> [(n, Maybe DebianVersion)] -> m ()
aptGetSource dir packages =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["source"] ++ map formatPackage packages)) {cwd = Just dir}
       liftIO $ createDirectoryIfMissing True dir >> runProcessF (Just (" 1> ", " 2> ")) p L.empty >> return ()
    where
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

aptGetUpdate :: (MonadCache m, MonadIO m) => m ()
aptGetUpdate =
    do args <- aptOpts
       _ <- liftIO $ runProcessF (Just (" 1> ", " 2> ")) (proc "apt-get" (args ++ ["update"])) L.empty
       return ()

aptOpts :: MonadCache m => m [String]
aptOpts =
    do root <- rootPath <$> rootDir
       return $ [ "-o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status"
                , "-o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists"
                , "-o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives"
                , "-o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list"
                , "-o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d" ]

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"
