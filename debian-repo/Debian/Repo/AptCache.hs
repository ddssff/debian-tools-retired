{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptCache
    ( MonadCache(rootDir, aptBaseSources, aptArch, aptSourcePackages, aptBinaryPackages)
    , aptReleaseName
    , distDir
    , sourcesPath
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
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Data (Data)
import Data.List (sortBy)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (BinPkgName, PkgName, SrcPkgName)
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(..))
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Repo.Slice (NamedSliceList(sliceListName), SliceList)
import Debian.Repo.Top (askTop, MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Env (setEnv)
import System.Process (CreateProcess(cwd), proc, readProcessWithExitCode)
import System.Process.Progress (runProcessF)
import System.Unix.Chroot (useEnv)
import Text.PrettyPrint.ANSI.Leijen (pretty)

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
