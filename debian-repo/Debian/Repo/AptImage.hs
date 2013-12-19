{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( AptImage
    , aptDir
    , aptImageSliceList
    , aptImageSourcePackages
    , aptImageBinaryPackages
    , prepareAptEnv''
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
import Debian.Repo.AptCache (AptCache(..), distDir)
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath), envRoot, EnvRoot(rootPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList, SliceList(..))
import Debian.Repo.Sync (rsync)
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SliceName(sliceName), SourceType(..), SourceType(..))
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

-- | The AptImage object is an instance of AptCache.
data AptImage =
    AptImage { _aptGlobalCacheDir :: FilePath
             , _aptImageRoot :: EnvRoot
             , _aptImageArch :: Arch
             , _aptImageSliceList :: SliceList
             , _aptImageReleaseName :: ReleaseName
             , _aptImageSourcePackages :: [SourcePackage]
             , _aptImageBinaryPackages :: [BinaryPackage]
             }

$(makeLenses [''AptImage])

instance Show AptImage where
    show apt = "AptImage " ++ relName (getL aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = getL aptGlobalCacheDir
    rootDir = getL aptImageRoot
    aptArch = getL aptImageArch
    aptBaseSliceList = getL aptImageSliceList
    aptSourcePackages = getL aptImageSourcePackages
    aptBinaryPackages = getL aptImageBinaryPackages
    aptReleaseName = getL aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (getL aptImageReleaseName a) (getL aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ

-- | The location of the top directory of a source packages's files in
-- an AptImage (but not an OSImage.)
aptDir :: AptImage -> SrcPkgName -> FilePath
aptDir cache package = distDir cache </> "apt" </> unSrcPkgName package

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

prepareAptEnv'' :: MonadIO m => FilePath -> NamedSliceList -> m AptImage
prepareAptEnv'' cacheDir sources =
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
       arch <- liftIO buildArchOfRoot
       let os = AptImage { _aptGlobalCacheDir = cacheDir
                         , _aptImageRoot = EnvRoot root
                         , _aptImageArch = arch
                         , _aptImageReleaseName = ReleaseName . sliceName . sliceListName $ sources
                         , _aptImageSliceList = sliceList sources
                         , _aptImageSourcePackages = []
                         , _aptImageBinaryPackages = [] }
       return os
