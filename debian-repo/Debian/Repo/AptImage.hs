{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( AptImage
    , MonadApt(getApt, putApt)
    , modifyApt
    , aptDir
    , aptImageRoot
    , aptImageSources
    , aptImageSourcePackages
    , aptImageBinaryPackages
    , aptSourcePackagesSorted
    , createAptImage
    , cacheRootDir
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad.State (StateT, MonadState(get, put))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Data (Data)
import Data.Lens.Lazy (getL)
import Data.Lens.Template (makeLenses)
import Data.List (sortBy)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.AptCache (distDir, MonadCache(..))
import Debian.Repo.EnvPath (EnvRoot(..))
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName))
import Debian.Repo.Top (askTop, MonadTop)
import Extra.Files (replaceFile, writeFileIfMissing)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.PrettyPrint.ANSI.Leijen (pretty)

data AptImage =
    AptImage { _aptImageRoot :: EnvRoot
             , _aptImageArch :: Arch
             , _aptImageSources :: NamedSliceList
             , _aptImageSourcePackages :: [SourcePackage]
             , _aptImageBinaryPackages :: [BinaryPackage]
             }

$(makeLenses [''AptImage])

class (Monad m, Functor m) => MonadApt m where
    getApt :: m AptImage
    putApt :: AptImage -> m ()

modifyApt :: MonadApt m => (AptImage -> AptImage) -> m ()
modifyApt f = getApt >>= putApt . f

instance (Monad m, Functor m) => MonadApt (StateT AptImage m) where
    getApt = get
    putApt = put

instance Show AptImage where
    show apt = "AptImage " ++ relName (sliceListName (getL aptImageSources apt))

instance (Monad m, Functor m) => MonadCache (StateT AptImage m) where
    rootDir = _aptImageRoot <$> getApt
    aptArch = _aptImageArch <$> getApt
    aptBaseSources = _aptImageSources <$> getApt
    aptSourcePackages = _aptImageSourcePackages <$> getApt
    aptBinaryPackages = _aptImageBinaryPackages <$> getApt

instance Ord AptImage where
    compare a b = compare (sliceListName . getL aptImageSources $ a) (sliceListName . getL aptImageSources $ b)

instance Eq AptImage where
    a == b = compare a b == EQ

-- | The location of the top directory of a source packages's files in
-- an AptImage (but not an OSImage.)
aptDir :: (MonadTop m, MonadCache m) => SrcPkgName -> m FilePath
aptDir package =
    do dir <- distDir
       return $ dir </> "apt" </> unSrcPkgName package

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

createAptImage :: (MonadTop m, MonadIO m) => NamedSliceList -> m AptImage
createAptImage sources = do
  root <- cacheRootDir (sliceListName sources)
  liftIO $ do
    arch <- buildArchOfRoot
    let os = AptImage { _aptImageRoot = root
                      , _aptImageArch = arch
                      , _aptImageSources = sources
                      , _aptImageSourcePackages = []
                      , _aptImageBinaryPackages = [] }

    --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/cache/apt/archives/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/dpkg")
    createDirectoryIfMissing True (rootPath root ++ "/etc/apt")
    writeFileIfMissing True (rootPath root ++ "/var/lib/dpkg/status") ""
    writeFileIfMissing True (rootPath root ++ "/var/lib/dpkg/diversions") ""
    -- We need to create the local pool before updating so the
    -- sources.list will be valid.
    let sourceListText = show (pretty (sliceList sources))
    -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
    replaceFile (rootPath root ++ "/etc/apt/sources.list") sourceListText
    return os

cacheRootDir :: MonadTop m => ReleaseName -> m EnvRoot
cacheRootDir release =
    do top <- askTop
       return $ EnvRoot (top </> "dists" </> relName release </> "aptEnv")

-- |Return all the named source packages sorted by version
aptSourcePackagesSorted :: MonadApt m => [SrcPkgName] -> m [SourcePackage]
aptSourcePackagesSorted names =
    (sortBy cmp . filterNames names . getL aptImageSourcePackages) <$> getApt
    where
      filterNames names' packages =
          filter (flip elem names' . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2
