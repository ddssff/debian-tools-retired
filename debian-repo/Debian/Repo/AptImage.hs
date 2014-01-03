{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( AptImage
    , MonadApt(getApt, putApt)
    , modifyApt
    , aptDir
    , aptImageRoot
    , aptImageArch
    , aptImageSources
    , createAptImage
    , cacheRootDir
    , aptGetSource
    , aptGetUpdate
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad.State (MonadState(get, put), StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Data (Data)
import Data.Lens.Lazy (getL)
import Data.Lens.Template (makeLenses)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Relation (PkgName, SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(EnvRoot, rootPath))
import Debian.Repo.Prelude (runProc)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName))
import Debian.Repo.Top (distDir, dists, MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Extra.Files (replaceFile, writeFileIfMissing)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), proc, readProcessWithExitCode)
import Text.PrettyPrint.ANSI.Leijen (pretty)

data AptImage =
    AptImage { _aptImageRoot :: EnvRoot
             , _aptImageArch :: Arch
             , _aptImageSources :: NamedSliceList
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

instance Ord AptImage where
    compare a b = compare (sliceListName . getL aptImageSources $ a) (sliceListName . getL aptImageSources $ b)

instance Eq AptImage where
    a == b = compare a b == EQ

-- | The location of the top directory of a source packages's files in
-- an AptImage (but not an OSImage.)
aptDir :: (MonadTop m, MonadApt m) => SrcPkgName -> m FilePath
aptDir package =
    do rel <- getL aptImageSources <$> getApt
       dir <- distDir (sliceListName rel)
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
    let apt = AptImage { _aptImageRoot = root
                       , _aptImageArch = arch
                       , _aptImageSources = sources }

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
    return apt

cacheRootDir :: MonadTop m => ReleaseName -> m EnvRoot
cacheRootDir release = EnvRoot . (</> relName release </> "aptEnv") <$> dists

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetSource :: (MonadIO m, MonadApt m, PkgName n) => FilePath -> [(n, Maybe DebianVersion)] -> m ()
aptGetSource dir packages =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["source"] ++ map formatPackage packages)) {cwd = Just dir}
       liftIO $ createDirectoryIfMissing True dir >> runProc p >> return ()
    where
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

aptGetUpdate :: (MonadIO m, MonadApt m) => m ()
aptGetUpdate =
    do args <- aptOpts
       _ <- liftIO $ runProc (proc "apt-get" (args ++ ["update"]))
       return ()

aptOpts :: MonadApt m => m [String]
aptOpts =
    do root <- (rootPath . getL aptImageRoot) <$> getApt
       return $ [ "-o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status"
                , "-o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists"
                , "-o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives"
                , "-o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list"
                , "-o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d" ]
