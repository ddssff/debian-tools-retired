module Debian.AutoBuilder.Env
    ( cleanEnv
    , dependEnv
    , buildEnv
    ) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo (EnvRoot(EnvRoot))
import Debian.Repo.Monads.Top (MonadTop, sub)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

buildEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnvOfRelease distro =
    do maybeCreateLink distro "build-Lax" "build"
       sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnvOfRelease distro =
    do maybeCreateLink distro "clean-Lax" "depend"
       sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    do maybeCreateLink distro "clean-Moderate" "clean"
       sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot

buildEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnv distro = buildEnvOfRelease distro

dependEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnv distro = dependEnvOfRelease distro

cleanEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro

maybeCreateLink :: (MonadIO m, MonadTop m) => ReleaseName -> String -> String -> m ()
maybeCreateLink distro old new =
    do newExists <- liftIO $ doesDirectoryExist new
       when (not newExists)
            (do path <- sub ("dists" </> releaseName' distro </> old)
                exists <- liftIO $ doesDirectoryExist path
                when exists (liftIO $ createSymbolicLink new path))
