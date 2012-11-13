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
import System.FilePath ((</>))

buildEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot

buildEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnv distro = buildEnvOfRelease distro

dependEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnv distro = dependEnvOfRelease distro

cleanEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro
