{-# LANGUAGE PackageImports #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (MonadIO(liftIO))
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archList, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo.Apt (MonadDeb)
import Debian.Repo.Apt.AptImage (prepareOSEnv)
import Debian.Repo.Apt.Package (deleteGarbage)
import Debian.Repo.AptImage (chrootEnv, OSImage)
import Debian.Repo.EnvPath (EnvRoot(rootPath), EnvRoot(EnvRoot))
import Debian.Repo.Slice (NamedSliceList)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Top (MonadTop, sub)
import Prelude hiding (null)
import System.Directory (doesDirectoryExist)
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

prepareDependOS :: MonadDeb m => P.ParamRec -> NamedSliceList -> m OSImage
prepareDependOS params buildRelease =
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archList params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
       when (P.cleanUp params) (deleteGarbage localRepo)
       dependRoot <- dependEnv (P.buildRelease params)
       exists <- liftIO $ doesDirectoryExist (rootPath dependRoot)
       when (not exists || P.flushDepends params)
            (do cleanRoot <- cleanEnv (P.buildRelease params)
                _ <- prepareOSEnv cleanRoot
                                buildRelease
                                localRepo
                                (P.flushRoot params)
                                (P.ifSourcesChanged params)
                                (P.includePackages params)
                                (P.optionalIncludePackages params)
                                (P.excludePackages params)
                                (P.components params)
                _ <- rsync ["-x"] (rootPath cleanRoot) (rootPath dependRoot)
                return ())
       prepareOSEnv dependRoot
                  buildRelease
                  localRepo
                  False
                  (P.ifSourcesChanged params)
                  (P.includePackages params)
                  (P.optionalIncludePackages params)
                  (P.excludePackages params)
                  (P.components params)

prepareBuildOS :: MonadDeb m => ReleaseName -> OSImage -> m OSImage
prepareBuildOS buildRel dependOS = chrootEnv dependOS <$> buildEnv buildRel
