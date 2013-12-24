{-# LANGUAGE PackageImports #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (MonadIO(liftIO))
import Data.Lens.Lazy (getL)
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archList, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo.Apt.OSImage (prepareOSEnv)
import Debian.Repo.Apt.Package (deleteGarbage)
import Debian.Repo.EnvPath (EnvRoot(rootPath), EnvRoot(EnvRoot))
import Debian.Repo.OSImage (OSImage, chrootEnv, osRoot)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.Repos (MonadRepos)
import Debian.Repo.Slice (NamedSliceList)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Top (MonadTop, sub)
import Prelude hiding (null)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

buildEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnv distro = buildEnvOfRelease distro

buildEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnv distro = dependEnvOfRelease distro

dependEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro

cleanEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot

prepareCleanOS :: (MonadRepos m, MonadTop m) => P.ParamRec -> NamedSliceList -> LocalRepository -> m OSImage
prepareCleanOS params rel localRepo =
    do cleanRoot <- cleanEnv (P.buildRelease params)
       prepareOSEnv cleanRoot
                    rel
                    localRepo
                    (P.flushRoot params)
                    (P.ifSourcesChanged params)
                    (P.includePackages params)
                    (P.optionalIncludePackages params)
                    (P.excludePackages params)
                    (P.components params)

prepareDependOS :: (MonadRepos m, MonadTop m) => P.ParamRec -> NamedSliceList -> m OSImage
prepareDependOS params rel =
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archList params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
       when (P.cleanUp params) (deleteGarbage localRepo)
       dependRoot <- dependEnv (P.buildRelease params)
       exists <- liftIO $ doesDirectoryExist (rootPath dependRoot)
       when (not exists || P.flushDepends params)
            (do cleanOS <- prepareCleanOS params rel localRepo
                _ <- rsync ["-x"] (rootPath (getL osRoot cleanOS)) (rootPath dependRoot)
                return ())
       prepareOSEnv dependRoot
                  rel
                  localRepo
                  False
                  (P.ifSourcesChanged params)
                  (P.includePackages params)
                  (P.optionalIncludePackages params)
                  (P.excludePackages params)
                  (P.components params)

prepareBuildOS :: (MonadRepos m, MonadTop m) => ReleaseName -> OSImage -> m OSImage
prepareBuildOS buildRel dependOS = chrootEnv dependOS <$> buildEnv buildRel
