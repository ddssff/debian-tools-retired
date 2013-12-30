{-# LANGUAGE PackageImports #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State (MonadIO(liftIO), get)
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archList, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo.Apt.OSImage (prepareOS)
import Debian.Repo.Apt.Package (deleteGarbage)
import Debian.Repo.EnvPath (EnvRoot(rootPath), EnvRoot(EnvRoot))
import Debian.Repo.OSImage (MonadOS, chrootEnv, osRoot)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.Prelude (access, checkRsyncExitCode)
import Debian.Repo.Repos (MonadRepos, OSKey, evalMonadOS, putOSImage)
import Debian.Repo.Slice (NamedSliceList)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Top (MonadTop, sub)
import Prelude hiding (null)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

buildRoot :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildRoot distro = sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependRoot :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependRoot distro = sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro

cleanEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot

prepareCleanOS :: (MonadRepos m, MonadTop m) => P.ParamRec -> NamedSliceList -> LocalRepository -> m OSKey
prepareCleanOS params rel localRepo =
    do cleanRoot <- cleanEnv (P.buildRelease params)
       prepareOS cleanRoot
                    rel
                    localRepo
                    (P.flushRoot params)
                    (P.ifSourcesChanged params)
                    (P.includePackages params)
                    (P.optionalIncludePackages params)
                    (P.excludePackages params)
                    (P.components params)

prepareDependOS :: (MonadRepos m, MonadTop m) => P.ParamRec -> NamedSliceList -> m OSKey
prepareDependOS params rel =
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archList params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
       when (P.cleanUp params) (deleteGarbage localRepo)
       dRoot <- dependRoot (P.buildRelease params)
       exists <- liftIO $ doesDirectoryExist (rootPath dRoot)
       when (not exists || P.flushDepends params)
            (do cleanOS <- prepareCleanOS params rel localRepo
                code <- evalMonadOS (access osRoot >>= \ cRoot -> liftIO (rsync ["-x"] (rootPath cRoot) (rootPath dRoot))) cleanOS
                checkRsyncExitCode code
                return ())
       prepareOS dRoot
                  rel
                  localRepo
                  False
                  (P.ifSourcesChanged params)
                  (P.includePackages params)
                  (P.optionalIncludePackages params)
                  (P.excludePackages params)
                  (P.components params)

prepareBuildOS :: (MonadOS m, MonadTop m, MonadRepos m, Applicative m) => ReleaseName -> m OSKey
prepareBuildOS rel = do
    os <- chrootEnv <$> get <*> buildRoot rel
    putOSImage os
