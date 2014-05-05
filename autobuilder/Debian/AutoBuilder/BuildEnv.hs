{-# LANGUAGE PackageImports #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    , envSet
    ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State (MonadIO(liftIO), get)
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archSet, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Debianize.Types.Atoms (EnvSet(..))
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo.EnvPath (EnvRoot(rootPath), EnvRoot(EnvRoot))
import Debian.Repo.OSImage (MonadOS, chrootEnv, osRoot, syncLocalPool)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.Prelude (access, checkRsyncExitCode, rsync)
import Debian.Repo.Slice (NamedSliceList)
import Debian.Repo.State (MonadRepos, OSKey, evalMonadOS, putOSImage)
import Debian.Repo.State.OSImage (prepareOS)
import Debian.Repo.State.Package (deleteGarbage, evalInstall)
import Debian.Repo.Top (MonadTop, sub)
import Prelude hiding (null)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

envSet :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvSet
envSet distro = sub ("dists" </> releaseName' distro) >>= \ parent ->
                return (EnvSet {cleanOS = parent </> "clean", dependOS = parent </> "depend", buildOS = parent </> "build"})

{-
buildRoot :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
buildRoot distro = sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependRoot :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
dependRoot distro = sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnv :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro

cleanEnvOfRelease :: (MonadIO m, MonadTop m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot
-}

prepareCleanOS :: (MonadRepos m, MonadTop m) => P.ParamRec -> NamedSliceList -> LocalRepository -> m OSKey
prepareCleanOS params rel localRepo =
    do eset <- envSet (P.buildRelease params)
       prepareOS eset
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
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archSet params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archSet params)
       when (P.cleanUp params) (evalInstall deleteGarbage localRepo Nothing)
       eset <- envSet (P.buildRelease params)
       let dRoot = dependOS eset
       exists <- liftIO $ doesDirectoryExist dRoot
       when (not exists || P.flushDepends params)
            (do cOS <- prepareCleanOS params rel localRepo
                code <- evalMonadOS (access osRoot >>= \ cRoot -> liftIO (rsync ["-x"] (rootPath cRoot) dRoot)) cOS
                checkRsyncExitCode code
                return ())
       dOS <- prepareOS eset rel localRepo False (P.ifSourcesChanged params) (P.includePackages params) (P.optionalIncludePackages params) (P.excludePackages params) (P.components params)
       evalMonadOS syncLocalPool dOS
       return dOS

prepareBuildOS :: (MonadOS m, MonadTop m, MonadRepos m, Applicative m) => ReleaseName -> m OSKey
prepareBuildOS rel = do
  s <- envSet rel >>= return . EnvRoot . buildOS
  os <- chrootEnv <$> get <*> return s
  putOSImage os
