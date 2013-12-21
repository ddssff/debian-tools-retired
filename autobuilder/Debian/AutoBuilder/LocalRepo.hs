-- | Code pertaining to the local repository created to hold newly
-- built packages before uploading them to a remote repository.
module Debian.AutoBuilder.LocalRepo
    ( subDir
    , poolDir
    , prepare
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Debian.Arch (Arch)
import Debian.Release (ReleaseName, releaseName', parseSection')
import Debian.Repo.EnvPath (rootEnvPath)
import Debian.Repo.LocalRepository (LocalRepository, prepareLocalRepository)
import Debian.Repo.Apt.Release (prepareRelease)
import Debian.Repo.Repos (MonadRepos)
import Debian.Repo.Top (MonadTop(askTop))
import System.FilePath ((</>))
import System.Unix.Directory(removeRecursiveSafely)

subDir :: String
subDir = "localpools"

-- |Location of the local repository for uploaded packages.
poolDir :: MonadTop m => ReleaseName -> m FilePath
poolDir rel = askTop >>= \ top -> return $ top </> subDir </> releaseName' rel

prepare :: (MonadRepos m, MonadTop m) => Bool -> ReleaseName -> [Arch] -> m LocalRepository
prepare flush rel archlist =
    do localRepo <- poolDir rel
       when flush (liftIO (removeRecursiveSafely localRepo))
       repo <- prepareLocalRepository (rootEnvPath localRepo) Nothing
       prepareRelease repo rel [] [parseSection' "main"] archlist
       return repo
