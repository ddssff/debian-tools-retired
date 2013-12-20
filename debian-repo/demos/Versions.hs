{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TupleSections #-}
-- |Print the available version numbers of a package.
module Main where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.Maybe (fromJust)
import Debian.Arch (Arch(Binary), ArchCPU(..), ArchOS(..))
import Debian.Repo.Apt.Release (insertRelease)
import Debian.Repo.Repo (RepoKey(Remote), repoReleaseInfo)
import Debian.Repo.Repos (MonadRepos, foldRepository, runReposT)
import Debian.URI (readURI')

main :: IO ()
main = runReposT main'

main' :: MonadRepos m => m ()
main' =
    do foldRepository f g (Remote (fromJust (readURI' uri)))
    where
      f repo =
          do releases <- mapM (insertRelease repo) (repoReleaseInfo repo)
             liftIO (putStrLn ("\n" ++ show releases))
      g repo = 
          do releases <- mapM (insertRelease repo) (repoReleaseInfo repo)
             liftIO (putStrLn ("\n" ++ show releases))
{-
    do repo <- prepareRepository (Remote (fromJust (readURI' uri)))
       releases <- mapM (insertRelease repo) (repoReleaseInfo repo)
       -- let binaryIndexes = map (filter (\ i -> packageIndexArch i == arch)) (map binaryIndexList releases)
       -- _binaryPackages <- mapM (packageLists release) binaryIndexes
       liftIO (putStrLn ("\n" ++ show releases))
-}
{-
    where
      insert repo info = insertRelease repo 
-}

-- packageLists :: MonadRepos m => Release -> [PackageIndex] -> m [[BinaryPackage]]
-- packageLists release indexes = mapM (packages release) indexes

-- packages :: MonadRepos m => Release -> PackageIndex -> m [BinaryPackage]
-- packages release index = liftIO (binaryPackagesOfIndex release index) >>= return . either throw id

uri = "http://deb.seereason.com/ubuntu/"
arch = Binary (ArchOS "linux") (ArchCPU "i386")