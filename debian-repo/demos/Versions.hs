{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TupleSections #-}
-- |Print the available version numbers of a package.
module Main where

import Control.Monad.Trans
import Data.Maybe
import Debian.Arch (Arch(Binary), ArchCPU(..), ArchOS(..))
import Debian.Repo.Monads.Apt (MonadApt, runAptT)
--import Debian.Repo.PackageIndex
import Debian.Repo.Release
import Debian.Repo.Repository
import Debian.Repo.Types
import Debian.Repo.Types.Repo (repoReleaseInfo)
import Debian.URI

main :: IO ()
main = runAptT main'

main' :: MonadApt m => m ()
main' =
    do repo <- prepareRepository (fromJust (parseURI uri))
       releases <- mapM insertRelease (map (repo,) (repoReleaseInfo repo))
       -- let binaryIndexes = map (filter (\ i -> packageIndexArch i == arch)) (map binaryIndexList releases)
       -- _binaryPackages <- mapM (packageLists release) binaryIndexes
       liftIO (putStrLn ("\n" ++ show releases))
{-
    where
      insert repo info = insertRelease repo 
-}

-- packageLists :: MonadApt m => Release -> [PackageIndex] -> m [[BinaryPackage]]
-- packageLists release indexes = mapM (packages release) indexes

-- packages :: MonadApt m => Release -> PackageIndex -> m [BinaryPackage]
-- packages release index = liftIO (binaryPackagesOfIndex release index) >>= return . either throw id

uri = "http://deb.seereason.com/ubuntu/"
arch = Binary (ArchOS "linux") (ArchCPU "i386")