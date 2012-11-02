-- |Print the available version numbers of a package.
module Main where

import Control.Exception (throw)
import Control.Monad.Trans
import Data.Maybe
import Debian.Release (Arch(Binary))
import Debian.Repo.Monad
import Debian.Repo.PackageIndex
import Debian.Repo.Package
import Debian.Repo.Release
import Debian.Repo.Repository
import Debian.Repo.Types
import Debian.URI

main :: IO ()
main = runAptIO main'

main' :: AptIOT IO ()
main' =
    do repo <- prepareRepository (fromJust (parseURI uri))
       releases <- mapM insertRelease (map (Release repo) (repoReleaseInfo repo))
       let binaryIndexes = map (filter (\ i -> packageIndexArch i == arch)) (map binaryIndexList releases)
       _binaryPackages <- mapM packageLists binaryIndexes
       liftIO (putStrLn ("\n" ++ show releases))
{-
    where
      insert repo info = insertRelease repo 
-}

packageLists :: [PackageIndex] -> AptIOT IO [[BinaryPackage]]
packageLists indexes = mapM packages indexes

packages :: PackageIndex -> AptIOT IO [BinaryPackage]
packages index = liftIO (binaryPackagesOfIndex index) >>= return . either throw id

uri = "http://deb.seereason.com/ubuntu/"
arch = Binary "i386"