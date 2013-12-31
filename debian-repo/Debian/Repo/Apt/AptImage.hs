{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Debian.Repo.Apt.AptImage
    ( evalMonadApt
    , execMonadApt
    , runMonadApt
    , withAptImage
    ) where

import Control.Applicative ((<$>))
import Control.Monad.State (runStateT, StateT)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Function (on)
import Data.Lens.Lazy (getL, modL, setL)
import Data.List (sortBy)
import Data.Map as Map (insert, lookup)
import Debian.Release (ReleaseName(relName))
import Debian.Repo.Apt.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.Apt.Slice (updateCacheSources)
import Debian.Repo.AptImage (aptGetUpdate, AptImage, aptImageArch, aptImageBinaryPackages, aptImageRoot, aptImageSourcePackages, aptImageSources, cacheRootDir, createAptImage, getApt, modifyApt, MonadApt(putApt, getApt))
import Debian.Repo.OSImage (OSImage)
import Debian.Repo.PackageID (PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Repos (aptImageMap, findAptImage, modifyRepos, MonadRepos(getRepos))
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), SliceList, SourcesChangedAction)
import Debian.Repo.Top (MonadTop)
import System.Process.Progress (qPutStrLn, quieter)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance MonadApt m => MonadApt (StateT OSImage m) where
    getApt = lift getApt
    putApt = lift . putApt

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m a
evalMonadApt task apt = fst <$> runMonadApt task apt

-- | Run MonadOS and update the osImageMap with the modified value
execMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m AptImage
execMonadApt task apt = snd <$> runMonadApt task apt

-- | Run MonadOS and update the osImageMap with the modified value
runMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m (a, AptImage)
runMonadApt task apt = do
  (a, apt') <- runStateT task apt
  modifyRepos (modL aptImageMap (Map.insert (getL aptImageRoot apt') apt'))
  return (a, apt')

withAptImage :: (MonadRepos m, MonadTop m) => SourcesChangedAction -> NamedSliceList -> StateT AptImage m a -> m a
withAptImage sourcesChangedAction sources action = prepareAptImage sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage :: (MonadTop m, MonadRepos m) =>
                 SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptImage sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (relName (sliceListName sources))) >> quieter 2 x) $
    cacheRootDir (sliceListName sources) >>= \ root ->
    getRepos >>= return . Map.lookup root . getL aptImageMap >>=
    maybe (prepareAptImage' sourcesChangedAction sources) return

prepareAptImage' :: (MonadTop m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptImage' sourcesChangedAction sources =
    cacheRootDir (sliceListName sources) >>= \ root ->
    findAptImage root >>=
    maybe (createAptImage sources >>= execMonadApt (updateCacheSources sourcesChangedAction sources >> updateAptEnv)) return

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
updateAptEnv :: (MonadRepos m, MonadApt m) => m ()
updateAptEnv =
    do aptGetUpdate
       sourcePackages <- getSourcePackages >>= return . sortBy cmp
       binaryPackages <- getBinaryPackages
       modifyApt (setL aptImageSourcePackages sourcePackages)
       modifyApt (setL aptImageBinaryPackages binaryPackages)
    where
      -- Flip args to get newest version first
      cmp = flip (compare `on` (packageVersion . sourcePackageID))
{-
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

    putStrLn ("> " ++ cmd) >> system cmd >>= \ code ->
    case code of
      ExitSuccess -> return ()
      ExitFailure n -> error $ cmd ++ " -> ExitFailure " ++ show n
-}

getSourcePackages :: (MonadRepos m, MonadApt m) => m [SourcePackage]
getSourcePackages =
    do qPutStrLn "AptImage.getSourcePackages"
       root <- getL aptImageRoot <$> getApt
       arch <- getL aptImageArch <$> getApt
       sources <- (sliceList . getL aptImageSources) <$> getApt
       sourcePackagesFromSources root arch sources

getBinaryPackages :: (MonadRepos m, MonadApt m) => m [BinaryPackage]
getBinaryPackages =
    do qPutStrLn "AptImage.getBinaryPackages"
       root <- getL aptImageRoot <$> getApt
       arch <- getL aptImageArch <$> getApt
       sources <- (sliceList . getL aptImageSources) <$> getApt
       binaryPackagesFromSources root arch sources

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"
