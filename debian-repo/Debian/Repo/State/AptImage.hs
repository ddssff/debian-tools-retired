{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Debian.Repo.State.AptImage
    ( withAptImage
    , aptSourcePackages
    , aptBinaryPackages
    , prepareSource
    ) where

import Control.Applicative ((<$>))
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadIO(..), MonadTrans(lift))
import Data.Lens.Lazy (getL)
import Data.Maybe (listToMaybe)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Relation (SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.AptImage (aptDir, aptGetSource, aptGetUpdate, AptImage, aptImageArch, aptImageRoot, aptImageSources, cacheRootDir, createAptImage, MonadApt(..))
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.OSImage (OSImage)
import Debian.Repo.PackageID (PackageID(packageName), PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Prelude (symbol)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), SliceList, SourcesChangedAction)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), DebianSourceTreeC(entry), findDebianBuildTrees, SourceTree(dir'))
import Debian.Repo.State (AptKey, evalMonadApt, findAptKey, MonadRepos, putAptImage)
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (updateCacheSources)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Process.Progress (qPutStr, qPutStrLn, quieter)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance MonadApt m => MonadApt (StateT OSImage m) where
    getApt = lift getApt
    putApt = lift . putApt

withAptImage :: (MonadRepos m, MonadTop m) => SourcesChangedAction -> NamedSliceList -> StateT AptImage m a -> m a
withAptImage sourcesChangedAction sources action = prepareAptImage sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage :: (MonadTop m, MonadRepos m) =>
                 SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptKey		-- The resulting environment
prepareAptImage sourcesChangedAction sources = do
  quieter 1 $ qPutStrLn ("Preparing apt-get environment for " ++ show (relName (sliceListName sources)))
  root <- cacheRootDir (sliceListName sources)
  mkey <- findAptKey root
  maybe (prepareAptImage' sourcesChangedAction sources) return mkey

prepareAptImage' :: (MonadTop m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptKey
prepareAptImage' sourcesChangedAction sources =
    cacheRootDir (sliceListName sources) >>= \ root ->
    findAptKey root >>=
    maybe (createAptImage sources >>= putAptImage >>= \ key ->
           evalMonadApt (updateCacheSources sourcesChangedAction sources >> updateAptEnv) key >>
           return key) return

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
updateAptEnv :: (MonadRepos m, MonadApt m) => m ()
updateAptEnv = aptGetUpdate

aptSourcePackages :: (MonadRepos m, MonadApt m) => m [SourcePackage]
aptSourcePackages =
    do root <- getL aptImageRoot <$> getApt
       arch <- getL aptImageArch <$> getApt
       sources <- getL aptImageSources <$> getApt
       -- quieter 1 $ qPutStrLn ($(symbol 'aptSourcePackages) ++ " " ++ show (pretty (sliceListName sources)))
       sourcePackagesFromSources root arch (sliceList sources)

aptBinaryPackages :: (MonadRepos m, MonadApt m) => m [BinaryPackage]
aptBinaryPackages =
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

-- |Retrieve a source package via apt-get.
prepareSource :: (MonadRepos m, MonadApt m, MonadTop m, MonadIO m) =>
                 SrcPkgName			-- The name of the package
              -> Maybe DebianVersion		-- The desired version, if Nothing get newest
              -> m DebianBuildTree		-- The resulting source tree
prepareSource package version =
    do root <- (rootPath . getL aptImageRoot) <$> getApt
       dir <- aptDir package
       liftIO $ createDirectoryIfMissing True dir
       ready <- liftIO $ findDebianBuildTrees dir
       version' <- latestVersion package version
       case (version', ready) of
         (Nothing, _) ->
             fail $ "No available versions of " ++ unSrcPkgName package ++ " in " ++ root
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed in " ++ dir ++ " (1): trees=" ++ show (map (dir' . tree' . debTree') trees)
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                qPutStr $ "Retrieving APT source for " ++ unSrcPkgName package
                aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed (2): trees=" ++ show (map (dir' . tree' . debTree') trees)

latestVersion :: (MonadRepos m, MonadApt m) => SrcPkgName -> Maybe DebianVersion -> m (Maybe DebianVersion)
latestVersion package version = do
  pkgs <- aptSourcePackages
  let newest = (listToMaybe . map (packageVersion . sourcePackageID) . filter ((== package) . packageName . sourcePackageID)) $ pkgs
  return $ maybe newest Just version
