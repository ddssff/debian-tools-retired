{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(allSources, params))
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Packages as P (PackageFlag(AptPin), Packages(flags, spec))
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(flushSource, ifSourcesChanged))
import Debian.Relation (SrcPkgName)
import Debian.Repo.Apt (MonadDeb)
import Debian.Repo.Apt.AptImage (aptGetSource, prepareAptEnv)
import Debian.Repo.AptImage (aptDir)
import Debian.Repo.Slice (NamedSliceList(sliceListName))
import Debian.Repo.SourceTree (topdir)
import Debian.Repo.Top (askTop)
import Debian.Sources (SliceName(SliceName, sliceName))
import Debian.Version (parseDebianVersion, prettyDebianVersion)
import System.Unix.Directory (removeRecursiveSafely)

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

prepare :: MonadDeb m => P.CacheRec -> P.Packages -> String -> SrcPkgName -> m Download
prepare cache target dist package =
    do top <- askTop
       os <- prepareAptEnv top (P.ifSourcesChanged (P.params cache)) distro
       when (P.flushSource (P.params cache)) (liftIO . removeRecursiveSafely $ aptDir os package)
       tree <- liftIO $ aptGetSource (aptDir os package) os package version'
       return $ Download {
                    package = target
                  , getTop = topdir tree
                  , logText = "Built from " ++ sliceName (sliceListName distro) ++ " apt pool, apt-revision: " ++ show (P.spec target)
                  , mVersion = Nothing
                  , origTarball = Nothing
                  , cleanTarget = \ _ -> return ([], 0)
                  , buildWrapper = id }
    where
      distro = maybe (error $ "Invalid dist: " ++ sliceName dist') id (findRelease (P.allSources cache) dist')
      dist' = SliceName dist
      version' = case (nub (sort (catMaybes (map (\ flag -> case flag of
                                                              P.AptPin s -> Just (parseDebianVersion s)
                                                              _ -> Nothing) (P.flags target))))) of
                   [] -> Nothing
                   [v] -> Just v
                   vs -> error ("Conflicting pin versions for apt-get: " ++ show (map prettyDebianVersion vs))
      findRelease distros dist =
          case filter ((== dist) . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ sliceName dist ++ ": " ++ show (map (sliceName . sliceListName) a))
