{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad
import Control.Monad.Trans
import Data.List (sort, nub)
import Data.Maybe (catMaybes)
--import Debian.AutoBuilder.BuildTarget (Download(..))
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Relation (SrcPkgName)
import Debian.Repo
import Debian.Sources
import Debian.Version (parseDebianVersion, prettyDebianVersion)
import System.Unix.Directory

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

prepare :: P.CacheRec -> P.Packages -> String -> SrcPkgName -> AptIOT IO Download
prepare cache target dist package =
    do os <- prepareAptEnv (P.topDir cache) (P.ifSourcesChanged (P.params cache)) distro
       when (P.flushSource (P.params cache)) (liftIO . removeRecursiveSafely $ aptDir os package)
       tree <- lift $ Debian.Repo.aptGetSource (aptDir os package) os package version'
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
