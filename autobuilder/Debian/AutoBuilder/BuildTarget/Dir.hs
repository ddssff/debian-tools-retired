module Debian.AutoBuilder.BuildTarget.Dir where

import Control.Monad.Trans (liftIO)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo

-- |Dir is a simple instance of BuildTarget representing building the
-- debian source in a local directory.  This type of target is used
-- for testing, and is also returned by the clean method when the
-- source control information has been stripped out of some other type
-- of BuildTarget.
prepare :: MonadApt e m => P.CacheRec -> P.Packages -> FilePath -> m T.Download
prepare _cache package path =
    do tree <- liftIO (findSourceTree path)
       return $ T.Download { T.package = package
                           , T.getTop = topdir tree
                           , T.logText =  "Built from local directory " ++ show (P.spec package)
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id
                           }
