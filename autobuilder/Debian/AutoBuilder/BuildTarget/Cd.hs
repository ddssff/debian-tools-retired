{-# LANGUAGE ExistentialQuantification #-}
-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Monad (AptIOT)
import System.FilePath ((</>))

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

prepare :: P.CacheRec -> P.Packages -> FilePath -> Download -> AptIOT IO Download
prepare _cache package subdir target =
    do     
    return $ Download { package = package
                        , getTop = getTop target </> subdir
                        , logText = logText target ++ " (in subdirectory " ++ subdir ++ ")"
                        , mVersion = Nothing
                        , origTarball = Nothing
                        , cleanTarget = cleanTarget target
                        , buildWrapper = id
                        } 
