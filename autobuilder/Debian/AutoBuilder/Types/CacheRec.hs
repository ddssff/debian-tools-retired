module Debian.AutoBuilder.Types.CacheRec
    ( CacheRec(..)
    ) where

import Debian.AutoBuilder.Types.Packages (Packages)
import Debian.AutoBuilder.Types.ParamRec (ParamRec)
import Debian.Repo.Types ( SliceList, NamedSliceList )

data CacheRec
    = CacheRec
    { params :: ParamRec
    , topDir :: FilePath
    , allSources :: [NamedSliceList]
    , buildRepoSources :: SliceList
    , packages :: Packages
    }
