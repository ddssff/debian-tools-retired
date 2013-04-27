{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Release
    ( Release(..)
    ) where

import Debian.Arch (Arch(..))
import Debian.Release (Section(..), ReleaseName(..))

-- FIXME: The lists here should be sets so that == and compare work properly.
data Release = Release { releaseName :: ReleaseName
                       , releaseAliases :: [ReleaseName]
                       , releaseArchitectures :: [Arch]
                       , releaseComponents :: [Section]
                       } deriving (Eq, Ord, Read, Show)
