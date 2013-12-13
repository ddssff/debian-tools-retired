{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.AptImage
    ( AptCache(..)
    , AptBuildCache(..)
    , AptImage(..)
    ) where

import Debian.Arch (Arch(..))
import Debian.Release (ReleaseName(..))

import Debian.Repo.Types.EnvPath (EnvRoot)
import Debian.Repo.Types.PackageIndex (SourcePackage, BinaryPackage)
import Debian.Repo.Types.Slice (SliceList)

{-
instance Show FileStatus where
    show _ = "def :: FileStatus"
-}
---------------------- CACHED OS IMAGE ---------------------

class (Ord t, Eq t, Show t) => AptCache t where
    globalCacheDir :: t -> FilePath
    -- | The directory you might chroot to.
    rootDir :: t -> EnvRoot
    -- | The sources.list without the local repository
    aptBaseSliceList :: t -> SliceList
    -- | The build architecture
    aptArch :: t -> Arch
    -- | Return the all source packages in this AptCache.
    aptSourcePackages :: t -> [SourcePackage]
    -- | Return the all binary packages for the architecture of this AptCache.
    aptBinaryPackages :: t -> [BinaryPackage]
    -- | Name of release
    aptReleaseName :: t -> ReleaseName

class AptCache t => AptBuildCache t where
    -- | The sources.list
    aptSliceList :: t -> SliceList

data AptImage =
    AptImage { aptGlobalCacheDir :: FilePath
             , aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSliceList :: SliceList
             , aptImageReleaseName :: ReleaseName
             , aptImageSourcePackages :: [SourcePackage]
             , aptImageBinaryPackages :: [BinaryPackage]
             }
