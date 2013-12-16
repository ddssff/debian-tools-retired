{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( AptCache(..)
    , AptBuildCache(..)
    , AptImage(..)
    ) where

import Debian.Arch (Arch(..))
import Debian.Release (ReleaseName(..))
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Slice (SliceList)

-- | The AptCache class abstracts the basic properties of an apt-get
-- environment.  This represents some of the properties of an OSImage,
-- a complete build environment.  It is enough to run apt-get, and
-- thus obtain repository info and download source code packages from
-- a remote repository.
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

-- | The AptImage object is an instance of AptCache.
data AptImage =
    AptImage { aptGlobalCacheDir :: FilePath
             , aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSliceList :: SliceList
             , aptImageReleaseName :: ReleaseName
             , aptImageSourcePackages :: [SourcePackage]
             , aptImageBinaryPackages :: [BinaryPackage]
             }

instance Show AptImage where
    show apt = "AptImage " ++ relName (aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = aptGlobalCacheDir
    rootDir = aptImageRoot
    aptArch = aptImageArch
    aptBaseSliceList = aptImageSliceList
    aptSourcePackages = aptImageSourcePackages
    aptBinaryPackages = aptImageBinaryPackages
    aptReleaseName = aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (aptImageReleaseName a) (aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ
