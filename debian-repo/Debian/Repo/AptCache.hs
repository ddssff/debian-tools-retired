{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptCache
    ( MonadCache(aptBaseSources)
{-
    , aptReleaseName
-}
    , distDir
    , sourcesPath
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(..))
import Debian.Repo.Slice (NamedSliceList(sliceListName), SliceList)
import Debian.Repo.Top (askTop, MonadTop, dists)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import System.Unix.Chroot (useEnv)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance NFData ExitCode

-- | The AptCache class abstracts the basic properties of an apt-get
-- environment.  This represents some of the properties of an OSImage,
-- a complete build environment.  It is enough to run apt-get, and
-- thus obtain repository info and download source code packages from
-- a remote repository.
class (Monad m, Functor m) => MonadCache m where
    aptBaseSources :: m NamedSliceList
    -- ^ The sources.list excluding lines for the local repository

aptReleaseName :: MonadCache m => m ReleaseName
aptReleaseName = sliceListName <$> aptBaseSources

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

-- | The directory in a repository where the package index files for a
-- particular dist or release is stored.  (Wait, that's not right.)
distDir :: MonadTop m => ReleaseName -> m FilePath
distDir rel = (</> relName rel) <$> dists

-- | The path of the text file containing the sources.list (aka SliceList)
sourcesPath :: MonadTop m => ReleaseName -> m FilePath
sourcesPath rel = (</> "sources") <$> distDir rel

{-
data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"
-}
