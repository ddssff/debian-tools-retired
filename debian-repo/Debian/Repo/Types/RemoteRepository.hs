{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.RemoteRepository
    ( RemoteRepository(..)
    , repoReleaseNames
    ) where

import Debian.Release (ReleaseName)
import qualified Debian.Repo.Pretty as F (Pretty(..))
import Debian.Repo.Types.Release (Release(releaseName))
import Debian.Repo.Types.Repo (Repo(repoKey, repoReleaseInfo), RepoKey(Remote))
import Debian.URI (fromURI', URI')
import Text.PrettyPrint.ANSI.Leijen (text)

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show, Eq, Ord)

instance Repo RemoteRepository where
    repoKey (RemoteRepository uri _) = Remote uri
    repoReleaseInfo (RemoteRepository _ info) = info

-- | URI has a bogus show function, which we are using here.
instance F.Pretty URI' where
    pretty = text . show . fromURI'

instance F.Pretty RemoteRepository where
    pretty (RemoteRepository s _) = F.pretty s

-- Nice code to do caching, but I figured out how to fix the old code.

repoReleaseNames :: RemoteRepository -> [ReleaseName]
repoReleaseNames (RemoteRepository _ rels) = map releaseName rels
