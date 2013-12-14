{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository(LocalRepo, RemoteRepo)
    , fromLocalRepository
    , unLocalRepository
    , unRemoteRepository
    ) where

import qualified Debian.Repo.Pretty as F (Pretty(..))
import Debian.Repo.Types.EnvPath (outsidePath)
import Debian.Repo.Types.LocalRepository (LocalRepository, repoRoot)
import Debian.Repo.Types.RemoteRepository (RemoteRepository(..))
import Debian.Repo.Types.Repo (Repo(repoKey, repoReleaseInfo))
import Text.PrettyPrint.ANSI.Leijen (text)

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
data Repository
    = LocalRepo LocalRepository
    | RemoteRepo RemoteRepository
    deriving (Show, Read)

unRemoteRepository :: Repository -> Maybe RemoteRepository
unRemoteRepository (RemoteRepo x) = Just x
unRemoteRepository _ = Nothing

unLocalRepository :: Repository -> Maybe LocalRepository
unLocalRepository (LocalRepo x) = Just x
unLocalRepository _ = Nothing

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

instance F.Pretty Repository where
    pretty (LocalRepo r) = text $ outsidePath (repoRoot r)
    pretty (RemoteRepo r) = F.pretty r

instance Repo Repository where
    repoKey (LocalRepo r) = repoKey r
    repoKey (RemoteRepo r) = repoKey r
    repoReleaseInfo (LocalRepo r) = repoReleaseInfo r
    repoReleaseInfo (RemoteRepo r) = repoReleaseInfo r

fromLocalRepository :: LocalRepository -> Repository
fromLocalRepository = LocalRepo
