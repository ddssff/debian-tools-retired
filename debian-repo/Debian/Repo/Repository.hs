{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Repository
    ( Repository(LocalRepo, RemoteRepo)
    , fromLocalRepository
    , unLocalRepository
    , unRemoteRepository
    ) where

import Debian.Repo.EnvPath (outsidePath)
import Debian.Repo.LocalRepository (LocalRepository, repoRoot)
import qualified Debian.Repo.Pretty as F (Pretty(..))
import Debian.Repo.RemoteRepository (RemoteRepository(..))
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo))
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

{-
showIndexBrief :: (Repository, Release) -> PackageIndex -> String
showIndexBrief release index =
    (releaseName' . releaseName . snd $ release) </> sectionName' (packageIndexComponent index) </> showArch (packageIndexArch index)
    where showArch Source = "source"
          showArch All = "all"
          showArch x@(Binary _ _) = "binary-" ++ show (prettyArch x)

debSourceFromIndex :: (Repository, Release) -> PackageIndex -> DebSource
debSourceFromIndex (repo, release) index =
    DebSource {sourceType = typ,
               sourceUri = repoURI repo,
               sourceDist = Right (dist, components)}
    where
      typ = case arch of (Binary _ _) -> Deb; Source -> DebSrc; All -> Deb
      arch = packageIndexArch index
      dist = releaseName $ release
      components = releaseComponents $ release
-}
