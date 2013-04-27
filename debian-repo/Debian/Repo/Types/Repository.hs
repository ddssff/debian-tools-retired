{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository(..)
    , LocalRepository(..)
    , Layout(..)
    ) where

import Debian.Repo.Types.EnvPath (EnvPath(..))
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (Repo(..), RepoKey(..))
import Debian.URI (URI')

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
--data Repository = forall a. (Repo a) => Repository a
data Repository
    = LocalRepo LocalRepository
    | VerifiedRepo URI' [Release]
    | UnverifiedRepo URI'
    deriving (Show, Read)

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

instance Repo Repository where
    repoKey (LocalRepo (LocalRepository path _ _)) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoKey (VerifiedRepo uri _) = Remote uri
    repoKey (UnverifiedRepo uri) = Remote uri
    repoReleaseInfo (LocalRepo (LocalRepository _ _ info)) = info
    repoReleaseInfo (VerifiedRepo _ info) = info
    repoReleaseInfo (UnverifiedRepo _uri) = error "No release info for unverified repository"

data LocalRepository
    = LocalRepository
      { repoRoot :: EnvPath
      , repoLayout :: (Maybe Layout)
      , repoReleaseInfoLocal :: [Release]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show)

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info
