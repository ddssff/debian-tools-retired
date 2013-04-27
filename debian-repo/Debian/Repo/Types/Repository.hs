{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository(..)
    ) where

import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.LocalRepository (LocalRepository(..))
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
