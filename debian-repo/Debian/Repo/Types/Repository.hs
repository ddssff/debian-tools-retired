{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repo(..)
    , RepoKey(..)
    , repoURI
    , libraryCompatibilityLevel
    , compatibilityFile
    , Repository(..)
    , LocalRepository(..)
    , Layout(..)
    ) where

import Control.Exception ( throw )
import Data.Char ( isDigit )
import Data.Maybe ( fromJust )
import Data.Text (unpack)
import Debian.URI ( URI'(..), URI(uriPath), fileFromURI, parseURI )
import qualified Debian.UTF8 as Deb
import Debian.Repo.Types.EnvPath (EnvPath(..))
import Debian.Repo.Types.Release (Release)
import System.FilePath ( (</>) )

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
--data Repository = forall a. (Repo a) => Repository a
data Repository
    = LocalRepo LocalRepository
    | VerifiedRepo URI' [Release]
    | UnverifiedRepo URI'
    deriving (Show, Read)

data RepoKey
    = Remote URI
    | Local EnvPath
      deriving (Show, Eq, Ord)

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

data LocalRepository
    = LocalRepository
      { repoRoot :: EnvPath
      , repoLayout :: (Maybe Layout)
      , repoReleaseInfoLocal :: [Release]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show)

instance Repo Repository where
    repoKey (LocalRepo (LocalRepository path _ _)) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoKey (VerifiedRepo (URI' uri) _) = Remote uri
    repoKey (UnverifiedRepo (URI' uri)) = Remote uri
    repoReleaseInfo (LocalRepo (LocalRepository _ _ info)) = info
    repoReleaseInfo (VerifiedRepo _ info) = info
    repoReleaseInfo (UnverifiedRepo _uri) = error "No release info for unverified repository"

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

class (Ord t, Eq t) => Repo t where
    repoKey :: t -> RepoKey
    repositoryCompatibilityLevel :: t -> IO (Maybe Int)
    repositoryCompatibilityLevel r =
        fileFromURI uri' >>= either throw (return . parse . unpack . Deb.decode)
        where
          uri' = uri {uriPath = uriPath uri </> compatibilityFile}
          uri = case repoKey r of
                  Remote x -> x
                  Local x -> fromJust . parseURI $ "file://" ++ envPath x
          parse :: String -> Maybe Int
          parse s = case takeWhile isDigit s of
                         "" -> Nothing
                         s' -> Just . read $ s'
    -- | This method returns a list of all the release in the
    -- repository.  This can be used to identify all of the files
    -- in the repository that are not garbage.
    repoReleaseInfo :: t -> [Release]
    checkCompatibility :: t -> IO ()
    checkCompatibility repo =
        do level <- repositoryCompatibilityLevel repo
           case level of
             Nothing -> return ()
             Just n | n >= libraryCompatibilityLevel -> return ()
             Just n -> error ("Compatibility error: repository level " ++ show n ++
                              " < library level " ++ show libraryCompatibilityLevel ++ ", please upgrade.")

repoURI :: Repo r => r -> URI
repoURI r =
    case repoKey r of
      Local path -> fromJust . parseURI $ "file://" ++ envPath path
      Remote uri -> uri

-- |The name of the file which holds the repository's compatibility
-- level.
compatibilityFile :: FilePath
compatibilityFile = "repository-compat"

-- | The compatibility level of this library and any applications
-- which use it.  It is an error if we try to use a repository whose
-- compatibility level is higher than this, a newer version of the
-- library must be used.  This value was increased from 1 to 2 due
-- to a new version number tagging policy.
libraryCompatibilityLevel :: Int
libraryCompatibilityLevel = 2
