{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repo
    ( Repo(..)
    , RepoKey(..)
    , repoURI
    , libraryCompatibilityLevel
    , compatibilityFile
    ) where

import Control.Exception ( throw )
import Data.Char ( isDigit )
import Data.Maybe ( fromJust )
import Data.Text (unpack)
import Debian.URI ( URI(uriPath), fileFromURI, parseURI )
import qualified Debian.UTF8 as Deb
import Debian.Repo.Types.EnvPath (EnvPath(..))
import Debian.Repo.Types.Release (Release)
import Debian.URI (URI'(URI'))
import System.FilePath ( (</>) )

deriving instance Eq URI'
deriving instance Ord URI'

data RepoKey
    = Remote URI'
    | Local EnvPath
      deriving (Read, Show, Eq, Ord)

class (Ord t, Eq t) => Repo t where
    repoKey :: t -> RepoKey
    repositoryCompatibilityLevel :: t -> IO (Maybe Int)
    repositoryCompatibilityLevel r =
        fileFromURI uri' >>= either throw (return . parse . unpack . Deb.decode)
        where
          uri' = uri {uriPath = uriPath uri </> compatibilityFile}
          uri = case repoKey r of
                  Remote (URI' x) -> x
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

repoURI :: Repo r => r -> URI
repoURI r =
    case repoKey r of
      Local path -> fromJust . parseURI $ "file://" ++ envPath path
      Remote (URI' uri) -> uri