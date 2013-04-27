{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository(..)
    , LocalRepository(repoRoot, repoLayout, repoReleaseInfoLocal)
    , Layout(..)
    , MonadRepoCache(..)
    , makeReleaseInfo
    , readLocalRepo
    , parseComponents
    , parseArchitectures
    ) where

import Control.Applicative.Error (Failing(Success, Failure))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (groupBy, partition, sort)
import Data.Map (Map, insertWith)
import Data.Text (Text, unpack)
import Debian.Arch (Arch, parseArch)
import qualified Debian.Control.Text as T (Paragraph, ControlFunctions(parseControl), fieldValue, Control'(Control))
import Debian.Release (ReleaseName, releaseName', parseReleaseName, Section, parseSection')
import Debian.Repo.Types.EnvPath (EnvPath(..), outsidePath)
import Debian.Repo.Types.Release (Release(Release, releaseName, releaseAliases, releaseArchitectures, releaseComponents))
import Debian.Repo.Types.Repo (Repo(..), RepoKey(..))
import Debian.URI (URI')
import Extra.List ( partitionM )
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import qualified System.Posix.Files as F (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Text.Regex (mkRegex, splitRegex)
import qualified Tmp.File as F ( File(..), readFile )

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

class MonadIO m => MonadRepoCache m where
    getRepoCache :: m (Map RepoKey Repository)
    putRepoCache :: Map RepoKey Repository -> m ()

readLocalRepo :: MonadRepoCache m => EnvPath -> Maybe Layout -> m LocalRepository
readLocalRepo root layout =
    do
      state <- getRepoCache
      names <- liftIO (getDirectoryContents distDir) >>=
               return . filter (\ x -> not . elem x $ [".", ".."])
      (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
      linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
      let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
      let distGroups = groupBy fstEq . sort $ aliasPairs
      let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
      releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
      let repo = LocalRepository { repoRoot = root
                                 , repoLayout = layout
                                 , repoReleaseInfoLocal = releaseInfo }
      putRepoCache (insertWith (\ _ x -> x) (repoKey repo) (LocalRepo repo) state)
      return repo
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (ReleaseName, [ReleaseName]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist ++ "/Release"
      distDir = outsidePath root ++ "/dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (F.readFile path) >>= return . parseRelease dist aliases
{-
    do text <- liftIO (B.readFile path)
       return $ parseRelease path text dist aliases
-}

parseRelease :: ReleaseName -> [ReleaseName] -> F.File Text -> Release
parseRelease name aliases file =
    case F.text file of
      Failure msgs -> error $ "Could not read " ++ show (F.path file) ++ ": " ++ show msgs
      Success text ->
          case T.parseControl (show (F.path file)) text of
            Left msg -> error $ "Failure parsing " ++ show (F.path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (F.path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (F.File {F.path = F.path file, F.text = Success info}) name aliases

makeReleaseInfo :: F.File T.Paragraph -> ReleaseName -> [ReleaseName] -> Release
makeReleaseInfo file@(F.File {F.text = Failure msgs}) _name _aliases =
    error $ "Failure reading " ++ show (F.path file) ++ ": " ++ show msgs
makeReleaseInfo file@(F.File {F.text = Success info}) name aliases =
    case (T.fieldValue "Architectures" info, T.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          Release { releaseName = name
                  , releaseAliases = aliases
                  , releaseArchitectures = parseArchitectures archList
                  , releaseComponents = parseComponents compList }
      _ -> error $ "Missing Architectures or Components field in Release file " ++ show (F.path file)

parseArchitectures :: Text -> [Arch]
parseArchitectures archList =
    map parseArch . splitRegex re . unpack $ archList
    where
      re = mkRegex "[ ,]+"

parseComponents :: Text -> [Section]
parseComponents compList =
    map parseSection' . splitRegex re . unpack  $ compList
    where
      re = mkRegex "[ ,]+"
