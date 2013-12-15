{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Release
    ( Release(Release, releaseName, releaseAliases, releaseArchitectures, releaseComponents)
    , parseComponents
    , parseArchitectures
    , parseReleaseFile
    , getReleaseInfoRemote
    ) where

import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (ErrorCall(ErrorCall), Exception(toException))
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Debian.Arch (Arch(..), parseArch)
import qualified Debian.Control.Text as T (Control'(Control), fieldValue, Paragraph, Paragraph', parseControl)
import Debian.Release (parseReleaseName, parseSection', ReleaseName(..), releaseName', Section(..))
import Debian.URI (dirFromURI, fileFromURI, URI(uriPath), uriToString')
import Debian.UTF8 as Deb (decode)
import Prelude hiding (readFile)
import System.FilePath ((</>))
import System.Process.Progress (qPutStr, quieter)
import Text.Regex (mkRegex, splitRegex)

import Control.Applicative
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Source = LocalPath FilePath | RemotePath URI

-- |A file whose contents have been read into memory.
data File a = File { path :: Source, text :: Failing a }

readFile :: FilePath -> IO (File T.Text)
readFile x = File <$> return (LocalPath x) <*> (try (T.readFile x) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success)

instance Show Source where
    show (LocalPath p) = p
    show (RemotePath uri) = show uri

-- FIXME: The lists here should be sets so that == and compare work properly.
data Release = Release { releaseName :: ReleaseName
                       , releaseAliases :: [ReleaseName]
                       , releaseArchitectures :: [Arch]
                       , releaseComponents :: [Section]
                       } deriving (Eq, Ord, Read, Show)

makeReleaseInfo :: File T.Paragraph -> ReleaseName -> [ReleaseName] -> Release
makeReleaseInfo file@(File {text = Failure msgs}) _name _aliases =
    error $ "Failure reading " ++ show (path file) ++ ": " ++ show msgs
makeReleaseInfo file@(File {text = Success info}) name aliases =
    case (T.fieldValue "Architectures" info, T.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          Release { releaseName = name
                  , releaseAliases = aliases
                  , releaseArchitectures = parseArchitectures archList
                  , releaseComponents = parseComponents compList }
      _ -> error $ "Missing Architectures or Components field in Release file " ++ show (path file)

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

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (readFile path) >>= return . parseRelease dist aliases

parseRelease :: ReleaseName -> [ReleaseName] -> File Text -> Release
parseRelease name aliases file =
    case text file of
      Failure msgs -> error $ "Could not read " ++ show (path file) ++ ": " ++ show msgs
      Success t ->
          case T.parseControl (show (path file)) t of
            Left msg -> error $ "Failure parsing " ++ show (path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (File {path = path file, text = Success info}) name aliases

-- |Get the list of releases of a remote repository.
getReleaseInfoRemote :: URI -> IO [Release]
getReleaseInfoRemote uri =
    qPutStr ("(verifying " ++ uriToString' uri ++ ".") >>
    quieter 2 (dirFromURI distsURI) >>=
    quieter 2 . either (error . show) verify >>=
    return . catMaybes >>= 
    (\ result -> qPutStr ")\n" >> return result)
    where
      distsURI = uri {uriPath = uriPath uri </> "dists/"}
      verify names =
          do let dists = map parseReleaseName names
             (releaseFiles :: [File (T.Paragraph' Text)]) <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap T.unpack (T.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe Text -> (File T.Paragraph) -> ReleaseName -> Maybe Release
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (T.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo info (parseReleaseName (T.unpack dist)) []
      getSuite :: File (T.Paragraph' Text) -> Maybe Text
      getSuite (File {text = Success releaseFile}) = T.fieldValue (releaseNameField releaseFile) releaseFile
      getSuite (File {text = Failure msgs}) = fail (intercalate "\n" msgs)
      getReleaseFile :: ReleaseName -> IO (File (T.Paragraph' Text))
      getReleaseFile distName =
          do qPutStr "."
             release <- fileFromURI releaseURI
             let control = either Left (either (Left . toException . ErrorCall . show) Right . T.parseControl (show releaseURI) . Deb.decode) release
             case control of
               Right (T.Control [info :: T.Paragraph' Text]) -> return $ File {path = RemotePath releaseURI, text = Success info}
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c
