{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Monads.Cache
    ( MonadRepoCache(getRepoCache, putRepoCache)
    , loadRepoCache
    , saveRepoCache
    , prepareRemoteRepository
    , prepareRepository
    ) where

import Control.Applicative.Error (Failing(Success, Failure), maybeRead)
import Control.Exception (ErrorCall(..), SomeException, toException)
import Control.Monad (filterM, when, unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (groupBy, partition, sort, isPrefixOf, intercalate)
import Data.Map as Map (Map, insertWith, lookup, insert, fromList, toList, union, empty)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangesFile(changeInfo), ChangedFileSpec(changedFileSection))
import qualified Debian.Control.Text as T (ControlFunctions(parseControl), Control'(Control), fieldValue, Paragraph, Paragraph')
import Debian.Release (ReleaseName(..), releaseName', Section, sectionName', parseReleaseName, SubSection(section))
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.Types.LocalRepository (prepareLocalRepository)
import Debian.Repo.Types.Release (Release(releaseName), makeReleaseInfo)
import Debian.Repo.Types.RemoteRepository (RemoteRepository(RemoteRepository))
import Debian.Repo.Types.Repo (Repo(..), RepoKey(..), compatibilityFile, libraryCompatibilityLevel)
import Debian.Repo.Types.Repository (Repository(LocalRepo, RemoteRepo))
import Debian.Sources ( SliceName(..), DebSource(..), SourceType(..) )
import Debian.URI (URI', fromURI', toURI', uriToString', URI(uriScheme, uriPath), dirFromURI, fileFromURI)
import Debian.UTF8 as Deb (decode)
import Extra.Files (maybeWriteFile)
import Extra.List (partitionM)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified System.Posix.Files as F (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink,
                                          fileMode, getFileStatus, setFileMode, removeLink)
import System.Process.Progress (quieter, qPutStr, qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import qualified Debian.Repo.Pretty as F (Pretty(..))
import qualified Debian.Repo.File as F ( File(..), readFile )
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat, text)
import Text.Regex (matchRegex, mkRegex)
import qualified Debian.Repo.File as F (Source(RemotePath))

class MonadIO m => MonadRepoCache m where
    getRepoCache :: m (Map URI' RemoteRepository)
    putRepoCache :: Map URI' RemoteRepository -> m ()

modifyRepoCache :: MonadRepoCache m => (Map URI' RemoteRepository -> Map URI' RemoteRepository) -> m ()
modifyRepoCache f = do
    s <- getRepoCache
    putRepoCache (f s)

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: (MonadRepoCache m, MonadTop m) => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty)) >>= putRepoCache
    where
      loadRepoCache' :: FilePath -> IO (Map URI' RemoteRepository)
      loadRepoCache' repoCache =
          do qPutStrLn "Loading repo cache..."
             file <- readFile repoCache
             case maybeRead file of
               Nothing ->
                   error ("Ignoring invalid repoCache: " ++ show file)
               Just pairs ->
                   qPutStrLn ("Loaded " ++ show (length pairs) ++ " entries from the repo cache.") >>
                   return (fromList pairs)

-- | Write the repo cache map into a file.
saveRepoCache :: (MonadIO m, MonadTop m, MonadRepoCache m) => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- getRepoCache
             repoCache <- liftIO $ loadCache path
             let merged = show . Map.toList $ Map.union live repoCache
             liftIO (F.removeLink path `IO.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `IO.catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

prepareRemoteRepository :: (MonadRepoCache m) => URI -> m RemoteRepository
prepareRemoteRepository uri =
    do (state :: Map URI' RemoteRepository) <- getRepoCache
       -- FIXME: We only want to verifyRepository on demand.
       repo <- maybe (verifyRemoteRepository (toURI' uri)) return (Map.lookup (toURI' uri) state)
       putRepoCache (Map.insert (toURI' uri) repo state)
       return repo

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
{-# NOINLINE verifyRemoteRepository #-}
verifyRemoteRepository :: MonadRepoCache m => URI' -> m RemoteRepository
verifyRemoteRepository uri =
    do state <- getRepoCache
       case Map.lookup uri state of
         Just repo -> return repo
         Nothing ->
             do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
                let repo = RemoteRepository uri releaseInfo
                modifyRepoCache (Map.insert uri repo)
                return repo

--instance Read URI where
--    readsPrec _ s = [(fromJust (parseURI s), "")]
--
---- |Get the list of releases of a remote repository.
--getReleaseInfo :: FilePath
--               -> Bool     -- ^ If False don't look at existing cache
--               -> URI
--               -> IO [ReleaseInfo]
--getReleaseInfo top tryCache uri =
--    readCache >>= \ cache ->
--    return (lookup uri cache) >>=
--    maybe (updateCache cache) return
--    where
--      cachePath = top ++ "/repoCache"
--      readCache :: IO [(URI, [ReleaseInfo])]
--      readCache = if tryCache
--                  then try (readFile cachePath >>= return . read) >>= return . either (\ (_ :: SomeException) -> []) id
--                  else return []
--      updateCache :: [(URI, [ReleaseInfo])] -> IO [ReleaseInfo]
--      updateCache pairs = getReleaseInfoRemote uri >>= \ info ->
--                          writeCache ((uri, info) : pairs) >> return info
--      writeCache :: [(URI, [ReleaseInfo])] -> IO ()
--      writeCache pairs = writeFile (show pairs) cachePath

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
             (releaseFiles :: [F.File (T.Paragraph' Text)]) <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap T.unpack (T.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe Text -> (F.File T.Paragraph) -> ReleaseName -> Maybe Release
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (T.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo info (parseReleaseName (T.unpack dist)) []
      getSuite :: F.File (T.Paragraph' Text) -> Maybe Text
      getSuite (F.File {F.text = Success releaseFile}) = T.fieldValue (releaseNameField releaseFile) releaseFile
      getSuite (F.File {F.text = Failure msgs}) = fail (intercalate "\n" msgs)
      getReleaseFile :: ReleaseName -> IO (F.File (T.Paragraph' Text))
      getReleaseFile distName =
          do qPutStr "."
             release <- fileFromURI releaseURI
             let control = either Left (either (Left . toException . ErrorCall . show) Right . T.parseControl (show releaseURI) . Deb.decode) release
             case control of
               Right (T.Control [info :: T.Paragraph' Text]) -> return $ F.File {F.path = F.RemotePath releaseURI, F.text = Success info}
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c

prepareRepository :: (MonadRepoCache m) => RepoKey -> m Repository
prepareRepository key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= return . LocalRepo
      Remote uri' ->
          let uri = fromURI' uri' in
          case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               _ -> prepareRemoteRepository uri >>= return . RemoteRepo
