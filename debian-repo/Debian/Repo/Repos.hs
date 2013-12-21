{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Repos
    ( MonadRepos
    , runReposT

    , releaseMap
    , aptImageMap
    , sourcePackageMap
    , binaryPackageMap

    , prepareRemoteRepository
    , foldRepository

    , MonadReposCached
    , runReposCachedT
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException)
import Control.Monad (unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (bracket, catch, MonadCatchIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadIO(..), MonadTrans(..), StateT(runStateT), MonadState(get, put))
import Data.Lens.Lazy (getL, setL, modL)
import Data.Lens.Template (makeLenses)
import Data.Map as Map (empty, fromList, insert, lookup, Map, toList, union)
import Data.Maybe (fromMaybe)
import Debian.Release (ReleaseName)
import Debian.Repo.AptImage (AptImage)
import Debian.Repo.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot))
import Debian.Repo.LocalRepository (LocalRepository, prepareLocalRepository)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Release (getReleaseInfoRemote, Release)
import Debian.Repo.RemoteRepository (RemoteRepository, RemoteRepository(RemoteRepository))
import Debian.Repo.Repo (RepoKey(..))
import Debian.Repo.Top (MonadTop, runTopT, sub, TopT)
import Debian.Sources (SliceName)
import Debian.URI (fromURI', toURI', URI(uriPath, uriScheme), URI')
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)
import qualified System.Posix.Files as F (removeLink)
import System.Process.Progress (qPutStrLn)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- | A monad to support the IO requirements of the autobuilder.
class (MonadState ReposState m, MonadCatchIO m, Functor m) => MonadRepos m

instance (MonadState ReposState m, MonadCatchIO m, Functor m) => MonadRepos m

-- | This represents the state of the IO system.
data ReposState
    = ReposState
      { _repoMap :: Map.Map URI' RemoteRepository		-- ^ Map to look up known (remote) Repository objects
      , _releaseMap :: Map.Map (RepoKey, ReleaseName) Release -- ^ Map to look up known Release objects
      , _aptImageMap :: Map.Map SliceName AptImage	-- ^ Map to look up prepared AptImage objects
      , _sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      , _binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
      }

$(makeLenses [''ReposState])

runReposT :: Monad m => StateT ReposState m a -> m a
runReposT action = (runStateT action) initState >>= return . fst

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: ReposState
initState = ReposState
            { _repoMap = Map.empty
            , _releaseMap = Map.empty
            , _aptImageMap = Map.empty
            , _sourcePackageMap = Map.empty
            , _binaryPackageMap = Map.empty
            }

prepareRemoteRepository :: MonadRepos m => URI -> m RemoteRepository
prepareRemoteRepository uri =
    do mp <- getL repoMap <$> get
       maybe (loadRemoteRepository (toURI' uri)) return $ Map.lookup (toURI' uri) mp

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
loadRemoteRepository :: MonadRepos m => URI' -> m RemoteRepository
loadRemoteRepository uri =
    do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
       let repo = RemoteRepository uri releaseInfo
       get >>= put . modL repoMap (Map.insert uri repo)
       return repo

-- foldRepository :: forall m r a. MonadState ReposState m => (r -> m a) -> RepoKey -> m a
-- foldRepository f key =
--     case key of
--       Local path -> prepareLocalRepository path Nothing >>= f
--       Remote uri' ->
--           let uri = fromURI' uri' in
--           case uriScheme uri of
--             "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= f
--             _ -> prepareRemoteRepository uri >>= f

foldRepository :: MonadRepos m => (LocalRepository -> m a) -> (RemoteRepository -> m a) -> RepoKey -> m a
foldRepository f g key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= f
      Remote uri' ->
          let uri = fromURI' uri' in
          case uriScheme uri of
            "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= f
            _ -> prepareRemoteRepository uri >>= g

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

-- | Like @MonadRepos@, but is also an instance of MonadTop and tries to
-- load and save a list of cached repositories from @top/repoCache@.
class (MonadRepos m, MonadTop m, MonadCatchIO m, Functor m) => MonadReposCached m

-- instance (MonadState ReposState m, MonadIO m, Functor m) => MonadReposCached (TopT m)

instance (MonadRepos m, MonadTop m, MonadCatchIO m, Functor m) => MonadReposCached m

-- instance (MonadTop m, MonadIO m, Functor m) => MonadReposCached (StateT ReposState m)

-- instance (MonadState ReposState m, MonadCatchIO m, Functor m) => MonadRepos m
-- instance MonadReposCached m => MonadRepos m

type ReposCachedT m = TopT (StateT ReposState m)

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runReposCachedT :: (MonadCatchIO m, Functor m) => FilePath -> ReposCachedT m a -> m a
runReposCachedT top action = runReposT $ runTopT top $ bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: MonadReposCached m => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       mp <- liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty))
       get >>= put . setL repoMap mp
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
saveRepoCache :: MonadReposCached m => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- getL repoMap <$> get
             repoCache <- liftIO $ loadCache path
             let merged = Map.union live repoCache
             liftIO (F.removeLink path `IO.catch` (\e -> unless (isDoesNotExistError e) (ioError e)) >>
                     writeFile path (show . Map.toList $ merged))
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `IO.catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead
