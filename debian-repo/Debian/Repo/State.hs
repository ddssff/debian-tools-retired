-- | 'MonadRepos' is a state monad that maintains maps of all the
-- known repositories, releases, apt-get environements and operating
-- system environments.
{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.State
    ( ReposState
    , MonadRepos(getRepos, putRepos)
    , modifyRepos
    , runReposT

    , repoMap
    , releaseMap
    -- , sourcePackageMap
    -- , binaryPackageMap

    , osImageMap
    , OSKey
    , findOSKey
    , putOSImage
    , evalMonadOS

    , aptImageMap
    , AptKey
    , findAptKey
    , putAptImage
    , evalMonadApt

    , ReleaseKey
    , findRelease
    , putRelease
    , getRelease

    , MonadReposCached
    , runReposCachedT
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Catch (bracket, catch, MonadCatch)
import Control.Monad.State (MonadIO(..), MonadState(get, put), MonadTrans(..), StateT(runStateT))
import Data.Lens.Lazy (getL, modL, setL)
import Data.Lens.Template (makeLenses)
import Data.Map as Map (empty, fromList, insert, lookup, Map, toList, union)
import Data.Maybe (fromMaybe)
import Debian.Release (ReleaseName)
import Debian.Repo.AptImage (AptImage, aptImageRoot)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.OSImage (OSImage, osRoot)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (Repo, repoKey, RepoKey(..))
import Debian.Repo.Top (MonadTop, runTopT, sub, TopT)
import Debian.URI (URI')
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)
import qualified System.Posix.Files as F (removeLink)
import System.Process.Progress (qPutStrLn)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- | A monad to support the IO requirements of the autobuilder.
class (MonadCatch m, MonadIO m, Functor m) => MonadRepos m where
    getRepos :: m ReposState
    putRepos :: ReposState -> m ()

modifyRepos :: MonadRepos m => (ReposState -> ReposState) -> m ()
modifyRepos f = getRepos >>= putRepos . f

instance (MonadIO m, Functor m, MonadCatch m) => MonadRepos (StateT ReposState m) where
    getRepos = get
    putRepos = put

instance MonadRepos m => MonadRepos (StateT AptImage m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

instance MonadRepos m => MonadRepos (StateT OSImage m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

newtype OSKey = OSKey EnvRoot deriving (Eq, Ord, Show)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

data ReleaseKey = ReleaseKey RepoKey ReleaseName deriving (Eq, Ord, Show)

-- | This represents the state of the IO system.
data ReposState
    = ReposState
      { _repoMap :: Map.Map URI' RemoteRepository		-- ^ Map to look up known (remote) Repository objects
      , _releaseMap :: Map.Map ReleaseKey Release -- ^ Map to look up known Release objects
      , _aptImageMap :: Map.Map AptKey AptImage	-- ^ Map to look up prepared AptImage objects
      , _osImageMap :: Map.Map OSKey OSImage	-- ^ Map to look up prepared OSImage objects
      -- , _sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      -- , _binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
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
            , _osImageMap = Map.empty
            -- , _sourcePackageMap = Map.empty
            -- , _binaryPackageMap = Map.empty
            }

-- | See if there is an OSImage associated with this directory
findOS :: MonadRepos m => EnvRoot -> m (Maybe OSImage)
findOS root = (Map.lookup (OSKey root) . getL osImageMap) <$> getRepos

-- | See if there is an OSImage associated with this directory
findOSKey :: MonadRepos m => EnvRoot -> m (Maybe OSKey)
findOSKey root = fmap (OSKey . getL osRoot) <$> (findOS root)

putOSImage :: MonadRepos m => OSImage -> m OSKey
putOSImage repo = do
  let key = OSKey (getL osRoot repo)
  modifyRepos (modL osImageMap (Map.insert key repo))
  return key

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadRepos m => StateT OSImage m a -> OSKey -> m a
evalMonadOS task (OSKey key) = do
  Just os <- findOS key
  (a, os') <- runStateT task os
  putOSImage os'
  return a

findApt :: MonadRepos m => EnvRoot -> m (Maybe AptImage)
findApt root = (Map.lookup (AptKey root) . getL aptImageMap) <$> getRepos

findAptKey :: MonadRepos m => EnvRoot -> m (Maybe AptKey)
findAptKey root = fmap (AptKey . getL aptImageRoot) <$> (findApt root)

putAptImage :: MonadRepos m => AptImage -> m AptKey
putAptImage repo = do
  let key = AptKey (getL aptImageRoot repo)
  modifyRepos (modL aptImageMap (Map.insert key repo))
  return key

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadApt :: MonadRepos m => StateT AptImage m a -> AptKey -> m a
evalMonadApt task (AptKey key) = do
  Just apt <- findApt key
  (a, apt') <- runStateT task apt
  putAptImage apt'
  return a

findRelease :: (Repo r, MonadRepos m) => r -> ReleaseName -> m (Maybe Release)
findRelease repo dist = (Map.lookup (ReleaseKey (repoKey repo) dist) . getL releaseMap) <$> getRepos

getRelease :: MonadRepos m => ReleaseKey -> m Release
getRelease key = do
  Just rel <- (Map.lookup key . getL releaseMap) <$> getRepos
  return rel

putRelease :: (Repo r, MonadRepos m) => r -> Release -> m ReleaseKey
putRelease repo release = do
    let key = ReleaseKey (repoKey repo) (releaseName release)
    modifyRepos $ modL releaseMap (Map.insert key release)
    return key

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
class (MonadRepos m, MonadTop m, MonadCatch m) => MonadReposCached m

type ReposCachedT m = TopT (StateT ReposState m)

-- instance (MonadRepos m, MonadTop m, MonadCatch m, Functor m) => MonadReposCached m
instance (MonadCatch m, MonadIO m, Functor m) => MonadReposCached (ReposCachedT m)
instance (MonadCatch m, MonadIO m, Functor m) => MonadRepos (ReposCachedT m) where
    getRepos = lift get
    putRepos = lift . put

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runReposCachedT :: (MonadIO m, MonadCatch m, Functor m) => FilePath -> ReposCachedT m a -> m a
runReposCachedT top action = runReposT $ runTopT top $ bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: MonadReposCached m => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       mp <- liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty))
       modifyRepos (setL repoMap mp)
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
             live <- getL repoMap <$> getRepos
             repoCache <- liftIO $ loadCache path
             let merged = Map.union live repoCache
             liftIO (F.removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e)) >>
                     writeFile path (show . Map.toList $ merged))
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead
