{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Monads.Apt
    ( MonadApt(..)
    , AptT
    , runAptT

    , AptState
    -- , repoMap
    , releaseMap
    , aptImageMap
    , sourcePackageMap
    , binaryPackageMap

    , prepareRemoteRepository
    , prepareRepository
    ) where

import Control.Monad (void)
import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (get, MonadIO(..), MonadTrans(..), put, StateT(runStateT))
import Data.Lens.Lazy (access, (~=))
import Data.Lens.Template (makeLenses)
import Data.Map as Map (empty, insert, lookup, Map)
import Debian.Release (ReleaseName)
import Debian.Repo.Types.AptImage (AptImage)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot))
import Debian.Repo.Types.LocalRepository (prepareLocalRepository)
import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Types.Release (getReleaseInfoRemote, Release)
import Debian.Repo.Types.RemoteRepository (RemoteRepository, RemoteRepository(RemoteRepository))
import Debian.Repo.Types.Repo (RepoKey(..))
import Debian.Repo.Types.Repository (Repository, Repository(LocalRepo, RemoteRepo))
import Debian.Sources (SliceName)
import Debian.URI (fromURI', toURI', URI(uriPath, uriScheme), URI')
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

type AptT = StateT AptState

-- | A monad to support the IO requirements of the autobuilder.
class (MonadIO m, Functor m, MonadCatchIO m) => MonadApt m where
    getApt :: m AptState
    putApt :: AptState -> m ()
    getRepoCache :: m (Map URI' RemoteRepository)
    putRepoCache :: Map URI' RemoteRepository -> m ()

-- | This represents the state of the IO system.
data AptState
    = AptState
      { _repoMap :: Map.Map URI' RemoteRepository		-- ^ Map to look up known (remote) Repository objects
      , _releaseMap :: Map.Map (RepoKey, ReleaseName) Release -- ^ Map to look up known Release objects
      , _aptImageMap :: Map.Map SliceName AptImage	-- ^ Map to look up prepared AptImage objects
      , _sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      , _binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
      }

$(makeLenses [''AptState])

runAptT :: Monad m => AptT m a -> m a
runAptT action = (runStateT action) initState >>= return . fst

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: AptState
initState = AptState
            { _repoMap = Map.empty
            , _releaseMap = Map.empty
            , _aptImageMap = Map.empty
            , _sourcePackageMap = Map.empty
            , _binaryPackageMap = Map.empty
            }

instance (MonadIO m, Functor m, MonadCatchIO m) => MonadApt (AptT m) where
    getApt = get
    putApt = put
    getRepoCache = access repoMap
    putRepoCache mp = void $ repoMap ~= mp

instance MonadApt m => MonadApt (ReaderT s m) where
    getApt = lift getApt
    putApt = lift . putApt
    getRepoCache = lift getRepoCache
    putRepoCache = lift . putRepoCache

modifyRepoCache :: MonadApt m => (Map URI' RemoteRepository -> Map URI' RemoteRepository) -> m ()
modifyRepoCache f = do
    s <- getRepoCache
    putRepoCache (f s)

prepareRemoteRepository :: MonadApt m => URI -> m RemoteRepository
prepareRemoteRepository uri =
    getRepoCache >>= maybe (loadRemoteRepository (toURI' uri)) return . Map.lookup (toURI' uri)

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
loadRemoteRepository :: MonadApt m => URI' -> m RemoteRepository
loadRemoteRepository uri =
    do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
       let repo = RemoteRepository uri releaseInfo
       modifyRepoCache (Map.insert uri repo)
       return repo

prepareRepository :: MonadApt m => RepoKey -> m Repository
prepareRepository key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= return . LocalRepo
      Remote uri' -> repositoryFromURI (fromURI' uri')

repositoryFromURI :: MonadApt m => URI -> m Repository
repositoryFromURI uri =
    case uriScheme uri of
      "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
      _ -> prepareRemoteRepository uri >>= return . RemoteRepo

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
