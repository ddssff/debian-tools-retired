{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
module Debian.Repo.Monads.Deb
    ( MonadDeb
    , runDebT
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException)
import Control.Monad (unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (bracket, catch, MonadCatchIO)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Map as Map (empty, fromList, Map, toList, union)
import Data.Maybe (fromMaybe)
import Debian.Repo.Monads.Apt (AptT, MonadApt(getRepoCache, putRepoCache), runAptT)
import Debian.Repo.Monads.Top (MonadTop, runTopT, sub, TopT)
import Debian.Repo.Types.RemoteRepository (RemoteRepository)
import Debian.URI (URI')
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Files as F (removeLink)
import System.Process.Progress (qPutStrLn)

-- | Like @MonadApt@, but is also an instance of MonadTop and tries to
-- load and save a list of cached repositories from @top/repoCache@.
class (MonadApt m, MonadTop m) => MonadDeb m

instance MonadApt m => MonadDeb (TopT m)

type DebT m = TopT (AptT m)

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runDebT :: (MonadCatchIO m, Functor m) => FilePath -> DebT m a -> m a
runDebT top action = runAptT $ runTopT top $ bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: MonadDeb m => m ()
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
saveRepoCache :: MonadDeb m => m ()
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
