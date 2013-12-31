-- | The top of a directory which belongs to the client process, used
-- for temporary storage.  The autobuilder usually assigns the path
-- "~/.autobuilder", and stores build environments and downloaded
-- source here.
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Debian.Repo.Top
    ( TopT
    , runTopT
    , MonadTop(askTop)
    , sub
    , dists
    ) where

import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import System.FilePath ((</>), isRelative)

newtype TopDir = TopDir {unTopDir :: FilePath}

type TopT = ReaderT TopDir

runTopT :: FilePath -> TopT m a -> m a
runTopT path action = (runReaderT action) (TopDir path)

class (Monad m, Functor m) => MonadTop m where
    askTop :: m FilePath

instance (Monad m, Functor m) => MonadTop (TopT m) where
    askTop = ask >>= return . unTopDir

sub :: MonadTop m => FilePath -> m FilePath
sub path | isRelative path = askTop >>= \ top -> return $ top </> path
sub path = fail ("sub - path argument must be relative: " ++ path)

instance MonadTop m => MonadTop (StateT s m) where
    askTop = lift askTop

dists :: MonadTop m => m FilePath
dists = sub "dists"
