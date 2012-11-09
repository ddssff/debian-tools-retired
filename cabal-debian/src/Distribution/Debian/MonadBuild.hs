{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Distribution.Debian.MonadBuild
    ( BuildT
    , runBuildT
    , MonadBuild(askBuild)
    ) where

import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

type BuildT = ReaderT FilePath

runBuildT :: FilePath -> BuildT m a -> m a
runBuildT path action = (runReaderT action) path

class Monad m => MonadBuild m where
    askBuild :: m FilePath

instance Monad m => MonadBuild (BuildT m) where
    askBuild = ask

instance MonadBuild m => MonadBuild (StateT s m) where
    askBuild = lift askBuild

