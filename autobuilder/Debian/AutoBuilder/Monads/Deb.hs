{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Monads.Deb
    ( MonadDeb
    , runDebT
    ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT(runStateT))
import Control.Monad.Trans (MonadIO)
import Debian.Repo.Monads.Apt (AptState, MonadApt, initState)
import Debian.Repo.Monads.Top (MonadTop)
import Distribution.Debian.MonadBuild (MonadBuild)

class (MonadIO m, Functor m, MonadApt m, MonadTop m, MonadBuild m) => MonadDeb m where

instance MonadApt m => MonadDeb (ReaderT FilePath m)

-- | Run a known instance of MonadDeb.
runDebT :: Monad m => FilePath -> ReaderT FilePath (StateT AptState m) a -> m a
runDebT top action = runStateT (runReaderT action top) initState >>= return . fst
