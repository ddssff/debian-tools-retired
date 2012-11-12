{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Monads.Deb
    ( MonadDeb
    , runDebT
    ) where

import Control.Monad.State (StateT(runStateT))
import Control.Monad.Trans (MonadIO)
import Debian.Repo.Monads.Apt (AptState, MonadApt, initState)
import Debian.Repo.Monads.Top (MonadTop, TopT, runTopT)

class (MonadIO m, Functor m, MonadApt m, MonadTop m) => MonadDeb m where

instance MonadApt m => MonadDeb (TopT m)

-- | Run a known instance of MonadDeb.
runDebT :: Monad m => FilePath -> TopT (StateT AptState m) a -> m a
runDebT top action = runStateT (runTopT top action) initState >>= return . fst
