{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
module Debian.Repo.Monads.Deb
    ( MonadDeb
    , runDebT
    ) where

import Control.Exception (Exception)
import Control.Monad.Error (MonadError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT(runStateT))
import Control.Monad.Trans (MonadIO)
import Debian.Repo.Monads.Apt (AptState, MonadApt, initState)
import Debian.Repo.Monads.Top (MonadTop)

class (Exception e, MonadError e m, MonadIO m, Functor m, MonadApt m, MonadTop m) => MonadDeb e m where

-- instance (Exception e, MonadError e m, MonadIO m, Functor m, MonadApt m, MonadTop m) => MonadDeb e m

instance (Exception e, MonadError e IO) => MonadDeb e (ReaderT FilePath (StateT AptState IO))

-- | Run a known instance of MonadDeb.
runDebT :: Monad m => r -> ReaderT r (StateT AptState m) a -> m a
runDebT top action = runStateT (runReaderT action top) initState >>= return . fst
