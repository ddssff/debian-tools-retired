{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Prelude
    ( countTasks
    , nub'
    , access
    , (~=)
    , (%=)
    , symbol
    , runProc
    , checkRsyncExitCode
    ) where

import Control.Monad.State (MonadState, MonadIO, modify, get)
import qualified Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L (empty)
import Data.Lens.Lazy (Lens, getL, modL)
import Data.List as List (group, map, sort)
import Language.Haskell.TH
import System.Exit (ExitCode(..))
import System.Process (CreateProcess)
import System.Process.Read
import System.Process.Progress (ePutStrLn, keepResult, keepResult, runProcessF)
import Text.Printf (printf)

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task

-- | This nub doesn't preserve order
nub' :: (Ord a) => [a] -> [a]
nub' = List.map head . group . sort

access :: MonadState a m => Lens a b -> m b
access l = get >>= return . getL l

(~=) :: MonadState a m => Lens a b -> b -> m ()
l ~= x = l %= const x

-- | Modify a value.  (This is a version of Data.Lens.Lazy.%= that returns () instead of a.)
(%=) :: MonadState a m => Lens a b -> (b -> b) -> m ()
l %= f = modify (modL l f)

symbol :: Name -> Q Exp
symbol x = return $ LitE (StringL (maybe "" (++ ".") (nameModule x) ++ nameBase x))

runProc p = runProcessF (Just (" 1> ", " 2> ")) p L.empty

checkRsyncExitCode :: Monad m => ExitCode -> m ()
checkRsyncExitCode ExitSuccess = return ()
checkRsyncExitCode (ExitFailure n) =
    case n of
      1 -> error "Syntax or usage error"
      2 -> error "Protocol incompatibility"
      3 -> error "Errors selecting input/output files, dirs"
      4 -> error "Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server."
      5 -> error "Error starting client-server protocol"
      6 -> error "Daemon unable to append to log-file"
      10 -> error "Error in socket I/O"
      11 -> error "Error in file I/O"
      12 -> error "Error in rsync protocol data stream"
      13 -> error "Errors with program diagnostics"
      14 -> error "Error in IPC code"
      20 -> error "Received SIGUSR1 or SIGINT"
      21 -> error "Some error returned by waitpid()"
      22 -> error "Error allocating core memory buffers"
      23 -> error "Partial transfer due to error"
      24 -> error "Partial transfer due to vanished source files"
      25 -> error "The --max-delete limit stopped deletions"
      30 -> error "Timeout in data send/receive"
      35 -> error "Timeout waiting for daemon connection"
      _ -> error $ "Unexpected failure in rsync: " ++ show n