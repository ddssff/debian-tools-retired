-- | A perhaps over-engineered set of wrappers around
-- readProcessChunks to run processes with a variety of echoing
-- options and responses to failure.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module System.Process.Read.Monad
    ( -- * Run processes with various types and amounts of feedback
      runProcessS
    , runProcessQ
    , runProcessD
    , runProcessV
    , runProcessSF
    , runProcessQF
    , runProcessDF
    , runProcessVF
    , runProcessSE
    , runProcessQE
    , runProcessDE
    ) where

import Control.Monad (when, unless)
import Control.Monad.State (StateT(runStateT), get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (intercalate)
import qualified Data.ListLike as P
import Prelude hiding (print)
import System.Exit (ExitCode(ExitFailure))
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Process (CreateProcess(cmdspec), CmdSpec(RawCommand, ShellCommand), showCommandForUser)
import qualified System.Process.Read.Chars as P
import qualified System.Process.Read.Chunks as P
import qualified System.Process.Read.Convenience as P

-- | The state we need when running processes
data RunState s
    = RunState
      { cpd :: Int  -- ^ Output one dot per n characters of process output, 0 means no dots
      , trace :: Bool -- ^ Echo the command line before starting, and after with the result code
      , echo :: Bool -- ^ Echo the output as it is red, using the prefixes set below
      , prefixes :: Maybe (s, s)
      -- ^ Prepend a prefix to the echoed lines of stdout and stderr.
      --  Special case for Just ("", ""), which means echo unmodified output.
      , exnPrefixes :: Maybe (s, s)
      -- ^ Prefixes to use when generating output after an exception occurs.
      , failEcho :: Bool -- ^ Echo the process output if the result code is ExitFailure
      , failExit :: Bool -- ^ Throw an IOError if the result code is ExitFailure
      } deriving (Show)

defaultRunState :: RunState s
defaultRunState = RunState {cpd=0, trace=True, echo=False, failEcho=False, failExit=False, prefixes=Nothing, exnPrefixes=Nothing}

-- | The monad for running processes
type RunT s = StateT (RunState s)

withRunState :: MonadIO m => RunState s -> RunT s m a -> m a
withRunState s action =
    (runStateT action) s >>= return . fst

modifyRunState :: MonadIO m => (RunState s -> RunState s) -> RunT s m ()
modifyRunState modify = get >>= put . modify

charsPerDot :: MonadIO m => Int -> RunT s m ()
charsPerDot x = modifyRunState (\ s -> s {cpd = x})

echoCommand :: MonadIO m => Bool -> RunT s m ()
echoCommand x = modifyRunState (\ s -> s {trace = x})

echoOnFailure :: MonadIO m => Bool -> RunT s m ()
echoOnFailure x = modifyRunState (\ s -> s {failEcho = x})

exceptionOnFailure :: MonadIO m => Bool -> RunT s m ()
exceptionOnFailure x = modifyRunState (\ s -> s {failExit = x})

echoOutput :: MonadIO m => Bool -> RunT s m ()
echoOutput x = modifyRunState (\ s -> s {echo = x})

setPrefixes :: (P.ListLikePlus s c, MonadIO m) => Maybe (s, s) -> RunT s m ()
setPrefixes x = modifyRunState (\ s -> s {prefixes = x})

runProcessM :: forall s c m. (P.NonBlocking s c, MonadIO m) => CreateProcess -> s -> RunT s m [P.Output s]
runProcessM p input =
    do s <- get
       liftIO $ do
         when (trace s) (hPutStrLn stderr ("-> " ++ showCommand (cmdspec p)))
         (out1 :: [P.Output s]) <- P.readProcessChunks p input
         (out2 :: [P.Output s]) <- if cpd s > 0 then P.dots (fromIntegral (cpd s)) (\ n -> hPutStr stderr (replicate (fromIntegral n) '.')) out1 else return out1
         (out3 :: [P.Output s]) <- if echo s then doOutput (prefixes s) out2 else return out2
         (out5 :: [P.Output s]) <- if failExit s then P.foldFailure' (\ n -> doOutput (exnPrefixes s) out3 >> error (showCommand (cmdspec p) ++ " -> ExitFailure " ++ show n)) out3 else return out3
         (out6 :: [P.Output s]) <- (if trace s then  P.foldResult (\ ec -> hPutStrLn stderr ("<- " ++ showCommand (cmdspec p) ++ ": " ++ show ec) >> return (P.Result ec)) else return) out5
         (out7 :: [P.Output s]) <- (if failEcho s then P.foldFailure (\ n -> unless (trace s) (hPutStrLn stderr ("<- " ++ showCommand (cmdspec p) ++ ": " ++ show (ExitFailure n))) >>
                                                                              doOutput (prefixes s) out5 >> return (P.Result (ExitFailure n))) else return) out6
         return out7
    where
      text (P.Stdout x) = P.toList x
      text (P.Stderr x) = P.toList x
      text _ = []

doOutput :: P.ListLikePlus a c => Maybe (a, a) -> [P.Output a] -> IO [P.Output a]
-- doOutput prefixes out = maybe (P.doOutput out) (\ (sout, serr) -> P.prefixed sout serr out) prefixes >> return out
doOutput prefixes out = P.doOutput out >> return out

s :: MonadIO m => RunT s m ()
s = echoCommand False

c :: MonadIO m => RunT s m ()
c = echoCommand True

v :: (P.ListLikePlus s c, MonadIO m) => RunT s m ()
v = echoOutput True >> {- setPrefixes (Just (P.fromList " 1> ", P.fromList " 2> ")) >> -} echoOnFailure False

d :: MonadIO m => RunT s m ()
d = charsPerDot 50 >> echoOutput False

f :: MonadIO m => RunT s m ()
f = exceptionOnFailure True

e :: (P.ListLikePlus s c, MonadIO m) => RunT s m ()
e = echoOnFailure True >> {- setPrefixes (Just (P.fromList " 1> ", P.fromList " 2> ")) >> -} exceptionOnFailure True >> echoOutput False

-- | No output.
runProcessS :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessS p input = withRunState defaultRunState (s >> runProcessM p input)

-- | Command line trace only.
runProcessQ :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessQ p input = withRunState defaultRunState (runProcessM p input)

-- | Dot output
runProcessD :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessD p input =
    withRunState defaultRunState (c >> d >> runProcessM p input)

-- | Echo output
runProcessV :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessV p input =
    withRunState defaultRunState (c >> v >> runProcessM p input)

-- | Exception on failure
runProcessSF :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessSF p input =
    withRunState defaultRunState (s >> f >> runProcessM p input)

runProcessQF :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessQF p input =
    withRunState defaultRunState (c >> f >> runProcessM p input)

-- | Dot output and exception on failure
runProcessDF :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessDF p input =
    withRunState defaultRunState (c >> d >> f >> runProcessM p input)

-- | Echo output and exception on failure
runProcessVF :: (P.NonBlocking a c, MonadIO m) => CreateProcess -> a -> m [P.Output a]
runProcessVF p input =
    withRunState defaultRunState (c >> v >> f >> runProcessM p input)

-- | Exception and echo on failure
runProcessSE :: (P.NonBlocking a c, MonadIO m) => Maybe (a, a) -> CreateProcess -> a -> m [P.Output a]
runProcessSE prefixes p input =
    withRunState (defaultRunState {exnPrefixes = prefixes}) (s >> e >> runProcessM p input)

-- | Exception and echo on failure
runProcessQE :: (P.NonBlocking a c, MonadIO m) => Maybe (a, a) -> CreateProcess -> a -> m [P.Output a]
runProcessQE prefixes p input =
    withRunState (defaultRunState {exnPrefixes = prefixes}) (c >> e >> runProcessM p input)

-- | Dot output, exception on failure, echo on failure.  Note that
-- runProcessVE isn't a useful option, you get the output twice.  VF
-- makes more sense.
runProcessDE :: (P.NonBlocking a c, MonadIO m) => Maybe (a, a) -> CreateProcess -> a -> m [P.Output a]
runProcessDE prefixes p input =
    withRunState (defaultRunState {exnPrefixes = prefixes}) (c >> d >> e >> runProcessM p input)

showCommand :: CmdSpec -> String
showCommand (RawCommand cmd args) = showCommandForUser cmd args
showCommand (ShellCommand cmd) = cmd
