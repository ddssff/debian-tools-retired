module System.Process.Read.Verbosity
    ( quieter
    , noisier
    , withModifiedVerbosity
    , defaultVerbosity
    , verbosity
    -- * Process functions controlled by the VERBOSITY level.
    , runProcess
    , runProcessF
    -- * Output functions controlled by the VERBOSITY level.  We want these
    -- to output whenever the runProcess functions are not silent, and we want
    -- them to output at the first silent output setting, but then to stop.
    , qPutStr
    , qPutStrLn
    , qMessage
    , qMessageLn
    ) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Process (CreateProcess)
import System.Posix.EnvPlus (getEnv, modifyEnv)
import System.Process.Read.Chunks (Output, NonBlocking)
import System.Process.Read.Convenience (ePutStr, ePutStrLn)
import System.Process.Read.Monad (runProcessS, runProcessQ, runProcessD, runProcessV,
                                  runProcessSF, runProcessQF, runProcessDF, runProcessVF,
                                  runProcessSE, runProcessQE, runProcessDE)

quieter :: MonadIO m => Int -> m a -> m a
quieter n action = withModifiedVerbosity (\ v -> v - n) action
noisier :: MonadIO m => Int -> m a -> m a
noisier n action = withModifiedVerbosity (\ v -> v + n) action

withModifiedVerbosity :: MonadIO m => (Int -> Int) -> m a -> m a
withModifiedVerbosity f action =
    verbosity >>= \ v ->
    liftIO (modifyEnv "VERBOSITY" (const (Just (show (f v))))) >>
    action >>= \ result ->
    liftIO (modifyEnv "VERBOSITY" (const (Just (show v)))) >>
    return result

defaultVerbosity :: Int
defaultVerbosity = 1

verbosity :: MonadIO m => m Int
verbosity = liftIO $ getEnv "VERBOSITY" >>= return . maybe 1 read

-- | Select from the runProcess* functions in Monad based on a verbosity level.
runProcess :: (NonBlocking c, MonadIO m) => CreateProcess -> c -> m [Output c]
runProcess p input = liftIO $ 
    verbosity >>= \ v ->
    case v of
      _ | v <= 0 -> runProcessS p input
      1 -> runProcessQ p input
      2 -> runProcessD p input
      _ -> runProcessV p input

-- | A version of 'runProcess' that throws an exception on failure.
runProcessF :: (NonBlocking c, MonadIO m) => CreateProcess -> c -> m [Output c]
runProcessF p input = liftIO $
    verbosity >>= \ v ->
    case v of
      _ | v < 0 -> runProcessSF p input
      0 -> runProcessSE p input
      1 -> runProcessQE p input
      2 -> runProcessDE p input
      _ -> runProcessVF p input

qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn s = verbosity >>= \ v -> when (v > 1) (ePutStrLn s)

qPutStr :: MonadIO m => String -> m ()
qPutStr s = verbosity >>= \ v -> when (v > 1) (ePutStr s)

qMessage :: MonadIO m => String -> a -> m a
qMessage s x = qPutStr s >> return x

qMessageLn :: MonadIO m => String -> a -> m a
qMessageLn s x = qPutStrLn s >> return x
