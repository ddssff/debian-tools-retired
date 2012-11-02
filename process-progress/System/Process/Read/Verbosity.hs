module System.Process.Read.Verbosity
    ( quieter
    , noisier
    , withModifiedVerbosity
    , verbosity
    , qPutStr
    , qPutStrLn
    , qMessage
    , qMessageLn
    ) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Posix.Env (getEnv, setEnv, unsetEnv)
import System.Process.Read.Convenience (ePutStr, ePutStrLn)

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

verbosity :: MonadIO m => m Int
verbosity = liftIO $ getEnv "VERBOSITY" >>= return . maybe 1 read

modifyEnv :: String -> (Maybe String -> Maybe String) -> IO ()
modifyEnv name f = getEnv name >>= mySetEnv name . f

mySetEnv :: String -> Maybe String -> IO ()
mySetEnv name mvalue =
    maybe (unsetEnv name)
          (\ value -> setEnv name value True)
          mvalue

qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn s = verbosity >>= \ v -> when (v > 1) (ePutStrLn s)

qPutStr :: MonadIO m => String -> m ()
qPutStr s = verbosity >>= \ v -> when (v > 1) (ePutStr s)

qMessage :: MonadIO m => String -> a -> m a
qMessage s x = qPutStr s >> return x

qMessageLn :: MonadIO m => String -> a -> m a
qMessageLn s x = qPutStrLn s >> return x
