-- | Some functions brought over from my obsolete progress packages.
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.Compat
    ( echo
{-
    , lazyCommandE
    , lazyCommandEF
    , lazyCommandF
    , lazyCommandV
    , lazyCommand
    , lazyProcessE
    , lazyProcessEF
    , lazyProcessF
    , lazyProcessV
    , lazyProcess
-}
    , oneResult
    , timeTask
    ) where

import Control.Exception (evaluate)
--import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..), showCommandForUser)
import System.Process.Read.Chars (Chars)
import System.Process.Read.Convenience (ePutStrLn, keepResult)
import System.Process.Read.Chunks (Output(..))

echo :: CmdSpec -> IO () -> IO ()
echo (RawCommand cmd args) io = ePutStrLn ("-> " ++ showCommandForUser cmd args) >> io
echo (ShellCommand cmd) io = ePutStrLn ("-> " ++ cmd) >> io

{-
lazyCommandE :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandE cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput
lazyCommandEF :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandEF cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput >>= foldFailure (\ n -> error ("Command failed with exit code " ++ show n))
lazyCommandF :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandF cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput >>= foldFailure (\ n -> error ("Command failed with exit code " ++ show n))
lazyCommandV :: NonBlocking a => String -> a -> IO [Output a]
lazyCommandV cmd input = readProcessChunks id (ShellCommand cmd) input >>= doOutput

lazyProcessE :: NonBlocking a => String -> [String] -> a -> IO [Output a]
lazyProcessE cmd args input = readProcessChunks id (RawCommand cmd args) input >>= doOutput
lazyProcessEF :: NonBlocking a => String -> [String] -> a -> IO [Output a]
lazyProcessEF cmd args input = readProcessChunks id (RawCommand cmd args) input >>= doOutput >>= foldFailure (\ n -> error ("Process failed with exit code " ++ show n))
lazyProcessF :: NonBlocking a => String -> [String] -> a -> IO [Output a]
lazyProcessF cmd args input = readProcessChunks id (RawCommand cmd args) input >>= doOutput >>= foldFailure (\ n -> error ("Process failed with exit code " ++ show n))
lazyProcessV :: NonBlocking a => String -> [String] -> a -> IO [Output a]
lazyProcessV cmd args input = readProcessChunks id (RawCommand cmd args) input >>= doOutput
-}

oneResult :: Chars a => [Output a] -> ExitCode
oneResult xs =
    case keepResult xs of
      [code] -> code
      [] -> error "Missing result code"
      codes -> error $ "Multiple result codes: " ++ show codes

{-
doFailure :: Chars a => (Int -> IO ()) -> [Output a] -> IO [Output a]
doFailure onFailure = mapM (\ x -> foldOutput doFailure ignore ignore ignore x >> return x)
    where doFailure ExitSuccess = return ()
          doFailure (ExitFailure n) = onFailure n

checkResult :: (Int -> IO ()) -> [Output L.ByteString] -> IO [Output L.ByteString]
checkResult = doFailure
-}

-- |Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)

{-
lazyCommand :: NonBlocking a => String -> a -> IO [Output a]
lazyCommand cmd input = readProcessChunks id (ShellCommand cmd) input

lazyProcess :: NonBlocking a => String -> [String] -> a -> IO [Output a]
lazyProcess cmd args input = readProcessChunks id (RawCommand cmd args) input
-}
