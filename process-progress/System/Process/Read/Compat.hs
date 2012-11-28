-- | Some functions brought over from my obsolete progress packages.
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module System.Process.Read.Compat
    ( echo
    , oneResult
    , timeTask
    ) where

import Control.Exception (evaluate)
--import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..), showCommandForUser)
import System.Process.Read.Chars (ListLikePlus)
import System.Process.Read.Convenience (ePutStrLn, keepResult)
import System.Process.Read.Chunks (Output(..))

echo :: CmdSpec -> IO () -> IO ()
echo (RawCommand cmd args) io = ePutStrLn ("-> " ++ showCommandForUser cmd args) >> io
echo (ShellCommand cmd) io = ePutStrLn ("-> " ++ cmd) >> io

oneResult :: ListLikePlus a c => [Output a] -> ExitCode
oneResult xs =
    case keepResult xs of
      [code] -> code
      [] -> error "Missing result code"
      codes -> error $ "Multiple result codes: " ++ show codes

-- |Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)
