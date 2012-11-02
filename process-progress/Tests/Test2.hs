#!/usr/bin/runhaskell

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.IO (hPutStr, stderr)
import System.Process (CmdSpec(ShellCommand))
import System.Process.Read (readProcessChunks, dots)

main :: IO ()
main = readProcessChunks id (ShellCommand "yes") L.empty >>=
       dots 50000000 (\ n -> hPutStr stderr (replicate (fromIntegral n) '.')) >>
       return ()
