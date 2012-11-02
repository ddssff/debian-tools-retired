#!/usr/bin/runhaskell

import Data.ByteString.Lazy (empty)
import Data.ByteString.UTF8 (fromString)
--import Data.ByteString.Lazy.Char8 (pack)
import System.IO (hPutStr, stderr, hPutStrLn)
import System.Process (CmdSpec(RawCommand, ShellCommand))
import System.Process.Read (readProcessChunks)
import System.Process.Read.Convenience (isResult, dots)

main :: IO ()
main =
    do readProcessChunks id (ShellCommand "rsync -aHxS -v dsf@src.seereason.com:/srv/darcs/public/process-extras/ /tmp/process-extras 1>&2") (fromString "") >>=
                         mapM_ (\ x -> System.IO.hPutStrLn stderr (show x))

-- Never exits
test2 :: IO ()
test2 =
       readProcessChunks id (RawCommand "/usr/bin/yes" []) empty >>=
                         -- mapM (\ x -> hPutStrLn stderr (show x) >> return x) >>=
                         -- On my machine I get about 1 dot per second at 50M chars perdot
                         dots 50000000 (\ n -> hPutStr stderr (replicate (fromInteger (toInteger n)) '.')) >>=
                         hPutStrLn stderr . show . filter isResult
