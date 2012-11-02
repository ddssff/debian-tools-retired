#!/usr/bin/runhaskell

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Exception (SomeException, onException, evaluate, catch, try, throwIO, mask)
import Control.Monad (when)
import Control.Monad.State (StateT(runStateT), get, put)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode)
import System.IO (Handle, hClose, hFlush, hIsClosed, hPrint, hPutStr, hPutStrLn, stdout, stderr)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (ProcessHandle, runInteractiveCommand, terminateProcess, waitForProcess)

data Output = Stdout B.ByteString | Stderr B.ByteString | StdoutEOF | StderrEOF | Result ExitCode deriving (Eq, Show)

-- | An idea for a replacement of elements
elements :: Handle -> Handle -> IO [Output]
elements outh errh = do
  res <- newEmptyMVar
  forkIO $ L.hGetContents outh >>= mapM_ (\ c -> putMVar res (Stdout c)) . L.toChunks >> hClose outh >> putMVar res StdoutEOF
  forkIO $ L.hGetContents errh >>= mapM_ (\ c -> putMVar res (Stderr c)) . L.toChunks >> hClose errh >> putMVar res StderrEOF
  takeChunks False False res
    where
      takeChunks :: Bool -> Bool -> MVar Output -> IO [Output]
      takeChunks True True _ = return []
      takeChunks stdoutClosed stderrClosed res =
          do x <- takeMVar res
             xs <- unsafeInterleaveIO $ takeChunks (stdoutClosed || x == StdoutEOF) (stderrClosed || x == StderrEOF) res
             return (x : xs)

main :: IO ()
main = do
    (inh, outh, errh, pid) <- runInteractiveCommand cmd
    outputs <- elements outh errh >>= dots {-50000000-} 50 (\ n -> hPutStr stderr (replicate n '.') >> hFlush stderr) >>= dots {-5000000-} 50 (\ n -> hPutStr stderr (replicate n '*') >> hFlush stderr)
    result <- waitForProcess pid
    return ()
    where
      cmd = "yes | head -100000 | while read i; do echo stdout; echo stderr 1>&2; done"
      -- cmd = "yes"

doOutput [] = return ()
doOutput (Stdout s : more) = B.hPutStr stdout s >> doOutput more
doOutput (Stderr s : more) = B.hPutStr stderr s >> doOutput more
doOutput (_ : more) = doOutput more

dots :: Int -> (Int -> IO ()) -> [Output] -> IO [Output]
dots charsPerDot nDots outputs =
    dots' 0 outputs >> return outputs
    where
      dots' :: Int -> [Output] -> IO ()
      dots' _ [] = return ()
      dots' rem (x : xs) = do
        let (count', rem') = divMod (rem + foldOutput B.length B.length 0 x) charsPerDot
        when (count' > 0) (nDots count')
        dots' rem' xs
{-
    fst <$> runStateT (dots' outputs) 0
    where
      dots' [] = return []
      dots' (x : xs) = do
          rem <- get
          let (count', rem') = divMod (rem + foldOutput B.length B.length 0 x) charsPerDot
          when (count' > 0) (lift (nDots count'))
          put rem'
          xs' <- unsafeInterleaveIO (dots' xs)
          return (x : xs')
-}

foldOutput :: (B.ByteString -> b) -> (B.ByteString -> b) -> b -> Output -> b
foldOutput outfn _ _ (Stdout out) = outfn out
foldOutput _ errfn _ (Stderr err) = errfn err
foldOutput _ _ other _ = other
