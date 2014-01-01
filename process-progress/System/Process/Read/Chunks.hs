-- | The 'readProcessChunks' function is a process reader that returns
-- a list (stream) of 'Output', which represent chunks of text read
-- from 'Stdout', 'Stderr', a 'Result' code, or an 'Exception'.  This
-- has the advantage of preserving the order in which these things
-- appeared.  The 'foldOutput', 'foldOutputsL', and 'foldOutputsR'
-- functions can be used to process the output stream.  The output
-- text can be any 'NonBlocking' instance, which needs to be implemented
-- using a non-blocking read function like 'B.hGetSome'.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
module System.Process.Read.Chunks (
  NonBlocking(..),
  Output(..),
  foldOutput,
  foldOutputsL,
  foldOutputsR,
  readProcessChunks,
  readProcessChunks'
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData)
import Control.Exception as E (onException, catch, mask, try, throwIO, SomeException)
import Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.Word (Word8)
import qualified GHC.IO.Exception as E
import GHC.IO.Exception (IOErrorType(ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import qualified Prelude
import System.Exit (ExitCode)
import System.IO hiding (hPutStr, hGetContents)
import System.IO.Error (mkIOError)
import System.Process.Read (ListLikePlus(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(..), StdStream(CreatePipe), ProcessHandle,
                       createProcess, waitForProcess, terminateProcess)

-- | This lets us use deepseq's force on the stream of data returned
-- by the process-progress functions.
instance NFData ExitCode

-- | Class of types which can also be used by 'System.Process.Read.readProcessChunks'.
class ListLikePlus a c => NonBlocking a c where
  hGetSome :: Handle -> LengthType a -> IO a
  toChunks :: a -> [a]

data Output a = Stdout a | Stderr a | Result ExitCode | Exception IOError deriving (Eq, Show)

-- | A fold function for the Output type, dispatches the value
-- depending on the constructor.
foldOutput :: ListLikePlus a c =>
              (ExitCode -> b)
           -> (a -> b)
           -> (a -> b)
           -> (IOError -> b)
           -> Output a
           -> b
foldOutput codefn _ _ _ (Result code) = codefn code
foldOutput _ outfn _ _ (Stdout out) = outfn out
foldOutput _ _ errfn _ (Stderr err) = errfn err
foldOutput _ _ _ exnfn (Exception exn) = exnfn exn

foldOutputsL :: ListLikePlus a c =>
                (b -> ExitCode -> b)
             -> (b -> a -> b)
             -> (b -> a -> b)
             -> (b -> IOError -> b)
             -> b
             -> [Output a]
             -> b
foldOutputsL _ _ _ _ result [] = result
foldOutputsL codefn outfn errfn exnfn result (x : xs) =
    let result' = foldOutput (codefn result) (outfn result) (errfn result) (exnfn result) x in
    foldOutputsL codefn outfn errfn exnfn result' xs
    -- Pretty sure this is equivalent:
    -- foldl (\ r x -> foldOutput (codefn r) (outfn r) (errfn r) (exnfn r) x) result xs

foldOutputsR :: forall a b c. ListLikePlus a c =>
                (b -> ExitCode -> b)
             -> (b -> a -> b)
             -> (b -> a -> b)
             -> (b -> IOError -> b)
             -> b
             -> [Output a]
             -> b
foldOutputsR _ _ _ _ result [] = result
foldOutputsR codefn outfn errfn exnfn result (x : xs) =
    let result' = foldOutputsR codefn outfn errfn exnfn result xs in
    foldOutput (codefn result') (outfn result') (errfn result') (exnfn result') x
    -- foldr (\ x r -> foldOutput (codefn r) (outfn r) (errfn r) (exnfn r) x) result xs

-- | This is a process runner which (at the cost of a busy loop) gives
-- you the chunks of text read from stdout and stderr interleaved in
-- the order they were written, along with any ResourceVanished
-- exxception that might occur.  Its interface is similar to
-- 'System.Process.Read.readModifiedProcessWith', though the
-- implementation is somewhat alarming.
readProcessChunks :: (NonBlocking a c) =>
                     CreateProcess
                  -> a
                  -> IO [Output a]
readProcessChunks p input = mask $ \ restore -> do
  (Just inh, Just outh, Just errh, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

  binary input [inh, outh, errh]

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do

    waitOut <- forkWait $ elements pid (Just outh) (Just errh)

    -- now write and flush any input
    (do unless (null input) (hPutStr inh input >> hFlush inh)
        hClose inh) `E.catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | Take the info returned by 'createProcess' and gather and return
-- the stdout and stderr of the process.
elements :: NonBlocking a c => ProcessHandle -> Maybe Handle -> Maybe Handle -> IO [Output a]
elements pid outh errh =
    do case (outh, errh) of
         (Nothing, Nothing) ->
           -- EOF on both output descriptors, get exit code.  It can be
           -- argued that the result will always contain exactly one exit
           -- code if traversed to its end, because the only case of
           -- 'elements' that does not recurse is the one that adds a Result,
           -- and there is nowhere else where a Result is added.  However,
           -- the process doing the traversing may die before that end is
           -- reached.
           do result <- waitForProcess pid
              return [Result result]
         _ ->
           -- The available output has been processed, send input and read
           -- from the ready handles
           do (outh', errh', elems') <- ready uSecs outh errh
              more <- unsafeInterleaveIO (elements pid outh' errh')
              return $ elems' ++ more

data Readyness = Ready | Unready | Closed

hReady' :: Handle -> IO Readyness
hReady' h =
    -- This check is necessary because if we run hReady on a closed
    -- handle it will keep giving us EOF exceptions.
    hIsClosed h >>= checkReady
    where
      checkReady True = return Closed
      checkReady False =
          (hReady h >>= (\ flag -> return (if flag then Ready else Unready)))
            `catch` endOfFile (\ _ -> return Closed)

-- | Wait until at least one handle is ready and then read output.  If
-- none of the output descriptors are ready for reading the function
-- sleeps and tries again.
ready :: (NonBlocking a c) =>
         Int -> Maybe Handle -> Maybe Handle
      -> IO (Maybe Handle, Maybe Handle, [Output a])
ready waitUSecs outh errh =
    do
      outReady <- maybe (return Closed) hReady' outh
      errReady <- maybe (return Closed) hReady' errh
      case (outReady, errReady) of
        (Closed, Closed) ->
            return (Nothing, Nothing, [])
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        (Unready, Unready) ->
            do threadDelay waitUSecs
               ready (min maxUSecs (2 * waitUSecs)) outh errh
        _ ->
            -- One or both output handles are ready, try to read from
            -- them.  If (out1 ++ out2) is an empty list we know that
            -- we have reached EOF on both descriptors, because at
            -- least one was ready and nextOut only returns [] for a
            -- ready file descriptor on EOF.
            do (errh', out1) <- nextOut errh errReady Stderr
               (outh', out2) <- nextOut outh outReady Stdout
               return (outh', errh', out1 ++ out2)

-- | Return the next output element and the updated handle from a
-- handle which is assumed ready.  If the handle is closed or unready,
-- or we reach end of file an empty list of outputs is returned.
nextOut :: NonBlocking a c => Maybe Handle -> Readyness -> (a -> Output a) -> IO (Maybe Handle, [Output a])
nextOut Nothing _ _ = return (Nothing, [])
nextOut (Just h) Ready constructor =	-- Perform a read
   do -- We can call hGetSome because we know this handle is ready for reading.
      a <- hGetSome h (fromIntegral bufSize)
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose h
                return (Nothing, [])
        -- Got some output
        _n -> return (Just h, [constructor a])
nextOut h _ _ = return (h, [])	-- Handle is closed or not ready

endOfFile :: (IOError -> IO a) -> IOError -> IO a
endOfFile eeof e = if E.ioe_type e == E.EOF then eeof e else ioError e

bufSize :: Int
bufSize = 4096		-- maximum chunk size
uSecs :: Int
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs :: Int
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | A test version of readProcessChunks.
-- Pipes code here: http://hpaste.org/76631
readProcessChunks' :: (NonBlocking a c) =>
                      CreateProcess
                   -> a
                   -> IO [Output a]
readProcessChunks' p input = mask $ \ restore -> do
  (Just inh, Just outh, Just errh, pid) <-
      createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

  binary input [inh, outh, errh]

  flip onException
    (do hClose inh; hClose outh; hClose errh;
        terminateProcess pid; waitForProcess pid) $ restore $ do

    waitOut <- forkWait $ elements' pid outh errh

    -- now write and flush any input
    (do unless (null input) (hPutStr inh input >> hFlush inh)
        hClose inh) `catch` resourceVanished (\ _e -> return ())

    -- wait on the output
    waitOut

-- | An idea for a replacement of elements
elements' :: forall a c. NonBlocking a c => ProcessHandle -> Handle -> Handle -> IO [Output a]
elements' pid outh errh = do
  res <- newEmptyMVar
  -- We use Exception EOF to indicate that we reached EOF on one
  -- handle.  This is not a value that could otherwise have been put
  -- into the MVar.
  _outtid <- forkIO $ hGetContents outh >>= mapM_ (\ c -> putMVar res (Stdout c)) . toChunks >> hClose outh >> putMVar res (Exception (mkIOError E.EOF "EOF" Nothing Nothing))
  _errtid <- forkIO $ hGetContents errh >>= mapM_ (\ c -> putMVar res (Stderr c)) . toChunks >> hClose errh >> putMVar res (Exception (mkIOError E.EOF "EOF" Nothing Nothing))
  takeChunks 0 res
    where
      takeChunks :: Int -> MVar (Output a) -> IO [Output a]
      takeChunks 2 _ = waitForProcess pid >>= \ result -> return [Result result]
      takeChunks closedCount res = takeMVar res >>= takeChunk closedCount res
      takeChunk closedCount res (Exception _) = takeChunks (closedCount + 1) res
      takeChunk closedCount res x =
          do xs <- unsafeInterleaveIO $ takeChunks closedCount res
             return (x : xs)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e

{-
proc' :: CmdSpec -> CreateProcess
proc' (RawCommand cmd args) = proc cmd args
proc' (ShellCommand cmd) = shell cmd
-}

{- Its not safe to read a bytestring chunk and then convert it to a string, the chunk
   might end with part of a character.
instance NonBlocking String Char where
  -- hGetNonBlocking n h = (toString . B.concat . L.toChunks) <$> L.hGetNonBlocking n h
  hGetSome h n = toList <$> B.hGetSome h n
  toChunks = error "toChunks"
-}

instance NonBlocking B.ByteString Word8 where
  -- hGetNonBlocking = B.hGetNonBlocking
  hGetSome = B.hGetSome
  toChunks = (: [])

instance NonBlocking L.ByteString Word8 where
  -- hGetNonBlocking = L.hGetNonBlocking
  hGetSome h n = (L.fromChunks . (: [])) <$> B.hGetSome h (fromIntegral n)
  toChunks = Prelude.map (L.fromChunks . (: [])) . L.toChunks
