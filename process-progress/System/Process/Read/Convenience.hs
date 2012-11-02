{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module System.Process.Read.Convenience
    ( -- * Predicates
      isResult
    , isStdout
    , isStderr
    , isOutput
    , isException
    -- * Filters
    , discardStdout
    , discardStderr
    , discardOutput
    , discardExceptions
    , discardResult
    , keepStdout
    , keepStderr
    , keepOutput
    , keepExceptions
    , keepResult
    -- * Transformers
    , mergeToStdout
    , mergeToStderr
    , mapMaybeResult
    , mapMaybeStdout
    , mapMaybeStderr
    , mapMaybeException
    -- * Collectors
    , collectOutputs
    , unpackOutputs
    -- * IO operations
    , ePutStr
    , ePutStrLn
    , eMessage
    , eMessageLn

    , foldException
    , foldChars
    , foldStdout
    , foldStderr
    , foldResult
    , foldSuccess
    , foldFailure

    , doException
    , doOutput
    , doStdout
    , doStderr
    , doExit
    , doAll

    , dots
    , prefixed
    ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (mapMaybe)
import Prelude hiding (length, rem, concat)
import System.Exit (ExitCode(..), exitWith)
import System.IO (stdout, stderr)
import qualified System.IO as IO (hPutStr, hPutStrLn)
import System.Process.Read.Chars (Chars(..))
import System.Process.Read.Chunks (NonBlocking(..), Output(..), foldOutput, foldOutputsR)
import System.Process.Read.Instances ()

isResult :: Chars a => Output a -> Bool
isResult = foldOutput (const True) (const False) (const False) (const False)
isStdout :: Chars a => Output a -> Bool
isStdout = foldOutput (const False) (const True) (const False) (const False)
isStderr :: Chars a => Output a -> Bool
isStderr = foldOutput (const False) (const False) (const True) (const False)
isOutput :: Chars a => Output a -> Bool
isOutput = foldOutput (const False) (const True) (const True) (const False)
isException :: Chars a => Output a -> Bool
isException = foldOutput (const False) (const False) (const False) (const True)

toStdout :: Chars a => Output a -> Output a
toStdout = foldOutput Result Stdout Stdout Exception
toStderr :: Chars a => Output a -> Output a
toStderr = foldOutput Result Stderr Stderr Exception

mergeToStdout :: Chars a => [Output a] -> [Output a]
mergeToStdout = map toStdout
mergeToStderr :: Chars a => [Output a] -> [Output a]
mergeToStderr = map toStderr

discardStdout :: Chars a => [Output a] -> [Output a]
discardStdout = filter (not . isStdout)
discardStderr :: Chars a => [Output a] -> [Output a]
discardStderr = filter (not . isStderr)
discardOutput :: Chars a => [Output a] -> [Output a]
discardOutput = filter (\ x -> not (isStdout x || isStderr x))
discardExceptions :: Chars a => [Output a] -> [Output a]
discardExceptions = filter (not . isException)
discardResult :: Chars a => [Output a] -> [Output a]
discardResult = filter (not . isResult)

keepStdout :: Chars a => [Output a] -> [a]
keepStdout = mapMaybe $ foldOutput (const Nothing) Just (const Nothing) (const Nothing)
keepStderr :: Chars a => [Output a] -> [a]
keepStderr = mapMaybe $ foldOutput (const Nothing) (const Nothing) Just (const Nothing)
keepOutput :: Chars a => [Output a] -> [a]
keepOutput = mapMaybe $ foldOutput (const Nothing) Just Just (const Nothing)
keepExceptions :: Chars a => [Output a] -> [IOError]
keepExceptions = mapMaybe $ foldOutput (const Nothing) (const Nothing) (const Nothing) Just
keepResult :: Chars a => [Output a] -> [ExitCode]
keepResult = mapMaybe $ foldOutput Just (const Nothing) (const Nothing) (const Nothing)

mapMaybeResult :: Chars a => (ExitCode -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeResult f = mapMaybe (foldOutput f (Just . Stdout) (Just . Stderr) (Just . Exception))
mapMaybeStdout :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStdout f = mapMaybe (foldOutput (Just . Result) f (Just . Stderr) (Just . Exception))
mapMaybeStderr :: Chars a => (a -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeStderr f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) f (Just . Exception))
mapMaybeException :: Chars a => (IOError -> Maybe (Output a)) -> [Output a] -> [Output a]
mapMaybeException f = mapMaybe (foldOutput (Just . Result) (Just . Stdout) (Just . Stderr) f)

collectOutputs :: forall a. Chars a => [Output a] -> ([ExitCode], a, a, [IOError])
collectOutputs xs =
    foldOutputsR codefn outfn errfn exnfn result0 xs
    where
      result0 :: ([ExitCode], a, a, [IOError])
      result0 = ([], empty, empty, [])
      codefn :: ([ExitCode], a, a, [IOError]) -> ExitCode -> ([ExitCode], a, a, [IOError])
      codefn (codes, outs, errs, exns) code = (code : codes, outs, errs, exns)
      outfn :: ([ExitCode], a, a, [IOError]) -> a -> ([ExitCode], a, a, [IOError])
      outfn (codes, outs, errs, exns) out = (codes, append out outs, errs, exns)
      errfn :: ([ExitCode], a, a, [IOError]) -> a -> ([ExitCode], a, a, [IOError])
      errfn (codes, outs, errs, exns) err = (codes, outs, append err errs, exns)
      exnfn :: ([ExitCode], a, a, [IOError]) -> IOError -> ([ExitCode], a, a, [IOError])
      exnfn (codes, outs, errs, exns) exn = (codes, outs, errs, exn : exns)

unpackOutputs :: forall a. Chars a => [Output a] -> ([ExitCode], String, String, [IOError])
unpackOutputs xs =
    (codes, toString outs, toString errs, exns)
    where
      (codes, outs, errs, exns) = collectOutputs xs

ePutStr :: MonadIO m => String -> m ()
ePutStr s = liftIO $ IO.hPutStr stderr s

ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn s = liftIO $ IO.hPutStrLn stderr s

eMessage :: MonadIO m => String -> a -> m a
eMessage s x = ePutStr s >> return x

eMessageLn :: MonadIO m => String -> a -> m a
eMessageLn s x = ePutStrLn s >> return x

foldException :: Chars a => (IOError -> IO (Output a)) -> [Output a] -> IO [Output a]
foldException exnfn = mapM (foldOutput (return . Result) (return . Stdout) (return . Stderr) exnfn)

foldChars :: Chars a => (a -> IO (Output a)) -> (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldChars outfn errfn = mapM (foldOutput (return . Result) outfn errfn (return . Exception))

foldStdout :: Chars a => (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldStdout outfn = foldChars outfn (return . Stderr)

foldStderr :: Chars a => (a -> IO (Output a)) -> [Output a] -> IO [Output a]
foldStderr errfn = foldChars (return . Stdout) errfn

foldResult :: Chars a => (ExitCode -> IO (Output a)) -> [Output a] -> IO [Output a]
foldResult codefn = mapM (foldOutput codefn (return . Stdout) (return . Stderr) (return . Exception))

foldFailure :: Chars a => (Int -> IO (Output a)) -> [Output a] -> IO [Output a]
foldFailure failfn = foldResult codefn
    where codefn (ExitFailure n) = failfn n
          codefn x = return (Result x)

foldSuccess :: Chars a => IO (Output a) -> [Output a] -> IO [Output a]
foldSuccess successfn = foldResult codefn
    where codefn ExitSuccess = successfn
          codefn x = return (Result x)

doException :: Chars a => [Output a] -> IO [Output a]
doException = foldException throw

doOutput :: Chars a => [Output a] -> IO [Output a]
doOutput = foldChars (\ cs -> hPutStr stdout cs >> return (Stdout cs)) (\ cs -> hPutStr stderr cs >> return (Stderr cs))

doStdout :: Chars a => [Output a] -> IO [Output a]
doStdout = foldStdout (\ cs -> hPutStr stdout cs >> return (Stdout cs))

doStderr :: Chars a => [Output a] -> IO [Output a]
doStderr = foldStderr (\ cs -> hPutStr stderr cs >> return (Stderr cs))

-- | I don't see much use for this.
doExit :: Chars a => [Output a] -> IO [Output a]
doExit = foldResult (\ code -> exitWith code >> return (Result code))

doAll :: Chars a => [Output a] -> IO [Output a]
doAll = mapM (foldOutput (\ code -> exitWith code >> return (Result code))
                         (\ cs -> hPutStr stdout cs >> return (Stdout cs))
                         (\ cs -> hPutStr stderr cs >> return (Stderr cs))
                         throw)

dots :: forall a. NonBlocking a => LengthType a -> (LengthType a -> IO ()) -> [Output a] -> IO [Output a]
dots charsPerDot nDots outputs =
    nDots 1 >> dots' 0 outputs >>= eMessage "\n"
    where
      dots' _ [] = nDots 1 >> return []
      dots' rem (x : xs) =
          do let (count', rem') = divMod (rem + foldOutput (const 0) length length (const 0) x) charsPerDot
             when (count' > 0) (nDots count')
             xs' <- dots' rem' xs
             return (x : xs')

-- | Output the stream with a prefix added at the beginning of each
-- line of stdout and stderr.
prefixed :: forall a. Chars a => String -> String -> [Output a] -> IO [Output a]
prefixed opre epre output =
    mapM (\ (old, new) -> doOutput [new] >> return old) (prefixes opre epre output)

-- | Return the original stream of outputs zipped with one that has
-- had prefixes for stdout and stderr inserted.  For the prefixed
-- stream only, apply @map snd@.
prefixes :: forall a. Chars a => String -> String -> [Output a] -> [(Output a, Output String)]
prefixes opre epre output =
    loop True output
    where
      loop :: Bool -> [Output a] -> [(Output a, Output String)]
      loop _ [] = []
      loop bol (x@(Stdout s) : xs) = let (s', bol') = step bol opre (toString s) in (x, Stdout s') : loop bol' xs
      loop bol (x@(Stderr s) : xs) = let (s', bol') = step bol epre (toString s) in (x, Stderr s') : loop bol' xs
      loop bol (x : xs) = (x, Stdout "") : loop bol xs

      step :: Bool -> String -> String -> (String, Bool)
      step bol pre s =
          let (a, b) = span (/= '\n') s in
          if a == ""
          then if b == ""
               then ("", bol)
               else let x = (if bol then pre else "")
                        (s', bol') = step True pre (tail b) in
                    (x ++ "\n" ++ s', bol')
          -- There is some text before a possible newline
          else let x = (if bol then pre ++ a else a)
                   (s', bol') = step False pre b in
               (x ++ s', bol')
