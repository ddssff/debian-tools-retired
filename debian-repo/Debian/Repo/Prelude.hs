{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Prelude
    ( countTasks
    , nub'
    , access
    , (~=)
    , (%=)
    , symbol
    , runProc
    , readProc
    , rsync
    , checkRsyncExitCode
    , Pretty(..)
    , partitionM
    , maybeWriteFile		-- writeFileUnlessSame
    , replaceFile
    , cond
    , listIntersection
    ) where

import Control.Exception (catch, try)
import Control.Monad (foldM)
import Control.Monad.State (get, modify, MonadIO, MonadState)
import qualified Data.ByteString.Lazy as B (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L (empty)
import Data.Lens.Lazy (getL, Lens, modL)
import Data.List as List (group, intersect, map, sort)
import Language.Haskell.TH (Exp(LitE), Lit(StringL), Name, nameBase, nameModule, Q)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Error (isDoesNotExistError)
import System.Process (CreateProcess, proc)
import System.Process.Progress (ePutStrLn, keepResult, keepResult, runProcessF)
import System.Process.Read.Chunks (Output)
import System.Process.Read.Verbosity (quieter, runProcess)
import Text.PrettyPrint.ANSI.Leijen (Doc, text)
import Text.Printf (printf)

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task

-- | This nub doesn't preserve order
nub' :: (Ord a) => [a] -> [a]
nub' = List.map head . group . sort

access :: MonadState a m => Lens a b -> m b
access l = get >>= return . getL l

(~=) :: MonadState a m => Lens a b -> b -> m ()
l ~= x = l %= const x

-- | Modify a value.  (This is a version of Data.Lens.Lazy.%= that returns () instead of a.)
(%=) :: MonadState a m => Lens a b -> (b -> b) -> m ()
l %= f = modify (modL l f)

-- | Build a string containing a symbol's fully qualified name (for debugging output.)
symbol :: Name -> Q Exp
symbol x = return $ LitE (StringL (maybe "" (++ ".") (nameModule x) ++ nameBase x))

-- | Convenience function for running shell commands
--    1. Runs quietly by default, but that can be controlled by the VERBOSITY environement variable
--    2. If it exits with an error code all its output is echoed (with prefixes) and an exception is thrown
--    3. The process input is the empty string
--    4. When the output is echoed stdout is prefixed with "1>" and stderr with "2>".
--    5. The output is a stream of 'Output'
runProc :: MonadIO m => CreateProcess -> m [Output B.ByteString]
runProc p = quieter 1 $ runProcessF prefixes p L.empty

-- | Like runProc, but does not raise an exception when process exit code is not 0.
readProc :: MonadIO m => CreateProcess -> m [Output B.ByteString]
readProc p = quieter 1 $ runProcess p L.empty

prefixes :: Maybe (B.ByteString, B.ByteString)
prefixes = Just (" 1> ", " 2> ")

rsync :: (Functor m, MonadIO m) => [String] -> FilePath -> FilePath -> m ExitCode
rsync extra source dest =
    do result <- runProc (proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                                        [dropTrailingPathSeparator source ++ "/",
                                         dropTrailingPathSeparator dest])) >>= return . keepResult
       case result of
         [x] -> return x
         _ -> error "Missing or multiple exit codes"

checkRsyncExitCode :: Monad m => ExitCode -> m ()
checkRsyncExitCode ExitSuccess = return ()
checkRsyncExitCode (ExitFailure n) =
    case n of
      1 -> error "rsync: Syntax or usage error"
      2 -> error "rsync: Protocol incompatibility"
      3 -> error "rsync: Errors selecting input/output files, dirs"
      4 -> error "rsync: Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server."
      5 -> error "rsync: Error starting client-server protocol"
      6 -> error "rsync: Daemon unable to append to log-file"
      10 -> error "rsync: Error in socket I/O"
      11 -> error "rsync: Error in file I/O"
      12 -> error "rsync: Error in rsync protocol data stream"
      13 -> error "rsync: Errors with program diagnostics"
      14 -> error "rsync: Error in IPC code"
      20 -> error "rsync: Received SIGUSR1 or SIGINT"
      21 -> error "rsync: Some error returned by waitpid()"
      22 -> error "rsync: Error allocating core memory buffers"
      23 -> error "Partial transfer due to error"
      24 -> error "Partial transfer due to vanished source files"
      25 -> error "rsync: The --max-delete limit stopped deletions"
      30 -> error "rsync: Timeout in data send/receive"
      35 -> error "rsync: Timeout waiting for daemon connection"
      _ -> error $ "rsync: Unexpected failure " ++ show n

-- | This is a private Pretty class that doesn't have built-in instances
-- for tuples or lists or anything else.
class Pretty a where
    pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty = text . show .map pretty

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs =
    foldM f ([], []) xs
    where f (a, b) x = p x >>= (\ flag -> return $ if flag then (x : a, b) else (a, x : b))

-- | Write a file if its content is different from the given text.
maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile path s =
    try (readFile path) >>= maybeWrite
    where
      maybeWrite (Left (e :: IOError)) | isDoesNotExistError e = writeFile path s
      maybeWrite (Left e) = error ("maybeWriteFile: " ++ show e)
      maybeWrite (Right old) | old == s = return ()
      maybeWrite (Right _old) =
          --hPutStrLn stderr ("Old text: " ++ show old) >>
          --hPutStrLn stderr ("New text: " ++ show text) >>
          replaceFile path s

-- Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.  Apparently there
-- is a race condition in the file system so we may get one or more
-- isAlreadyBusyError exceptions before the writeFile succeeds.
replaceFile :: FilePath -> String -> IO ()
replaceFile path s =
    --tries 100 10 f	-- There is now a fix for this problem, see ghc ticket 2122.
    f
    where
      f :: IO ()
      f = removeFile path `Control.Exception.catch` (\ e -> if isDoesNotExistError e then return () else ioError e) >> writeFile path s

cond :: a -> a -> Bool -> a
cond t _ True = t
cond _ f False = f

listIntersection :: Eq a => [[a]] -> [a]
listIntersection [] = []
listIntersection (first : rest) = foldr intersect first rest
