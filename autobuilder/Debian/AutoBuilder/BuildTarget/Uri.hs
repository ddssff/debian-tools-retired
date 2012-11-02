{-# LANGUAGE ScopedTypeVariables #-}
-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module Debian.AutoBuilder.BuildTarget.Uri
    ( documentation
    , prepare
    , tarball
    , sourceDir
    ) where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (empty, readFile)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.Repo as R
import Debian.URI
import Magic
import System.FilePath (splitFileName)
import System.Directory
import System.Process (CmdSpec(..))
import System.Process.Progress (timeTask, runProcessF)
import System.Unix.Directory

documentation = [ "uri:<string>:<md5sum> - A target of this form retrieves the file at the"
                , "given URI, which is assumed to be a gzipped tarball.  The optional md5sum"
                , "suffix causes the build to fail if the downloaded file does not match"
                , "this checksum.  This prevents builds when the remote tarball has changed." ]

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
prepare :: P.CacheRec -> P.Packages -> String -> String -> R.AptIOT IO T.Download
prepare c package u s = liftIO $
    do (uri, sum, tree) <- checkTarget >>= downloadTarget >> validateTarget >>= unpackTarget
       return $ T.Download { T.package = package
                           , T.getTop = R.topdir tree
                           , T.logText = "Built from URI download " ++ (uriToString' uri)
                           , T.mVersion = Nothing
                           , T.origTarball = Just (tarball c (uriToString' uri) sum)
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id }
    where
      checkTarget =
          do exists <- doesFileExist (tarball c u s)
             case exists of
               True -> 
                   do realSum <- try (B.readFile (tarball c u s) >>= return . show . md5) :: IO (Either SomeException String)
                      case realSum of
                        Right realSum | realSum == s -> return True
                        _ -> removeRecursiveSafely (tarball c u s ) >> return False
               False -> return False

      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: Bool -> IO ()
      downloadTarget True = return ()
      downloadTarget False =
          do when (P.flushSource (P.params c)) (removeRecursiveSafely (sumDir c s))
             createDirectoryIfMissing True (sumDir c s)
             exists <- doesFileExist (tarball c u s)
             _output <-
                 case exists of
                   True -> return []
                   False -> runProcessF id (ShellCommand ("curl -s '" ++ uriToString' (mustParseURI u) ++ "' > '" ++ tarball c u s ++ "'")) B.empty
             -- We should do something with the output
             return ()
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: IO String
      validateTarget =
          try (B.readFile (tarball c u s) >>= return . show . md5) >>= \ (realSum :: Either SomeException String) ->
          case realSum of
            Right realSum | realSum == s -> return realSum
            Right realSum -> error ("Checksum mismatch for " ++ tarball c u s ++ ": expected " ++ s ++ ", saw " ++ realSum ++ ".")
            Left msg -> error ("Checksum failure for " ++ tarball c u s ++ ": " ++ show msg)
      unpackTarget realSum =
          rmdir >> mkdir >> untar >>= read >>= search >>= verify
          where
            rmdir = liftIO (try (removeDirectoryRecursive (sourceDir c s)) >>= either (\ (_ :: SomeException) -> return ()) return)
            -- Create the unpack directory
            mkdir = liftIO (try (createDirectoryIfMissing True (sourceDir c s)) >>=
                            either (\ (e :: SomeException) -> error ("Could not create " ++ sourceDir c s ++ ": " ++ show e)) return)
            untar =
                do magic <- magicOpen []
                   magicLoadDefault magic
                   fileInfo <- magicFile magic (tarball c u s)
                   case () of
                     _ | isPrefixOf "Zip archive data" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("unzip " ++ tarball c u s ++ " -d " ++ sourceDir c s)) B.empty
                       | isPrefixOf "gzip" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("tar xfz " ++ tarball c u s ++ " -C " ++ sourceDir c s)) B.empty
                       | isPrefixOf "bzip2" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("tar xfj " ++ tarball c u s ++ " -C " ++ sourceDir c s)) B.empty
                       | True ->
                           timeTask $ runProcessF id (ShellCommand ("cp " ++ tarball c u s ++ " " ++ sourceDir c s ++ "/")) B.empty
            read (_output, _elapsed) = liftIO (getDir (sourceDir c s))
            getDir dir = getDirectoryContents dir >>= return . filter (not . flip elem [".", ".."])
            search files = checkContents (filter (not . flip elem [".", ".."]) files)
            checkContents :: [FilePath] -> IO R.SourceTree
            checkContents [] = error ("Empty tarball? " ++ show (mustParseURI u))
            checkContents [subdir] =
                try (R.findSourceTree (sourceDir c s ++ "/" ++ subdir)) >>=
                either (\ (_ :: SomeException) -> R.findSourceTree (sourceDir c s)) return
            checkContents _ = R.findSourceTree (sourceDir c s)
            verify tree = return (mustParseURI u, realSum, tree)

sumDir c s = P.topDir c ++ "/tmp/" ++ s

tname u = snd . splitFileName . uriPath $ (mustParseURI u)

tarball c u s = sumDir c s ++ "/" ++ tname u
sourceDir c s = sumDir c s ++ "/unpack"

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Uri - parse failure: " ++ show s)) id (parseURI s)
