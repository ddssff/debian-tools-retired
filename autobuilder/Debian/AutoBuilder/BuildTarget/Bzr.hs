-- | A Bazaar archive
{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Bzr where

import Control.Exception (SomeException, try, throw)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.URI
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Process (CmdSpec(..))
import System.Process.Progress (keepOutput, timeTask, runProcessF, qPutStrLn)
import System.Directory

documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

prepare :: P.CacheRec -> P.Packages -> String -> AptIOT IO Download
prepare cache package version = liftIO $
  do
    when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
    exists <- liftIO $ doesDirectoryExist dir
    tree <- if exists then updateSource dir else createSource dir
    return $ Download
               { package = package
               , getTop = topdir tree
               , logText = "Bazaar revision: " ++ show (P.spec package)
               , mVersion = Nothing
               , origTarball = Nothing
               , cleanTarget = \ top ->
                   do qPutStrLn ("Clean Bazaar target in " ++ top)
                      let cmd = "find '" ++ top ++ "' -name '.bzr' -prune | xargs rm -rf"
                      timeTask (runProcessF id (ShellCommand cmd) L.empty)
               , buildWrapper = id }
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            qPutStrLn ("Verifying Bazaar source archive '" ++ dir ++ "'") >>
            try (runProcessF id (ShellCommand cmd) L.empty) >>= \ result ->
            case result of
              -- if we fail then the source tree is corrupted, so get a new one
              Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir >> throw e
              -- If we succeed then we try to merge with the parent tree
              Right _output -> mergeSource dir
            where
                cmd   = "cd " ++ dir ++ " && ! `bzr status | grep -q 'modified:'`"
        
        -- computes a diff between this archive and some other parent archive and tries to merge the changes
        mergeSource dir =
            runProcessF id (ShellCommand cmd) L.empty >>= \ b ->
            if isInfixOf "Nothing to do." (L.unpack (L.concat (keepOutput b)))
            then findSourceTree dir
            else commitSource dir
            where
                cmd   = "cd " ++ dir ++ " && bzr merge"
                -- style = (setStart (Just ("Merging local Bazaar source archive '" ++ dir ++ "' with parent archive")).
                --          setError (Just (\ _ -> "bzr merge failed in '" ++ dir ++ "'")))
        
        -- Bazaar is a distributed revision control system so you must commit to the local source
        -- tree after you merge from some other source tree
        commitSource dir =
            runProcessF id (ShellCommand cmd) L.empty >> findSourceTree dir
            where
                cmd   = "cd " ++ dir ++ " && bzr commit -m 'Merged Upstream'"
                -- style = (setStart (Just ("Commiting merge to local Bazaar source archive '" ++ dir ++ "'")) .
                --     setError (Just (\ _ -> "bzr commit failed in '" ++ dir ++ "'")))
        
        removeSource dir = liftIO $ removeRecursiveSafely dir

        createSource dir = do
            -- Create parent dir and let bzr create dir
            let (parent, _) = splitFileName dir
            createDirectoryIfMissing True parent
            _output <- runProcessF id (ShellCommand cmd) L.empty
            findSourceTree dir
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                -- style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                --     setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
        dir = (P.topDir cache) ++ "/bzr/" ++ show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))))

