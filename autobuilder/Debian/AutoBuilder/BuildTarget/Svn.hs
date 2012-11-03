{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( prepare
    , documentation
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.URI
import System.Directory
import System.Exit
import System.FilePath (splitFileName)
import System.Process (CmdSpec(..))
import System.Process.Progress (Output, keepStdout, keepStderr, keepResult, timeTask, runProcessF, runProcess)
import System.Unix.Directory

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn :: [String] -> IO [Output L.ByteString]
svn args = runProcessF id (RawCommand "svn" args) L.empty

username userInfo = 
    let un = takeWhile (/= ':') userInfo in
    if null un
    then []
    else ["--username", unEscapeString un]
     
password userInfo =
    let pw = takeWhile (/= '@') . dropWhile (== ':') . dropWhile (/= ':') $ userInfo in
    if null pw
    then []
    else ["--password",unEscapeString pw]

prepare :: MonadApt e m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package uri = liftIO $
    do when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       return $ T.Download { T.package = package
                           , T.getTop = topdir tree
                           , T.logText =  "SVN revision: " ++ show (P.spec package)
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget =
                               \ path -> 
                                   let cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf" in
                                   timeTask (runProcessF id (ShellCommand cmd) L.empty)
                           , T.buildWrapper = id
                           }
    where
      uri' = mustParseURI uri
      verifySource dir =
          svn (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>= \ out ->
          case L.concat (keepStdout out ++ keepStderr out) == L.empty of
            -- no output == nothing changed
            True -> updateSource dir
            -- Failure - error code or output from status means changes have occured
            False ->  removeSource dir >> createSource dir

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            _output <- svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree dir

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          checkout >>
          findSourceTree dir
      checkout :: IO (Either String [Output L.ByteString])
      --checkout = svn createStyle args 
      checkout = runProcess id (RawCommand "svn" args) L.empty >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++ 
                    (username userInfo) ++ (password userInfo) ++ 
                    [ uri, dir ])
            finish output = case keepResult output of
                              [ExitSuccess] -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      userInfo = maybe "" uriUserInfo (uriAuthority uri')
      dir = P.topDir cache ++ "/svn/" ++ show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri') ++ (uriPath uri'))))

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Svn - parse failure: " ++ show s)) id (parseURI s)
