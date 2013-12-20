{-# LANGUAGE OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Git where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, shell, CmdSpec(..), CreateProcess(cwd, cmdspec), showCommandForUser)
import System.Process.Progress (keepStdout, keepResult, timeTask, runProcessF, runProcess)
import System.Unix.Directory
import Text.Regex

documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> P.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (runProcess (cmd {cwd = Just path}) B.empty >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . B.concat . keepStdout) >>= 
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmd (cmdspec cmd) ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = proc "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd (RawCommand cmd args) = showCommandForUser cmd args
showCmd (ShellCommand cmd) = cmd

prepare :: MonadReposCached m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package theUri =
    sub "git" >>= \ base ->
    sub ("git" </> sum) >>= \ dir -> liftIO $
    do
      when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      _output <- fixLink base
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "git revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> let cmd = "find " ++ top ++ " -name '.git' -maxdepth 1 -prune | xargs rm -rf" in
                                       timeTask (runProcessF (Just (" 1> ", " 2> ")) (shell cmd) B.empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- runProcess ((proc "git" ["status", "--porcelain"]) {cwd = Just dir}) B.empty >>= return . keepResult

      -- CB  No output lines means no changes
      -- CB  git reset --hard    will remove all edits back to the most recent commit

      -- The status code does not reflect whether changes were made
             case result of
               (ExitSuccess : _) -> removeSource dir >> createSource dir		-- Yes changes
               _ -> updateSource dir				-- No Changes!
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          runProcessF (Just (" 1> ", " 2> ")) ((proc "git" ["pull", "--all", "--commit", renderForGit theUri']) {cwd = Just dir}) B.empty >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _output <- runProcessF (Just (" 1> ", " 2> ")) cmd B.empty
             findSourceTree dir
          where
            cmd = proc "git" (["clone", renderForGit theUri'] ++ maybe [] (\ branch -> [" --branch", "'" ++ branch ++ "'"]) theBranch ++ [dir])

      -- CB  git reset --hard    will remove all edits back to the most recent commit
      fixLink base =
          let link = base </> name in
          runProcessF (Just (" 1> ", " 2> ")) (proc "rm" ["-rf", link]) B.empty >>
          runProcessF (Just (" 1> ", " 2> ")) (proc "ln" ["-s", sum, link]) B.empty
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndBranch))
      -- Maybe we should include the "git:" in the string we checksum?  -- DSF
      -- Need more info to answer that, but addition of git makes it more likely. -- CB
      uriAndBranch = uriToString id theUri' "" ++ maybe "" (\ branch -> "=" ++ branch) theBranch
      theBranch = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                             P.GitBranch s -> Just s
                                                             _ -> Nothing) (P.flags package)))) of
                 [] -> Nothing
                 [x] -> Just x
                 xs -> error ("Conflicting branches for git clone of " ++ theUri ++ ": " ++ show xs)
      theUri' = mustParseURI theUri

renderForGit :: URI -> String
renderForGit uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      (_, _) -> show uri

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Darcs - failed to parse URI: " ++ show s)) id (parseURI s)
