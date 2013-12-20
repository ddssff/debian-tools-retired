{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import System.Directory
import System.FilePath (splitFileName, (</>))
import System.Process (shell)
import System.Process.Progress (timeTask, runProcessF)
import System.Unix.Directory

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

prepare :: MonadReposCached m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package archive =
    do
      dir <- sub ("hg" </> archive)
      when (P.flushSource (P.params cache)) (liftIO $ removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "Hg revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> 
                                  let cmd = "rm -rf " ++ path ++ "/.hg" in
                                  timeTask (runProcessF (Just (" 1> ", " 2> ")) (shell cmd) empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource dir =
          try (runProcessF (Just (" 1> ", " 2> ")) (shell ("cd " ++ dir ++ " && hg status | grep -q .")) empty) >>=
          either (\ (_ :: SomeException) -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runProcessF (Just (" 1> ", " 2> ")) (shell ("cd " ++ dir ++ " && hg pull -u")) empty >>
          findSourceTree dir :: IO SourceTree

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          runProcessF (Just (" 1> ", " 2> ")) (shell ("hg clone " ++ archive ++ " " ++ dir)) empty >>
          findSourceTree dir :: IO SourceTree
