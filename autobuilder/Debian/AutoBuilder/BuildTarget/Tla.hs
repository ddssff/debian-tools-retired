{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import System.FilePath (splitFileName, (</>))
import System.Unix.Directory
import System.Process (shell)
import System.Process.Progress (timeTask, runProcessF, qPutStrLn)
import System.Directory

documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

prepare :: MonadDeb m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package version =
    do
      dir <- sub ("tla" </> version)
      when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "TLA revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> 
                                  let cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf" in
                                  timeTask (runProcessF (Just (" 1> ", " 2> ")) (shell cmd) L.empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource dir =
          do result <- try (runProcessF (Just (" 1> ", " 2> ")) (shell ("cd " ++ dir ++ " && tla changes")) L.empty)
             case result of
               Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir						         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runProcessF (Just (" 1> ", " 2> ")) (shell ("cd " ++ dir ++ " && tla update " ++ version)) L.empty >>
             -- At one point we did a tla undo here.  However, we are
             -- going to assume that the "clean" copies in the cache
             -- directory are clean, since some of the other target
             -- types have no way of doing this reversion.
          findSourceTree dir :: IO SourceTree

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            liftIO $ createDirectoryIfMissing True parent
            _output <- runProcessF (Just (" 1> ", " 2> ")) (shell ("tla get " ++ version ++ " " ++ dir)) L.empty
            findSourceTree dir
