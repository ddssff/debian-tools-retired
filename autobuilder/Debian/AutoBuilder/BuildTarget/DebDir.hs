{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (empty, pack)
import Data.Digest.Pure.MD5 (md5)
import Data.Version (showVersion)
import Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Version (version)
import Prelude hiding (catch)
import Debian.Repo
import System.Directory
import System.FilePath ((</>))
import System.Process (CmdSpec(..))
import System.Process.Progress (runProcessF)

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

prepare :: MonadDeb m => P.Packages -> T.Download -> T.Download -> m T.Download
prepare package upstream debian =
    sub "deb-dir" >>= \ dir ->
    sub ("deb-dir" </> show (md5 (pack (show (P.spec package))))) >>= \ dest ->
    liftIO (createDirectoryIfMissing True dir) >>
    copyUpstream dest >>
    copyDebian dest >>
    liftIO (findDebianSourceTree dest) >>= \ tree ->
    let tgt = T.Download {
                T.package = package
              , T.getTop = topdir tree
              , T.logText = "deb-dir revision: " ++ show (P.spec package)
              , T.mVersion = Nothing
              , T.origTarball = T.origTarball upstream
              , T.cleanTarget = \ _ -> return ([], 0)
              , T.buildWrapper = id
              } in
    -- The upstream and downstream versions must match after the epoch and revision is stripped.
    case T.mVersion upstream of
      Nothing -> return tgt
      Just upstreamV ->
          let debianV = logVersion (entry tree) in
          case compare (version debianV) (showVersion upstreamV) of
            -- If the debian version is too old it needs to be bumped, this ensures we notice
            -- when a new upstream appears.  We should just modify the changelog directly.
            LT -> error $ show (P.spec package) ++ ": version in Debian changelog (" ++ version debianV ++ ") is too old for the upstream (" ++ showVersion upstreamV ++ ")"
            _ -> return tgt
    where
      copyUpstream dest = runProcessF id (ShellCommand (cmd1 dest)) empty
      copyDebian dest = runProcessF id (ShellCommand (cmd2 dest)) empty
      upstreamDir = T.getTop upstream
      debianDir = T.getTop debian
      cmd1 dest = ("set -x && rsync -aHxSpDt --delete '" ++ upstreamDir ++ "/' '" ++ dest ++ "'")
      cmd2 dest = ("set -x && rsync -aHxSpDt --delete '" ++ debianDir ++ "/debian' '" ++ dest ++ "/'")
