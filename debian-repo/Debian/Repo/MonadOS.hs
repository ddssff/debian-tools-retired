{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS
    , updateLists
    , withProc
    , withTmp
    , aptGetInstall
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (evaluate, SomeException)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask, throwM, try)
import Control.Monad.State (MonadState(get), StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Monoid ((<>))
import Data.Time (NominalDiffTime)
import Debian.Pretty (pretty)
import Debian.Relation (PkgName)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.OSImage (OSImage(osRoot))
import Debian.Repo.Prelude (readProc, runProc, symbol)
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((</>))
import System.Process (proc, readProcess)
import System.Process.Progress (keepResult, qPutStrLn, quieter, timeTask)
import System.Unix.Chroot (useEnv)

class (MonadState OSImage m, Monad m, Functor m) => MonadOS m

instance (Monad m, Functor m) => MonadOS (StateT OSImage m)

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m NominalDiffTime
updateLists = quieter 1 $
    do root <-rootPath . osRoot <$> get
       withProc $ liftIO $ do
         qPutStrLn ($(symbol 'updateLists) <> ": updating OSImage " ++ root)
         out <- useEnv root forceList (readProc update)
         _ <- case keepResult out of
                [ExitFailure _] ->
                    do _ <- useEnv root forceList (runProc configure)
                       useEnv root forceList (runProc update)
                _ -> return []
         (_, elapsed) <- timeTask (useEnv root forceList (runProc upgrade))
         return elapsed
    where
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

-- | Do an IO task in the build environment with /proc mounted.
withProc :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withProc task =
    do root <- rootPath . osRoot <$> get
       let proc = root </> "proc"
           sys = root </> "sys"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True proc >> readProcess "mount" ["--bind", "/proc", proc] "" >>
                         createDirectoryIfMissing True sys >> readProcess "mount" ["--bind", "/sys", sys] "")
           post :: String -> m String
           post _s = liftIO $ readProcess "umount" [proc] "" >> readProcess "umount" [sys] ""
           task' :: String -> m c
           task' _s = task
       bracket pre post task'

-- | Do an IO task in the build environment with /proc mounted.
withTmp :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withTmp task =
    do root <- rootPath . osRoot <$> get
       let dir = root </> "tmp"
           pre :: m String
           pre = liftIO (createDirectoryIfMissing True dir >> readProcess "mount" ["--bind", "/tmp", dir] "")
           post :: String -> m String
           post _ = liftIO $ readProcess "umount" [dir] ""
           task' :: String -> m c
           task' _ = try task >>= either (\ (e :: SomeException) -> throwM e) return
       bracket pre post task'

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- rootPath . osRoot <$> get
       liftIO $ useEnv root (return . force) $ do
         _ <- runProc p
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
