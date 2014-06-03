{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.GHC
    ( withGHCVersion
    -- , ghcNewestAvailableVersion
    -- , ghcNewestAvailableVersion'
    -- , compilerIdFromDebianVersion
    ) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Version (showVersion, Version(Version))
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerId(CompilerId), CompilerFlavor(GHC))
import System.Directory (doesDirectoryExist)
import System.Process (readProcess)
import System.Unix.Chroot (useEnv)

withGHCVersion :: MonadIO m => FilePath -> (CompilerId -> m a) -> m a
withGHCVersion root f = liftIO (ghcNewestAvailableVersion' root) >>= f

-- | The IO portion of ghcVersion.  For there to be no version of ghc
-- available is an exceptional condition, it has been standard in
-- Debian and Ubuntu for a long time.
ghcNewestAvailableVersion :: FilePath -> IO DebianVersion
ghcNewestAvailableVersion root = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "ghcVersion: no such environment: " ++ show root)
  versions <- try $ chroot $
                (readProcess "apt-cache" ["showpkg", "ghc"] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "ghcNewestAvailableVersion failed in " ++ show root ++ ": " ++ show e
    Right (_ : versionLine : _) -> return . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> error $ "No version of ghc available in " ++ show root
    where
      chroot = case root of
                 "/" -> id
                 _ -> useEnv root (return . force)

ghcNewestAvailableVersion' :: FilePath -> IO CompilerId
ghcNewestAvailableVersion' root =
    do ver <- ghcNewestAvailableVersion root
       let cid = compilerIdFromDebianVersion ver
       -- hPutStrLn stderr ("GHC Debian version: " ++ show ver ++ ", Compiler ID: " ++ show cid)
       return cid

compilerIdFromDebianVersion :: DebianVersion -> CompilerId
compilerIdFromDebianVersion debVersion =
    let (Version ds ts) = greatestLowerBound debVersion (map (\ d -> Version [d] []) [0..]) in
    CompilerId GHC (greatestLowerBound debVersion (map (\ d -> Version (ds ++ [d]) ts) [0..]))
#if MIN_VERSION_Cabal(1,21,0)
               Nothing
#endif
    where
      greatestLowerBound :: DebianVersion -> [Version] -> Version
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion (showVersion v) < b) xs
