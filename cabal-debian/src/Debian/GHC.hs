{-# OPTIONS_GHC -Wall #-}
module Debian.GHC
    ( ghcNewestAvailableVersion
    , ghcNewestAvailableVersion'
    , compilerIdFromDebianVersion
    ) where

import Control.DeepSeq (force)
import Control.Monad (when)
import Data.Version (showVersion, Version(Version))
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerId(CompilerId), CompilerFlavor(GHC))
import System.Directory (doesDirectoryExist)
import System.Process (readProcess)
import System.Unix.Chroot (useEnv)

-- | The IO portion of ghcVersion.
ghcNewestAvailableVersion :: FilePath -> IO (Maybe DebianVersion)
ghcNewestAvailableVersion root = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "ghcVersion: no such environment: " ++ show root)
  versions <- chroot $
                (readProcess "apt-cache" ["showpkg", "ghc"] "" >>=
                return . dropWhile (/= "Versions: ") . lines)
  case versions of
    (_ : versionLine : _) -> return . Just . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> return Nothing
    where
      chroot = case root of
                 "/" -> id
                 _ -> useEnv root (return . force)

ghcNewestAvailableVersion' :: FilePath -> IO (Maybe CompilerId)
ghcNewestAvailableVersion' root =
    ghcNewestAvailableVersion root >>= return . maybe Nothing (Just . compilerIdFromDebianVersion)

compilerIdFromDebianVersion :: DebianVersion -> CompilerId
compilerIdFromDebianVersion debVersion =
    let (Version ds ts) = greatestLowerBound debVersion (map (\ d -> Version [d] []) [0..]) in
    CompilerId GHC (greatestLowerBound debVersion (map (\ d -> Version (ds ++ [d]) ts) [0..]))
    where
      greatestLowerBound :: DebianVersion -> [Version] -> Version
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion (showVersion v) < b) xs
