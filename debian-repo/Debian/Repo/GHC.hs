module Debian.Repo.GHC
    ( ghcNewestAvailableVersion
    ) where

import Control.DeepSeq (force)
import Debian.Version (DebianVersion, parseDebianVersion)
import System.Process (readProcess)
import System.Unix.Chroot (useEnv)

-- | The IO portion of ghcVersion.
ghcNewestAvailableVersion :: FilePath -> IO (Maybe DebianVersion)
ghcNewestAvailableVersion root = do
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
