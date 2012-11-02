{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.Types.Download
    ( Download(..)
    , handle
    , method
    , flags
    ) where

import Data.Time (NominalDiffTime)
import qualified Data.ByteString.Lazy as L
import Data.Version (Version)
import Debian.AutoBuilder.Types.Packages (Packages, PackageFlag, RetrieveMethod(..))
import qualified Debian.AutoBuilder.Types.Packages as P
import System.Process.Read (Output)

data Download
    = Download
      { package :: Packages
      -- ^ the data provided about the package in the target list
      , getTop :: FilePath
      -- ^ The directory containing the target's files.  For most target types, these
      --  files could be anything, not necessarily a Debian source directory.
      , mVersion :: Maybe Version
      -- ^ Some targets can return a cabal version, use this to retrieve it.
      , origTarball :: Maybe FilePath
      -- ^ If we have access to an original tarball, this returns its path.
      , logText :: String
      -- ^ Text to include in changelog entry.
      , cleanTarget :: FilePath -> IO ([Output L.ByteString], NominalDiffTime)
      -- ^ Clean version control info out of a target after it has
      -- been moved to the given location.
      , buildWrapper :: forall a. IO a -> IO a
      -- ^ Modify the build process in some way - currently only the
      -- proc target modifies this by mounting and then unmounting /proc.
      }

-- | The name used to identify the package in the target list.
handle :: Download -> String
handle = P.name . package
-- | The method used to retrieve this target.
method :: Download -> RetrieveMethod
method = P.spec . package
-- | The flags assocated with the package
flags :: Download -> [PackageFlag]
flags  = P.flags . package
