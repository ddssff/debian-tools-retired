module Debian.Debianize.Types
    ( PackageInfo(..)
    , Site(..)
    , Server(..)
    , InstallFile(..)
    , VersionSplits(..)
    ) where

import Data.Map as Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Version (Version(Version))
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName(PackageName))
import Prelude hiding (init, unlines, log)

data PackageInfo = PackageInfo { cabalName :: PackageName
                               , devDeb :: Maybe (BinPkgName, DebianVersion)
                               , profDeb :: Maybe (BinPkgName, DebianVersion)
                               , docDeb :: Maybe (BinPkgName, DebianVersion) } deriving (Eq, Ord, Show)

-- | Information about the web site we are packaging.
data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      , server :: Server   -- ^ The hint to install the server job
      } deriving (Read, Show, Eq, Ord)

-- | Information about the server we are packaging.
data Server
    = Server
      { hostname :: String      -- ^ Host on which the server will run
      , port :: Int             -- ^ Port on which the server will run.
                                -- Obviously, this must assign each and
                                -- every server package to a different
                                -- port.
      , headerMessage :: String -- ^ A comment that will be inserted to
                                -- explain how the file was generated
      , retry :: String         -- ^ start-stop-daemon --retry argument
      , serverFlags :: [String] -- ^ Extra flags to pass to the server via the init script
      , installFile :: InstallFile -- ^ The hint to install the server executable
      } deriving (Read, Show, Eq, Ord)

data InstallFile
    = InstallFile
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      } deriving (Read, Show, Eq, Ord)

data VersionSplits
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: PackageName
      , splits :: [(Version, PackageName)] -- Assumed to be in version number order
      } deriving (Eq, Ord, Show)
