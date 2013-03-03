module Debian.Debianize.Types
    ( Top(..)
    , PackageInfo(..)
    , Site(..)
    , Server(..)
    , InstallFile(..)
    , VersionSplits(..)
    , insertSplit
    , DebType(..)
    , DebAction(..)
    ) where

import Data.Version (Version(Version))
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)
import Prelude hiding (init, unlines, log)

newtype Top = Top {unTop :: FilePath}

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

-- | Describes a mapping from cabal package name and version to debian
-- package names.  For example, versions of the cabal QuickCheck
-- package less than 2 are mapped to "quickcheck1", while version 2 or
-- greater is mapped to "quickcheck2".
data VersionSplits
    = VersionSplits {
        oldestPackage :: String
      -- ^ The name given to versions older than the oldest split.
      , splits :: [(Version, String)]
      -- ^ Each pair is The version where the split occurs, and the
      -- name to use for versions greater than or equal to that
      -- version.  This list assumed to be in (must be kept in)
      -- ascending version number order.
      } deriving (Eq, Ord, Show)

-- | Split the version range and give the older packages a new name.
insertSplit :: String -> Version -> VersionSplits -> VersionSplits
insertSplit base ver@(Version ns _) sp@(VersionSplits {}) =
    case splits sp of
      -- This is the oldest split, change oldestPackage and insert a new head pair
      (ver', name') : _ | ver' > ver -> sp {oldestPackage = newname, splits = (ver, oldestPackage sp) : splits sp}
      [] -> sp {oldestPackage = newname, splits = [(ver, oldestPackage sp)]}
      -- Not the oldest split, insert it in its proper place.
      _ -> sp {splits = reverse (insert (reverse (splits sp)))}
    where
      -- Insert our new split into the reversed list
      insert ((ver', name') : more) =
          if ver' < ver
          then (ver, name') : (ver', newname) : more
          else (ver', name') : insert more
      -- ver' is older, change oldestPackage
      insert [] = [(ver, oldestPackage sp)]
      newname = base ++ "-" ++ (show (last ns - 1))

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show)
