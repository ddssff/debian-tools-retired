module Debian.Debianize.Types
    ( Flags(..)
    , DebAction(..)
    , PackageInfo(..)
    , Site(..)
    , Server(..)
    , InstallFile(..)
    , DebType(..)
    , VersionSplits(..)
    , defaultFlags
    , knownVersionSplits
    , knownEpochMappings
    , watchAtom
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    ) where

import Data.Map as Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Version (Version(Version))
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName(PackageName))
import Prelude hiding (init, unlines, log)

-- | This record supplies information about the task we want done -
-- debianization, validataion, help message, etc.
data Flags = Flags
    {
    -------------------------
    -- Modes of Operation ---
    -------------------------
      verbosity :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , dryRun :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , validate :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , debAction :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    } deriving (Eq, Ord, Show)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord)

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

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show)

data VersionSplits
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: PackageName
      , splits :: [(Version, PackageName)] -- Assumed to be in version number order
      } deriving (Eq, Ord, Show)

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: [VersionSplits]
knownVersionSplits =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = PackageName "parsec2"
      , splits = [(Version [3] [], PackageName "parsec3")] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = PackageName "quickcheck1"
      , splits = [(Version [2] [], PackageName "quickcheck2")] }
    ]

-- | We should always call this, just as we should always apply
-- knownVersionSplits.
knownEpochMappings :: Map PackageName Int
knownEpochMappings =
    Map.fromList [(PackageName "HaXml", 1)]

oldClckwrksSiteFlags :: Site -> [String]
oldClckwrksSiteFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ domain x ++ "/"
    , "--http-port", show port]
oldClckwrksServerFlags :: Server -> [String]
oldClckwrksServerFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ hostname x ++ ":" ++ show (port x) ++ "/"
    , "--http-port", show port]

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity = 1
    , debAction = Usage
    , dryRun = False
    , validate = False
    }

watchAtom :: PackageName -> Text
watchAtom (PackageName pkgname) =
    pack $ "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
           "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
           " \\\n    ([\\d\\.]*\\d)/\n"
