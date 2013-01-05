module Debian.Debianize.Types.PackageHints
    ( PackageHints
    , PackageHint(..)
    , InstallFile(..)
    , Server(..)
    , Site(..)
    , executableOption
    ) where

import Data.Text (Text)
--import Debian.Debianize.Types.Debianization (PackageRelations)
import Debian.Policy (PackagePriority, Section, PackageArchitectures)
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Orphans ()
import System.FilePath (splitFileName)

type PackageHints = [PackageHint]

data PackageHint
    = UtilsPackageHint BinPkgName
    -- ^ Change the name of the catch-all package
    | InstallFileHint BinPkgName InstallFile
    | ServerHint BinPkgName Server
    | SiteHint BinPkgName Site
    | PriorityHint BinPkgName (Maybe PackagePriority)
    | SectionHint BinPkgName (Maybe Section)
    | ArchitectureHint BinPkgName PackageArchitectures
    | DescriptionHint BinPkgName Text

data InstallFile
    = InstallFile
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      } deriving (Read, Show, Eq)

-- | Information about the web site we are packaging.
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
      } deriving (Read, Show, Eq)

data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      , server :: Server   -- ^ The hint to install the server job
      } deriving (Read, Show, Eq)

-- | Process a --executable command line argument
executableOption :: String -> (PackageHint -> a) -> a
executableOption arg f =
    case span (/= ':') arg of
      (sp, md) ->
          let (sd, name) = splitFileName sp in
          f (InstallFileHint (BinPkgName name)
                             (InstallFile { execName = name
                                          , destName = name
                                          , sourceDir = case sd of "./" -> Nothing; _ -> Just sd
                                          , destDir = case md of (':' : dd) -> Just dd; _ -> Nothing }))
