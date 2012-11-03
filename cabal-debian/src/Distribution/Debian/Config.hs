-- | Command line option processing for building (formerly RPM, now)
-- Debian packages.

module Distribution.Debian.Config
    ( Flags(..)
    , DebAction(..)
    , defaultFlags
    , missingDependencies'
    ) where

import qualified Data.Map as Map
import Data.Version (Version)
import Debian.Relation (BinPkgName(..), PkgName(..))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Debian.PackageInfo (DebType)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import Distribution.Verbosity (Verbosity, normal)

-- | Why rpm?  This started as a program to generate RPM packages from cabal files.  Yep.
data Flags = Flags
    {
      compilerFlavor :: CompilerFlavor
    -- ^ Only GHC is supported
    , compilerVersion :: Maybe Version
    , configurationsFlags :: [(FlagName, Bool)]
    -- ^ Flags to pass to Cabal function finalizePackageDescription
    , haddock :: Bool
    -- ^ Should we generate the library documentation package?
    , missingDependencies :: [String]
    -- ^ Mark a package missing, do not add it to any dependency lists
    -- in the debianization.  If some cabal build dependency has no
    -- documentation package you would list it here.
    , help :: Bool
    -- ^ Print a help message and exit.
    , debLibProf :: Bool
    -- ^ Don't generate profiling libraries.  May be needed to work
    -- around a compiler bug.
    , rpmName :: Maybe String
    , rpmOptimisation :: Bool
    , rpmRelease :: Maybe String
    , rpmSplitObjs :: Bool
    , debOutputDir :: FilePath
    , buildRoot :: FilePath
    , verbosity :: Verbosity
    -- ^ Run with progress messages at the given level of verboseness.
    , rpmVersion :: Maybe String
    , debAction :: DebAction
    , buildDeps :: [String]
    , extraDevDeps :: [String]
    -- , debName :: Maybe String
    , debVersion :: Maybe String
    , depMap :: Map.Map String [BinPkgName]
    , binaryPackageDeps :: [(String, String)]
    , binaryPackageConflicts :: [(String, String)]
    , epochMap :: Map.Map PackageName Int
    , revision :: String
    , execMap :: Map.Map String BinPkgName
    , omitLTDeps :: Bool
    , sourceFormat :: String
    , compareOnly :: Bool
    , executablePackages :: [String]
    , debMaintainer :: Maybe String
    }
    deriving (Eq, Show)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Eq, Show)

defaultFlags :: Flags

defaultFlags = Flags
    {
      compilerFlavor = GHC
    , compilerVersion = Nothing
    , configurationsFlags = []
    , haddock = True
    , missingDependencies = []
    , help = False
    , debLibProf = True
    , rpmName = Nothing
    , rpmOptimisation = True
    , rpmRelease = Nothing
    , rpmSplitObjs = True
    , debOutputDir = "./debian"
    , buildRoot = "/"
    , verbosity = normal
    , rpmVersion = Nothing
    , debAction = Usage
    , buildDeps = []
    , extraDevDeps = []
    , depMap = Map.empty
    , binaryPackageDeps = []
    , binaryPackageConflicts = []
    , epochMap = Map.empty
    -- , debName = Nothing
    , debVersion = Nothing
    , revision = "-1~hackage1"
    , execMap = Map.empty
    , omitLTDeps = False
    , sourceFormat = "3.0 (native)"
    , compareOnly = False
    , executablePackages = []
    , debMaintainer = Nothing
    }

missingDependencies' :: Flags -> [BinPkgName]
missingDependencies' = map (BinPkgName . PkgName) . missingDependencies
