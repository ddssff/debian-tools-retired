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
import Debian.Relation (BinPkgName(..))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Debian.DebHelper (DebAtom)
import Distribution.Debian.PackageInfo (DebType)
import Distribution.Debian.Server (Executable(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))

-- | This record supplies the information we use to generate a
-- debianization from a cabal package.
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
    , verbosity :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , debAction :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    , buildDeps :: [String]
    -- ^ Add a debian binary package to the debianization's list of build dependencies.
    , extraDevDeps :: [String]
    -- ^ Add a debian binary package to the list of dependencies of
    -- the dev library in this debianization.  This means it will be
    -- pulled in by packages that depend on the dev library.
    , binaryPackageDeps :: [(BinPkgName, BinPkgName)]
    -- ^ An entry (a, b) says that debian package a should have a
    -- dependency on b, e.g. ("cabal-debian", "apt-file") says that
    -- cabal-debian program needs apt-file to be installed.
    , extraLibMap :: Map.Map String [BinPkgName]
    -- ^ Specify which debian binary packages corresponds to the
    -- packages specified in the cabal file Extra-Library field,
    -- e.g. Map.insert "cryptopp" "libcrypto-dev"
    , binaryPackageConflicts :: [(BinPkgName, BinPkgName)]
    , epochMap :: Map.Map PackageName Int
    -- ^ Specify epoch numbers for the debian package generated from a
    -- cabal package.
    , debVersion :: Maybe String
    -- ^ Specify the exact debian version of the resulting package, including epoch
    , revision :: String
    -- ^ Specify the revision string to use when converting the cabal version to debian
    , execMap :: Map.Map String BinPkgName
    -- ^ Specify a mapping from the name appearing in the Build-Tool
    -- field of the cabal file to a debian binary package name,
    -- e.g. Map.insert "trhsx" "haskell-hsx-utils"
    , omitLTDeps :: Bool
    -- ^ Don't generate the << dependency when we see a cabal equals dependency.
    , selfDepend :: Bool
    -- ^ Add a build dependency in the generated debianization on this
    -- library, libghc-cabal-debian-dev.  This means any change to
    -- cabal-debian will trigger a rebuild of all packages debianized
    -- by it, (and for us that is many,) so while technically this
    -- should always be true we make it an option and default it to
    -- false.
    , sourceFormat :: String
    -- ^ The string to write into the debian/source/format file, default
    -- '3.0 (quilt)'.
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
    , executablePackages :: [Executable]
    -- ^ List of executable debian binary packages to create.
    , sourcePackageName :: Maybe String
    -- ^ Name to give the debian source package.  If none is given it
    -- is constructed from the cabal package name.
    , debMaintainer :: Maybe String
    -- ^ Value for the maintainer field in the control file.  Note
    -- that the cabal maintainer field can have multiple addresses,
    -- but debian only one.  If this is not explicitly set, it is
    -- obtained from the cabal file, and if it is not there from the
    -- environment.  As a last result, there is a hard coded string
    -- in here somewhere.
    , modifyAtoms :: [DebAtom] -> [DebAtom]
    -- ^ Function to modify the final list of DebAtom before they
    -- are turned into a debianization.
    , buildDir :: FilePath
      -- ^ This is the buildDir that will be used in the debianization
      -- generated by this standalone version of cabal-debian.  It is used
      -- to find executables created by the cabal build process so it must
      -- match the builddir used by the Setup script.
    }

data DebAction = Usage | Debianize | SubstVar DebType deriving (Eq, Show)

defaultFlags :: Flags
defaultFlags =
    Flags {
      compilerFlavor = GHC
    , compilerVersion = Nothing
    , configurationsFlags = []
    , haddock = True
    , missingDependencies = []
    , help = False
    , debLibProf = True
    , verbosity = 1
    , debAction = Usage
    , buildDeps = []
    , extraDevDeps = []
    , extraLibMap = Map.empty
    , binaryPackageDeps = []
    , binaryPackageConflicts = []
    , epochMap = Map.empty
    , debVersion = Nothing
    , revision = "-1~hackage1"
    , execMap = Map.empty
    , omitLTDeps = False
    , selfDepend = False
    , sourceFormat = "3.0 (native)"
    , dryRun = False
    , validate = False
    , executablePackages = []
    , sourcePackageName = Nothing
    , debMaintainer = Nothing
    , modifyAtoms = id
    , buildDir = "dist-ghc/build"
    }

missingDependencies' :: Flags -> [BinPkgName]
missingDependencies' = map BinPkgName . missingDependencies
