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
import Distribution.Debian.DebHelper (DebAtom)
import Distribution.Debian.PackageInfo (DebType)
import Distribution.Debian.Server (Executable(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import System.Process.Progress (defaultVerbosity)

-- | This record supplies the information we use to generate a
-- debianization from a cabal package.
data Flags = Flags
    {
      compilerVersion :: Maybe Version
    -- ^ Specify the version number of the GHC compiler in the build
    -- environment.  The default is to assume that version is the same
    -- as the one in the environment where cabal-debian is running.
    -- This is used to look up hard coded lists of packages bundled
    -- with the compiler and their version numbers.  (This could
    -- certainly be done in a more beautiful way.)

    -- *** Modes of Operation ***
    , help :: Bool
    -- ^ Print a help message and exit.
    , verbosity :: Int
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
    , buildDir :: FilePath
    -- ^ (Not sure if this option has a valid use case.)  This is the
    -- buildDir that will be used in the debianization generated by
    -- this standalone version of cabal-debian (normally
    -- dist-ghc/build).  It is used to find executables created by the
    -- cabal build process so it must match the builddir used by the
    -- Setup script.  The --builddir option appends the "/build" to
    -- the value it receives, so, yes, try not to get confused.

    -- *** Version Numbers ***
    , epochMap :: Map.Map PackageName Int
    -- ^ Specify epoch numbers for the debian package generated from a
    -- cabal package.  Example: @Map.insert epochMap "HTTP" 1@. 
    , revision :: String
    -- ^ Specify the revision string to use when converting the cabal
    -- version to debian.
    , debVersion :: Maybe String
    -- ^ Specify the exact debian version of the resulting package,
    -- including epoch.  One use case is to work around the the
    -- "buildN" versions that are often uploaded to the debian and
    -- ubuntu repositories.  Say the latest cabal version of
    -- transformers is 0.3.0.0, but the debian repository contains
    -- version 0.3.0.0-1build3, we need to specify
    -- debVersion="0.3.0.0-1build3" or the version we produce will
    -- look older than the one already available upstream.

    -- *** Build Dependencies - Controling the Build-Depends and
    -- Build-Depends-Indep fields of the debian source package.
    , buildDeps :: [String]
    -- ^ Add a debian binary package to the debianization's list of
    -- build dependencies.  These will be in addition to dependencies
    -- derived from the cabal build-depends list.
    , missingDependencies :: [String]
    -- ^ Mark a package missing, do not add it to any dependency lists
    -- in the debianization.  If some cabal build dependency foo has
    -- no documentation package (because it was built with the haddock
    -- option set to False) you would need to add libghc-foo-doc to
    -- this list.
    , extraLibMap :: Map.Map String [BinPkgName]
    -- ^ Specify which debian binary packages corresponds to the
    -- packages specified in the cabal file Extra-Library field,
    -- e.g. Map.insert extraLibMap "cryptopp" "libcrypto-dev" adds a
    -- build dependency on libcrypto-dev to any package that has
    -- cryptopp in its cabal Extra-Library list.
    , execMap :: Map.Map String BinPkgName
    -- ^ Specify a mapping from the name appearing in the Build-Tool
    -- field of the cabal file to a debian binary package name,
    -- e.g. Map.insert execMap "trhsx" "haskell-hsx-utils" adds a
    -- build dependency on haskell-hsx-utils to any package that has
    -- trhsx in its cabal build-tool list.
    , omitLTDeps :: Bool
    -- ^ Don't generate the << dependency when we see a cabal equals
    -- dependency.

    -- *** Install Dependencies - controlling the Depends, Conflicts,
    -- Provides, and Replaces fields of the debian binary packages.
    , extraDevDeps :: [String]
    -- ^ Add a debian binary package to the list of dependencies of
    -- the libghc-foo-dev package produced by the debianization.
    -- E.g., when building the haskell-terminfo package we put
    -- "libncurses5-dev" into this list to make it an install
    -- dependency of libghc-terminfo-dev.
    , binaryPackageDeps :: [(BinPkgName, BinPkgName)]
    -- ^ An entry (a, b) says that debian package a should have a
    -- dependency on b, e.g. ("cabal-debian", "apt-file") says that
    -- cabal-debian program needs apt-file to be installed.
    , binaryPackageConflicts :: [(BinPkgName, BinPkgName)]
    -- ^ An entry (a, b) says that two debian packages should both
    -- have Conflicts entries referring to each other.  For example
    -- [("libghc-quickcheck1-doc", "libghc-quickcheck2-doc")] says
    -- that libghc-quickcheck1-doc conflicts with
    -- libghc-quickcheck2-doc, and vice versa.

    -- *** Debian Binary Packages - Controlling which debian binary
    -- packages will be created.
    , debLibProf :: Bool
    -- ^ Don't generate profiling libraries.  May be needed to work
    -- around a compiler bug.
    , haddock :: Bool
    -- ^ Should we generate the library documentation package?
    , executablePackages :: [Executable]
    -- ^ List of executable debian binary packages to create.
    -- Normally all the executables are gathered into a package names
    -- haskell-sourcepackage-utils, this lets you split some of them
    -- out into individual debian binary packages.

    -- *** Cabal Options ***
    , configurationsFlags :: [(FlagName, Bool)]
    -- ^ Flags to pass to Cabal function finalizePackageDescription, this
    -- can be used to control the flags in the cabal file.

    -- *** Debian Package Configuration ***
    , sourcePackageName :: Maybe String
    -- ^ Name to give the debian source package.  If none is given it
    -- is constructed from the cabal package name.
    , sourceFormat :: String
    -- ^ The string to write into the debian/source/format file, default
    -- '3.0 (quilt)'.
    , debMaintainer :: Maybe String
    -- ^ Value for the maintainer field in the control file.  Note
    -- that the cabal maintainer field can have multiple addresses,
    -- but debian only one.  If this is not explicitly set, it is
    -- obtained from the cabal file, and if it is not there then from
    -- the environment.  As a last resort, there is a hard coded
    -- string in here somewhere.
{-
    -- *** 
    , modifyAtoms :: [DebAtom] -> [DebAtom]
    -- ^ Function to modify the final list of DebAtom before they
    -- are turned into a debianization.
-}
    }

data DebAction = Usage | Debianize | SubstVar DebType deriving (Eq, Show)

defaultFlags :: Flags
defaultFlags =
    Flags {
      compilerVersion = Nothing
    , configurationsFlags = []
    , haddock = True
    , missingDependencies = []
    , help = False
    , debLibProf = True
    , verbosity = defaultVerbosity
    , debAction = Usage
    , buildDeps = []
    , extraDevDeps = []
    , extraLibMap = Map.empty
    , binaryPackageDeps = []
    , binaryPackageConflicts = []
    , epochMap = Map.empty
    , debVersion = Nothing
    , revision = "-1~hackage1" -- This default produces a version number that is slightly older looking than an initial version from debian, which will have revision @-1@.
    , execMap = Map.empty
    , omitLTDeps = False
    , sourceFormat = "3.0 (native)"
    , dryRun = False
    , validate = False
    , executablePackages = []
    , sourcePackageName = Nothing
    , debMaintainer = Nothing
    -- , modifyAtoms = id
    , buildDir = "dist-ghc/build"
    }

missingDependencies' :: Flags -> [BinPkgName]
missingDependencies' = map BinPkgName . missingDependencies
