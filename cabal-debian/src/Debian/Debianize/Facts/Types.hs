{-# LANGUAGE DeriveDataTypeable #-}
module Debian.Debianize.Facts.Types where

import Data.Generics (Data, Typeable)
import Data.List (intercalate)
import Data.Map as Map (Map, toList)
import Data.Monoid (Monoid(..))
import Data.Set as Set (empty, null, Set, toList)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Changes (ChangeLog)
import Debian.Debianize.VersionSplits (VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackageArchitectures(All, Any), PackagePriority, Section, SourceFormat, StandardsVersion)
import Debian.Relation (BinPkgName, Relation(..), Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName)
import Prelude hiding (init, init, log, log, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | Bits and pieces of information about the mapping from cabal package
-- names and versions to debian package names and versions.  In essence,
-- an 'Atoms' value represents a package's debianization.  The lenses in
-- this module are used to get and set the values hidden in this Atoms
-- value.  Many of the values should be left alone to be set when the
-- debianization is finalized.
data Atoms
    = Atoms
      { top :: Top
      , atomMap :: Map DebAtomKey (Set DebAtom)
      , noDocumentationLibrary_ :: Set Bool
      -- ^ Do not produce a libghc-foo-doc package.
      , noProfilingLibrary_ :: Set Bool
      -- ^ Do not produce a libghc-foo-prof package.
      , omitLTDeps_ :: Set Bool
      -- ^ If present, don't generate the << dependency when we see a cabal
      -- equals dependency.  (The implementation of this was somehow lost.)
      , compilerVersion_ :: Set Version
      -- ^ Specify the version number of the GHC compiler in the build
      -- environment.  The default is to assume that version is the same
      -- as the one in the environment where cabal-debian is running.
      -- This is used to look up hard coded lists of packages bundled
      -- with the compiler and their version numbers.  (This could
      -- certainly be done in a more beautiful way.)
      , buildDir_ :: Set FilePath
      -- ^ The build directory used by cabal, typically dist/build when
      -- building manually or dist-ghc/build when building using GHC and
      -- haskell-devscripts.  This value is used to locate files
      -- produced by cabal so we can move them into the deb.  Note that
      -- the --builddir option of runhaskell Setup appends the "/build"
      -- to the value it receives, so, yes, try not to get confused.
      } deriving (Eq, Show)

newtype Top = Top {unTop :: FilePath} deriving (Eq, Ord, Show, Typeable)

newAtoms :: FilePath -> Atoms
newAtoms x
    = Atoms
      { top = Top x
      , atomMap = mempty
      , noDocumentationLibrary_ = mempty
      , noProfilingLibrary_ = mempty
      , omitLTDeps_ = mempty
      , compilerVersion_ = mempty
      , buildDir_ = mempty
      }

showAtoms :: Atoms -> IO ()
showAtoms x =
    do putStrLn ("\nTop: " ++ show (top x))
       mapM_ putAtoms (Map.toList (atomMap x))
       putStrLn ""
    where
      putAtoms (_, s) | Set.null s = return ()
      putAtoms (k, s) = putStrLn ("\n" ++ show k ++ ":\n  " ++ intercalate "\n  " (map show (Set.toList s)))

-- All the internals of this module is a steaming pile of poo, except
-- for the stuff that is exported.

data DebAtomKey
    = Source
    | Binary BinPkgName
    deriving (Eq, Ord, Show, Typeable)

-- | The smallest pieces of debhelper information.  Some of these are
-- converted directly into files in the debian directory, others
-- become fragments of those files, and others are first converted
-- into different DebAtom values as new information becomes available.
data DebAtom
    = DebSourceFormat SourceFormat                -- ^ Write debian/source/format
    | DebWatch Text                               -- ^ Write debian/watch
    | DHIntermediate FilePath Text                -- ^ Put this text into a file with the given name in the debianization.
    | DebRulesHead Text				  -- ^ The header of the debian/rules file.  The remainder is assembled
                                                  -- from DebRulesFragment values in the atom list.
    | DebRulesFragment Text                       -- ^ A Fragment of debian/rules
    | Warning Text                                -- ^ A warning to be reported later
    | UtilsPackageName BinPkgName                 -- ^ Name of a package that will get left-over data files and executables.
                                                  -- If there are more than one, each package will get those files.
    | DebChangeLog ChangeLog			  -- ^ The changelog, first entry contains the source package name and version
    | DebLogComments [[Text]]			  -- ^ Each element is a comment to be added to the changelog, where the
                                                  -- element's text elements are the lines of the comment.
    | DHMaintainer NameAddr			  -- ^ Value for the maintainer field in the control file.  Note that
                                                  -- the cabal maintainer field can have multiple addresses, but debian
                                                  -- only one.  If this is not explicitly set, it is obtained from the
                                                  -- cabal file, and if it is not there then from the environment.  As a
                                                  -- last resort, there is a hard coded string in here somewhere.
    | DHCabalFlagAssignments (Set (FlagName, Bool)) -- ^ Flags to pass to Cabal function finalizePackageDescription, this
                                                  -- can be used to control the flags in the cabal file.
    | DHFlags Flags                               -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc
    | DebRevision String			  -- ^ Specify the revision string to use when converting the cabal
                                                  -- version to debian.

    | DebVersion DebianVersion			  -- ^ Specify the exact debian version of the resulting package,
                                                  -- including epoch.  One use case is to work around the the
                                                  -- "buildN" versions that are often uploaded to the debian and
                                                  -- ubuntu repositories.  Say the latest cabal version of
                                                  -- transformers is 0.3.0.0, but the debian repository contains
                                                  -- version 0.3.0.0-1build3, we need to specify
                                                  -- debVersion="0.3.0.0-1build3" or the version we produce will
                                                  -- look older than the one already available upstream.
    | DebianNameMap (Map PackageName VersionSplits)
						  -- ^ Mapping from cabal package name and version to debian source
                                                  -- package name.  This allows different ranges of cabal versions to
                                                  -- map to different debian source package names.
    | SourcePackageName SrcPkgName                -- ^ Name to give to the debian source package.  If not supplied
                                                  -- the name is constructed from the cabal package name.  Note that
                                                  -- DebianNameMap could encode this information if we already knew
                                                  -- the cabal package name, but we can't assume that.
    | BuildDep Relations			  -- ^ Add build dependencies
    | BuildDepIndep Relations			  -- ^ Add arch independent build dependencies
    | MissingDependency BinPkgName		  -- ^ Lets cabal-debian know that a package it might expect to exist
                                                  -- actually does not, so omit all uses in resulting debianization.
    | ExtraLibMapping String Relations		  -- ^ Map a cabal Extra-Library name to a debian binary package name,
                                                  -- e.g. @ExtraLibMapping extraLibMap "cryptopp" "libcrypto-dev"@ adds a
                                                  -- build dependency *and* a regular dependency on @libcrypto-dev@ to
                                                  -- any package that has @cryptopp@ in its cabal Extra-Library list.
    | ExecMapping String Relations		  -- ^ Map a cabal Build-Tool name to a debian binary package name,
                                                  -- e.g. @ExecMapping "trhsx" "haskell-hsx-utils"@ adds a build
                                                  -- dependency on @haskell-hsx-utils@ to any package that has @trhsx@ in its
                                                  -- cabal build-tool list.
    | EpochMapping PackageName Int		  -- ^ Specify epoch numbers for the debian package generated from a
                                                  -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
    | DebPackageInfo PackageInfo		  -- ^ Supply some info about a cabal package.
    | DebCompat Int				  -- ^ The debhelper compatibility level, from debian/compat.
    | DebCopyright (Either License Text)	  -- ^ Copyright information, either as a Cabal License value or
                                                  -- the full text.
    | DebControl SourceDebDescription		  -- ^ The parsed contents of the control file

    -- From here down are atoms to be associated with a Debian binary
    -- package.  This could be done with more type safety, separate
    -- maps for the Source atoms and the Binary atoms.
    | DHApacheSite String FilePath Text           -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    | DHLogrotateStanza Text		          -- ^ Add a stanza of a logrotate file to the binary package
    | DHLink FilePath FilePath          	  -- ^ Create a symbolic link in the binary package
    | DHPostInst Text			 	  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm Text                     	  -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst Text                    	  -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm Text                      	  -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
    | DHArch PackageArchitectures       	  -- ^ Set the Architecture field of source or binary
    | DHPriority PackagePriority	       	  -- ^ Set the Priority field of source or binary
    | DHSection Section			       	  -- ^ Set the Section field of source or binary
    | DHDescription Text		       	  -- ^ Set the description of source or binary
    | DHInstall FilePath FilePath       	  -- ^ Install a build file into the binary package
    | DHInstallTo FilePath FilePath     	  -- ^ Install a build file into the binary package at an exact location
    | DHInstallData FilePath FilePath   	  -- ^ DHInstallTo somewhere relative to DataDir (see above)
    | DHFile FilePath Text              	  -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec String FilePath	  -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo String FilePath	  -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir FilePath             	  -- ^ Create a directory in the binary package
    | DHInstallInit Text                	  -- ^ Add an init.d file to the binary package
    | DHExecutable InstallFile                    -- ^ Create a binary package to hold a cabal executable
    | DHServer Server                             -- ^ Like DHExecutable, but configure the executable as a server process
    | DHWebsite Site                              -- ^ Like DHServer, but configure the server as a web server
    | DHBackups String                            -- ^ Configure the executable to do incremental backups
    | Depends Relation				  -- ^ Says that the debian package should have this relation in Depends
    | Conflicts Relation			  -- ^ Says that the debian package should have this relation in Conflicts
    | Provides Relation				  -- ^ Says that the debian package should have this relation in Provides
    | Replaces Relation				  -- ^ Says that the debian package should have this relation in Replaces
    | DevDepends Relation			  -- ^ Limited version of Depends, put a dependency on the dev library package.  The only
                                                  -- reason to use this is because we don't yet know the name of the dev library package.
    deriving (Eq, Ord, Show, Typeable)

-- | This record supplies information about the task we want done -
-- debianization, validataion, help message, etc.
data Flags = Flags
    {
    -------------------------
    -- Modes of Operation ---
    -------------------------
      verbosity_ :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , dryRun_ :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , validate_ :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , debAction_ :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    } deriving (Eq, Ord, Show)

-- | This type represents the debian/control file, which is the core
-- of the source package debianization.  It includes the information
-- that goes in the first, or source, section, and then a list of the
-- succeeding binary package sections.
data SourceDebDescription
    = SourceDebDescription
      { source :: Maybe SrcPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Source>
      , maintainer :: Maybe NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Maintainer>
      , changedBy :: Maybe NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Changed-By>
      , uploaders :: [NameAddr]
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
      , dmUploadAllowed :: Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-DM-Upload-Allowed>
      , priority :: Maybe PackagePriority
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Priority>
      , section :: Maybe Section
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
      , standardsVersion :: Maybe StandardsVersion
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
      , homepage :: Maybe Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Homepage>
      , vcsFields :: Set VersionControlSpec
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-VCS-fields>
      , xFields :: Set XField
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.7>
      , buildDepends :: Relations
      , buildConflicts :: Relations
      , buildDependsIndep :: Relations
      , buildConflictsIndep :: Relations
      , binaryPackages :: [BinaryDebDescription] -- This should perhaps be a set, or a map
      } deriving (Eq, Ord, Show, Data, Typeable)

newSourceDebDescription :: SourceDebDescription
newSourceDebDescription =
    SourceDebDescription
      { source = Nothing
      , maintainer = Nothing
      , changedBy = Nothing
      , uploaders = []
      , dmUploadAllowed = False
      , priority = Nothing
      , section = Nothing
      , buildDepends = []
      , buildConflicts = []
      , buildDependsIndep  = []
      , buildConflictsIndep  = []
      , standardsVersion = Nothing
      , homepage = Nothing
      , vcsFields = Set.empty
      , xFields = Set.empty
      , binaryPackages = [] }

newSourceDebDescription' :: SrcPkgName -> NameAddr -> SourceDebDescription
newSourceDebDescription' src who =
    newSourceDebDescription
      { source = Just src
      , maintainer = Just who }

data VersionControlSpec
    = VCSBrowser Text
    | VCSArch Text
    | VCSBzr Text
    | VCSCvs Text
    | VCSDarcs Text
    | VCSGit Text
    | VCSHg Text
    | VCSMtn Text
    | VCSSvn Text
    deriving (Eq, Ord, Show, Data, Typeable)

-- | User defined fields.  Parse the line "XBS-Comment: I stand
-- between the candle and the star." to get XField (fromList "BS")
-- "Comment" " I stand between the candle and the star."
data XField
    = XField (Set XFieldDest) Text Text
    deriving (Eq, Ord, Show, Data, Typeable)

data XFieldDest
    = B -- ^ Field will be copied to the binary packgae control files
    | S -- ^ Field will be copied to the source packgae control files
    | C -- ^ Field will be copied to the upload control (.changes) file
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | This type represents a section of the control file other than the
-- first, which in turn represent one of the binary packages or debs
-- produced by this debianization.
data BinaryDebDescription
    = BinaryDebDescription
      { package :: BinPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
      , architecture :: PackageArchitectures
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Architecture>
      , binarySection :: Maybe Section
      , binaryPriority :: Maybe PackagePriority
      , essential :: Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Essential>
      , description :: Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Description>
      , relations :: PackageRelations
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

newBinaryDebDescription :: BinPkgName -> PackageArchitectures -> BinaryDebDescription
newBinaryDebDescription name arch =
    BinaryDebDescription
      { package = name -- mkPkgName base typ
      , architecture = arch -- packageArch typ
      , binarySection = Nothing
      , binaryPriority = Nothing
      , essential = False
      , description = mempty
      , relations = newPackageRelations }

-- ^ Package interrelationship information.
data PackageRelations
    = PackageRelations
      { depends :: Relations
      , recommends :: Relations
      , suggests :: Relations
      , preDepends :: Relations
      , breaks :: Relations
      , conflicts :: Relations
      , provides_ :: Relations
      , replaces_ :: Relations
      , builtUsing :: Relations
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

newPackageRelations :: PackageRelations
newPackageRelations =
    PackageRelations
      { depends = []
      , recommends = []
      , suggests = []
      , preDepends = []
      , breaks = []
      , conflicts = []
      , provides_ = []
      , replaces_ = []
      , builtUsing = [] }

-- ^ The different types of binary debs we can produce from a haskell package
data PackageType
    = Development   -- ^ The libghc-foo-dev package.
    | Profiling     -- ^ The libghc-foo-prof package.
    | Documentation -- ^ The libghc-foo-doc package.
    | Exec          -- ^ A package related to a particular executable, perhaps
                    -- but not necessarily a server.
    | Utilities     -- ^ A package that holds the package's data files
                    -- and any executables not assigned to other
                    -- packages.
    | Source'       -- ^ The source package (not a binary deb actually.)
    | Cabal         -- ^ This is used to construct the value for
                    -- DEB_CABAL_PACKAGE in the rules file
    deriving (Eq, Show)

packageArch :: PackageType -> PackageArchitectures
packageArch Development = Any
packageArch Profiling = Any
packageArch Documentation = All
packageArch Utilities = All
packageArch Exec = Any
packageArch Cabal = undefined
packageArch Source' = undefined

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

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show)
