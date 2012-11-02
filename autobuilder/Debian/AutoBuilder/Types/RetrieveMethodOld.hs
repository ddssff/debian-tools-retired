-- |Data type representing a method for retrieving the source code of a package specification.
-- Used only for reading the Fingerprint string of packages built before 5 March 2012.
module Debian.AutoBuilder.Types.RetrieveMethodOld
    ( RetrieveMethod(..)
    ) where

-- |Represents only the data resulting from parsing the spec string (which is going away)
data RetrieveMethod
    = Apt String String [AptFlag]
    | Bzr String
    | Cd FilePath RetrieveMethod
    | Darcs String [DarcsFlag]
    | DebDir RetrieveMethod RetrieveMethod
    | Debianize String [CabalFlag]
    | Dir FilePath
    | Hackage String [CabalFlag]
    | Hg String
    | Proc RetrieveMethod
    | Quilt RetrieveMethod RetrieveMethod
    | SourceDeb RetrieveMethod
    | Svn String
    | Tla String
    | Twice RetrieveMethod
    | Uri String String
    deriving (Read, Show, Eq, Ord)

data AptFlag
    = AptPin String             -- ^ Specify the exact debian version of a package to retrieve via apt-get
    deriving (Read, Show, Eq, Ord)

data DarcsFlag
    = DarcsTag String           -- ^ When doing a darcs get pass this string to darcs via the --tag flag.
    deriving (Read, Show, Eq, Ord)

data CabalFlag
    = CabalPin String
    | ExtraDep String		-- ^ Build dependencies which should be added to the debian/control file via the --build-dep flag of cabal-debian.
    | ExtraDevDep String	-- ^ Install dependencies which should be added to the Depends entry for the dev package in the
                                -- debian/control file via the --dev-dep flag of cabal-debian
    | MapDep String String	-- ^ Tell cabal-debian to map the first argument (a name that appears in Extra-Libraries field of
                                -- the cabal file) to the second argument (a debian binary package name) using the --map-dep flag of cabal-debian.
    | DebVersion String         -- ^ The exact debian version number to insert into the changelog.  An exception will be thrown if
                                -- the hackage version looks newer than this.  This causes the --deb-version flag to be passed to cabal-debian.
    | Revision String           -- ^ Pass --revision <string> to cabal-debian so a suffix will be added to the cabal version to get
                                -- the debian version.  By default this is -1~hackage1.  Debian policy says this should either be
                                -- empty or begin with a dash.
    | Epoch String Int          -- ^ Set the epoch number in the version number of the given cabal package
    deriving (Read, Show, Eq, Ord)
