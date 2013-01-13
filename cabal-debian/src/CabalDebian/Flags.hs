module CabalDebian.Flags
    ( Flags(..)
    , defaultFlags
    , DebAction(..)
    , withFlags
    , debianize
{-  , compileArgs
    , withEnvironmentArgs
    , withEnvironmentFlags
    , putEnvironmentArgs
    , callDebianize
    , runDebianize -}
    ) where

import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Set (Set)
import Debian.Debianize.Types.Atoms (HasAtoms(..), DebAtomKey(..), DebAtom(NoDocumentationLibrary, NoProfilingLibrary, CompilerVersion, DebSourceFormat), insertAtom, compilerVersion, doDependencyHint, missingDependency, doExecutable)
import Debian.Debianize.Types.PackageHints (PackageHints)
import Debian.Debianize.Types.PackageType (DebType)
import Debian.Policy (SourceFormat(Native3, Quilt3))
import Distribution.PackageDescription as Cabal (FlagName)
import System.Exit (ExitCode(ExitSuccess))
import System.Process.Progress (defaultVerbosity)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

import Control.Monad (when)
import Data.Char (toLower, isDigit, ord)
import qualified Data.Map as Map
import Data.Version (parseVersion)
import Debian.Debianize.Types.Dependencies (DependencyHints (..))
import Debian.Debianize.Types.PackageHints (executableOption)
import Debian.Policy (parseMaintainer)
import Debian.Relation (BinPkgName(..))
import Debian.Version (parseDebianVersion)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), usageInfo, getOpt')
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (Handle, hPutStrLn, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.TDFA ((=~))

import Debian.Orphans ()
import Prelude hiding (readFile, lines, null, log, sum)
import System.Environment (getEnv)
import System.IO.Error (catchIOError)
import System.Posix.Env (setEnv)

import Debian.Cabal.Debianize (debianizationWithIO)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Output (outputDebianization)
import System.Directory (doesFileExist)
import System.Process (readProcessWithExitCode)

-- | This record supplies the information we use to generate a
-- debianization from a cabal package.
data Flags = Flags
    {
    -------------------------
    -- Modes of Operation ---
    -------------------------
      help :: Bool
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
    -------------------------
    -- Cabal Options
    -------------------------
    , cabalFlagAssignments :: [(FlagName, Bool)]
    -- ^ Flags to pass to Cabal function finalizePackageDescription, this
    -- can be used to control the flags in the cabal file.

    --------------------------------------------------
    -- Debian Package Configuration
    --------------------------------------------------
    , sourcePackageName :: Maybe String
    -- ^ Name to give the debian source package.  If none is given it
    -- is constructed from the cabal package name.
    , debMaintainer :: Maybe NameAddr
    -- ^ Value for the maintainer field in the control file.  Note
    -- that the cabal maintainer field can have multiple addresses,
    -- but debian only one.  If this is not explicitly set, it is
    -- obtained from the cabal file, and if it is not there then from
    -- the environment.  As a last resort, there is a hard coded
    -- string in here somewhere.
    , debAtoms :: Map DebAtomKey (Set DebAtom)
    -- ^ Preliminary value of corresponding Debianization field
    }

instance HasAtoms Flags where
    getAtoms = debAtoms
    putAtoms x flags = flags {debAtoms = x}

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq)

defaultFlags :: Flags
defaultFlags =
    Flags {
      cabalFlagAssignments = []
    , help = False
    , verbosity = defaultVerbosity
    , debAction = Usage
    , dryRun = False
    , validate = False
    , sourcePackageName = Nothing
    , debMaintainer = Nothing
    -- , modifyAtoms = id
    , buildDir = "dist-ghc/build"
    -- , dependencyHints = defaultDependencyHints
    , debAtoms = insertAtom Source (DebSourceFormat Native3) mempty
    }

getFlags :: IO Flags
getFlags = getArgs >>= parseArgs

withFlags :: (Flags -> IO a) -> IO a
withFlags action = getFlags >>= action

compileArgs :: [String] -> Flags -> Flags
compileArgs args flags =
  case getOpt' RequireOrder options args of
    (os, [], [], []) -> foldl (flip ($)) flags os
    (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                  ", Unrecognized: " ++ show unk ++
                                  ", Non-Options: " ++ show non)

parseArgs :: [String] -> IO Flags
parseArgs args = do
     when (help opts || debAction opts == Usage) $ do
       printHelp stdout
       exitWith ExitSuccess
     return opts
    where opts = compileArgs args defaultFlags

options :: [OptDescr (Flags -> Flags)]
options =
    [ Option "" ["executable"] (ReqArg (\ path x -> executableOption path (\ b e -> doExecutable b e x)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             "Create individual eponymous executable packages for these executables.  Other executables and data files are gathered into a single utils package.",
      Option "h?" ["help"] (NoArg (\x -> x { help = True }))
             "Show this help text",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> insertAtom Source (CompilerVersion (last (map fst (readP_to_S parseVersion ver)))) x) "VERSION")
             "Version of GHC in build environment",
      Option "" ["disable-haddock"] (NoArg (\ x -> insertAtom Source NoDocumentationLibrary x))
             "Don't generate API documentation.  Use this if build is crashing due to a haddock error.",
      Option "" ["missing-dependency"] (ReqArg (\ name flags -> missingDependency (BinPkgName name) flags) "DEB")
             "Mark a package missing, do not add it to any dependency lists in the debianization.",
      Option "" ["source-package-name"] (ReqArg (\ name x -> x {sourcePackageName = Just name}) "NAME")
             "Use this name for the debian source package.  Default is haskell-<cabalname>, where the cabal package name is downcased.",
      Option "" ["disable-library-profiling"] (NoArg (\ x -> insertAtom Source NoProfilingLibrary x))
             "Don't generate profiling libraries",
      Option "f" ["flags"] (ReqArg (\flags x -> x { cabalFlagAssignments = cabalFlagAssignments x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "v" ["verbose"] (ReqArg (\s x -> x { verbosity = read s }) "n")
             "Change build verbosity",
      Option "" ["maintainer"] (ReqArg (\maint x -> x { debMaintainer = either (error ("Invalid maintainer string: " ++ show maint)) Just (parseMaintainer maint) }) "Maintainer Name <email addr>")
             "Override the Maintainer name and email in $DEBEMAIL/$EMAIL/$DEBFULLNAME/$FULLNAME",
      Option "" ["debianize"] (NoArg (\x -> x {debAction = Debianize}))
             "Generate a new debianization, replacing any existing one.  One of --debianize or --substvar is required.",
      Option "" ["build-dep"] (ReqArg (\ name flags -> doDependencyHint (\ x -> x {buildDeps = BinPkgName name : (buildDeps x)}) flags) "Debian binary package name")
             "Specify a package to add to the build dependency list for this source package, e.g. '--build-dep libglib2.0-dev'.",
      Option "" ["dev-dep"] (ReqArg (\ name flags -> doDependencyHint (\ x -> x {extraDevDeps = BinPkgName name : (extraDevDeps x)}) flags) "Debian binary package name")
             "Specify a package to add to the Depends: list of the -dev package, e.g. '--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep.",
      Option "" ["depends"] (ReqArg (\ arg flags -> doDependencyHint (\ x -> x {binaryPackageDeps = parseDeps arg ++ binaryPackageDeps x}) flags) "deb:deb,deb:deb,...")
             "Generalized --dev-dep - specify pairs A:B of debian binary package names, each A gets a Depends: B",
      Option "" ["conflicts"] (ReqArg (\ arg flags -> doDependencyHint (\ x -> x {binaryPackageConflicts = parseDeps arg ++ binaryPackageConflicts x}) flags) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Conflicts: B.  Note that B can have debian style version relations",
      Option "" ["map-dep"] (ReqArg (\ pair flags -> doDependencyHint (\ x -> x {extraLibMap = case break (== '=') pair of
                                                                                                 (cab, (_ : deb)) -> Map.insertWith (++) cab [b deb] (extraLibMap x)
                                                                                                 (_, "") -> error "usage: --dep-map CABALNAME=DEBIANNAME"}) flags) "CABALNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Extra-Library field of the cabal file to a debian binary package name, e.g. --dep-map cryptopp=libcrypto-dev",
      Option "" ["deb-version"] (ReqArg (\ version flags -> doDependencyHint (\ x -> x {debVersion = Just (parseDebianVersion version)}) flags) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (\ rev flags -> doDependencyHint (\ x -> x {revision = rev}) flags) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"] (ReqArg (\ pair flags -> doDependencyHint (\ x -> x {epochMap =
                                                                                       case break (== '=') pair of
                                                                                         (_, (_ : ['0'])) -> epochMap x
                                                                                         (cab, (_ : [d])) | isDigit d -> Map.insert (PackageName cab) (ord d - ord '0') (epochMap x)
                                                                                         _ -> error "usage: --epoch-map CABALNAME=DIGIT"}) flags) "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["substvar"] (ReqArg (\ name x -> x {debAction = SubstVar (read name)}) "Doc, Prof, or Dev")
             (unlines ["Write out the list of dependencies required for the dev, prof or doc package depending",
                       "on the argument.  This value can be added to the appropriate substvars file."]),
      Option "" ["exec-map"] (ReqArg (\ s flags -> doDependencyHint (\ x -> x {execMap = case break (== '=') s of
                                                                                           (cab, (_ : deb)) -> Map.insert cab (b deb) (execMap x)
                                                                                           _ -> error "usage: --exec-map CABALNAME=DEBNAME"}) flags) "EXECNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (\ flags -> doDependencyHint (\ x -> x { omitLTDeps = True }) flags))
             "Don't generate the << dependency when we see a cabal equals dependency.",
      Option "" ["quilt"] (NoArg (\ x -> insertAtom Source (DebSourceFormat Quilt3) x))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "n" ["dry-run", "compare"] (NoArg (\ x -> x {dryRun = True}))
             "Just compare the existing debianization to the one we would generate.",
      Option "" ["builddir"] (ReqArg (\ s x -> x {buildDir = s </> "build"}) "PATH")
             "Subdirectory where cabal does its build, dist/build by default, dist-ghc when run by haskell-devscripts.  The build subdirectory is added to match the behavior of the --builddir option in the Setup script."
    ]

parseDeps :: String -> [(BinPkgName, BinPkgName)]
parseDeps arg =
    map pair (split arg)
    where
      split s =
          case s =~ "^[ \t,]*([^,]+)[ \t,]*" :: (String, String, String, [String]) of
            (_, _, tl, [hd]) -> hd : split tl
            (_, _, "", _) -> []
            _ -> error $ "Invalid dependency: " ++ show s
      pair s =
          case s =~ "^[ \t:]*([^ \t:]+)[ \t]*:[ \t]*(.+)[ \t]*" :: (String, String, String, [String]) of
            (_, _, _, [x, y]) -> (b x, b y)
            _ -> error $ "Invalid dependency: " ++ show s

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

printHelp :: Handle -> IO ()

printHelp h = do
    progName <- getProgName
    let info = "Usage: " ++ progName ++ " [FLAGS]\n"
    hPutStrLn h (usageInfo info options)

b :: String -> BinPkgName
b = BinPkgName

-- | Call a function with a list of strings read from the value of
-- $CABALDEBIAN.
withEnvironmentArgs :: ([String] -> IO a) -> IO a
withEnvironmentArgs f =
  (getEnv "CABALDEBIAN" >>= return . read) `catchIOError` handle >>= f
  where
    handle :: IOError -> IO [String]
    handle _ = return []

-- | Read the value of $CABALDEBIAN as a list of command line
-- arguments, construct a ('Flags' -> 'Flags') function from them, and
-- compose it with a function that takes a 'Flags' record.
withEnvironmentFlags :: Flags -> (Flags -> IO a) -> IO a
withEnvironmentFlags flags0 f =
  (getEnv "CABALDEBIAN" >>= return . compileArgs . read) `catchIOError` handle >>= \ ff ->
  let flags = ff flags0 in
  f flags
  where
    handle :: IOError -> IO (Flags -> Flags)
    handle _ = return id

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True

-- | Run the debianize function with arguments pulled out of the
-- @CABALDEBIAN@ environment variable, with the build directory set to
-- match what dpkg-buildpackage will use later when it uses the
-- resulting debianization.
callDebianize :: [String] -> IO ()
callDebianize args =
    debianize "." (compileArgs args defaultFlags)

-- | Try to run the custom script in debian/Debianize.hs to create the
-- debianization.  This can first put some extra arguments into the
-- @CABALDEBIAN@ environment variable.  Often this is used to set the
-- dryRun option by passing @["-n"]@.
runDebianize :: [String] -> IO Bool
runDebianize args =
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True ->
          putEnvironmentArgs args >> readProcessWithExitCode "runhaskell" ("debian/Debianize.hs" : args) "" >>= \ result ->
          case result of
            (ExitSuccess, _, _) -> return True
            (code, out, err) ->
              error ("runDebianize failed with " ++ show code ++ ":\n stdout: " ++ show out ++"\n stderr: " ++ show err)

-- | Generate a debianization for the cabal package in a directory
-- using information from the .cabal file and from the options in
-- Flags.  This ignores any existing debianization except for the
-- debian/changelog file.  A new entry changelog is generated, and any
-- entries already there that look older than the new one are
-- preserved.
debianize :: FilePath -> Flags -> IO ()
debianize top flags =
    withEnvironmentFlags flags $ \ flags' ->
    do old <- inputDebianization "."
       (new, dataDir) <- debianizationWithIO top (verbosity flags') (compilerVersion flags') (cabalFlagAssignments flags') (debMaintainer flags') (debAtoms flags') old
       outputDebianization (dryRun flags') (validate flags') (buildDir flags') dataDir old new
