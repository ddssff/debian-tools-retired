module CabalDebian.Flags
    ( Flags(..)
    , DebAction(..)
    , withFlags
    , debianize
    ) where

import Data.Char (toLower, isDigit, ord)
import qualified Data.Map as Map
import Data.Set (fromList)
import Data.Version (parseVersion)
import Debian.Cabal.Debianize (debianizationWithIO)
import Debian.Debianize.Atoms (doDependencyHint, missingDependency, doExecutable, setSourcePackageName,
                               buildDir, setBuildDir, putCabalFlagAssignments, defaultAtoms, flags, mapFlags)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Output (outputDebianization)
import Debian.Debianize.Types.Atoms (HasAtoms(..), DebAtomKey(..), DebAtom(NoDocumentationLibrary, NoProfilingLibrary, CompilerVersion, DebSourceFormat, DHMaintainer),
                                     insertAtom, AtomMap, Flags(..), DebAction(..))
import Debian.Debianize.Types.Dependencies (DependencyHints (..))
import Debian.Debianize.Types.PackageHints (InstallFile(..))
import Debian.Orphans ()
import Debian.Policy (SourceFormat(Quilt3), parseMaintainer)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Version (parseDebianVersion)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import Prelude hiding (readFile, lines, null, log, sum)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), usageInfo, getOpt')
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName, getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import System.IO (Handle, hPutStrLn)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
-- import System.Process.Progress (defaultVerbosity)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Posix.Env (setEnv)
import Text.Regex.TDFA ((=~))

-- | Compile the command line arguments into the atoms value and pass
-- to the action.
withFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withFlags def action = getArgs >>= action . compileArgs def

{-
getFlags :: HasAtoms atoms => IO atoms
getFlags = getArgs >>= parseArgs

parseArgs :: HasAtoms atoms => [String] -> atoms -> IO atoms
parseArgs args atoms = do
     when (debAction (flags opts) == Usage) $ do
       printHelp stdout
       exitWith ExitSuccess
     return opts
    where opts = compileArgs args atoms
-}

compileArgs :: HasAtoms atoms => atoms -> [String] -> atoms
compileArgs atoms args =
  case getOpt' RequireOrder (flagOptions ++ atomOptions) args of
    (os, [], [], []) -> foldl (flip ($)) atoms os
    (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                  ", Unrecognized: " ++ show unk ++
                                  ", Non-Options: " ++ show non)

printHelp :: Handle -> IO ()
printHelp h = do
    progName <- getProgName
    let info = "Usage: " ++ progName ++ " [FLAGS]\n"
    hPutStrLn h (usageInfo info ((flagOptions ++ atomOptions) :: [OptDescr (AtomMap -> AtomMap)]))

flagOptions :: HasAtoms atoms => [OptDescr (atoms -> atoms)]
flagOptions =
    [ Option "v" ["verbose"] (ReqArg (\ s atoms -> mapFlags (\ x -> x { verbosity = read s }) atoms) "n")
             "Change build verbosity",
      Option "n" ["dry-run", "compare"] (NoArg (\ atoms -> mapFlags (\ x -> x {dryRun = True}) atoms))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (\ atoms -> mapFlags (\ x -> x {debAction = Usage}) atoms))
             "Show this help text",
      Option "" ["debianize"] (NoArg (\ atoms -> mapFlags (\ x -> x {debAction = Debianize}) atoms))
             "Generate a new debianization, replacing any existing one.  One of --debianize or --substvar is required.",
      Option "" ["substvar"] (ReqArg (\ name atoms -> mapFlags (\ x -> x {debAction = SubstVar (read name)}) atoms) "Doc, Prof, or Dev")
             (unlines ["Write out the list of dependencies required for the dev, prof or doc package depending",
                       "on the argument.  This value can be added to the appropriate substvars file."])
    ]

atomOptions :: HasAtoms atoms => [OptDescr (atoms -> atoms)]
atomOptions =
    [ Option "" ["executable"] (ReqArg (\ path x -> executableOption path (\ bin e -> doExecutable bin e x)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             "Create individual eponymous executable packages for these executables.  Other executables and data files are gathered into a single utils package.",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> insertAtom Source (CompilerVersion (last (map fst (readP_to_S parseVersion ver)))) x) "VERSION")
             "Version of GHC in build environment",
      Option "" ["disable-haddock"] (NoArg (\ x -> insertAtom Source NoDocumentationLibrary x))
             "Don't generate API documentation.  Use this if build is crashing due to a haddock error.",
      Option "" ["missing-dependency"] (ReqArg (\ name atoms -> missingDependency (BinPkgName name) atoms) "DEB")
             "Mark a package missing, do not add it to any dependency lists in the debianization.",
      Option "" ["source-package-name"] (ReqArg (\ name x -> setSourcePackageName (SrcPkgName name) x) "NAME")
             "Use this name for the debian source package.  Default is haskell-<cabalname>, where the cabal package name is downcased.",
      Option "" ["disable-library-profiling"] (NoArg (\ x -> insertAtom Source NoProfilingLibrary x))
             "Don't generate profiling libraries",
      Option "f" ["flags"] (ReqArg (\ fs atoms -> putCabalFlagAssignments (fromList (flagList fs)) atoms) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["maintainer"] (ReqArg (\ maint x -> insertAtom Source (DHMaintainer (either (error ("Invalid maintainer string: " ++ show maint)) id (parseMaintainer maint))) x) "Maintainer Name <email addr>")
             "Override the Maintainer name and email in $DEBEMAIL/$EMAIL/$DEBFULLNAME/$FULLNAME",
      Option "" ["build-dep"] (ReqArg (\ name atoms -> doDependencyHint (\ x -> x {buildDeps = BinPkgName name : (buildDeps x)}) atoms) "Debian binary package name")
             "Specify a package to add to the build dependency list for this source package, e.g. '--build-dep libglib2.0-dev'.",
      Option "" ["dev-dep"] (ReqArg (\ name atoms -> doDependencyHint (\ x -> x {extraDevDeps = BinPkgName name : (extraDevDeps x)}) atoms) "Debian binary package name")
             "Specify a package to add to the Depends: list of the -dev package, e.g. '--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep.",
      Option "" ["depends"] (ReqArg (\ arg atoms -> doDependencyHint (\ x -> x {binaryPackageDeps = parseDeps arg ++ binaryPackageDeps x}) atoms) "deb:deb,deb:deb,...")
             "Generalized --dev-dep - specify pairs A:B of debian binary package names, each A gets a Depends: B",
      Option "" ["conflicts"] (ReqArg (\ arg atoms -> doDependencyHint (\ x -> x {binaryPackageConflicts = parseDeps arg ++ binaryPackageConflicts x}) atoms) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Conflicts: B.  Note that B can have debian style version relations",
      Option "" ["map-dep"] (ReqArg (\ pair atoms -> doDependencyHint (\ x -> x {extraLibMap = case break (== '=') pair of
                                                                                                 (cab, (_ : deb)) -> Map.insertWith (++) cab [b deb] (extraLibMap x)
                                                                                                 (_, "") -> error "usage: --dep-map CABALNAME=DEBIANNAME"}) atoms) "CABALNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Extra-Library field of the cabal file to a debian binary package name, e.g. --dep-map cryptopp=libcrypto-dev",
      Option "" ["deb-version"] (ReqArg (\ version atoms -> doDependencyHint (\ x -> x {debVersion = Just (parseDebianVersion version)}) atoms) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (\ rev atoms -> doDependencyHint (\ x -> x {revision = rev}) atoms) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"] (ReqArg (\ pair atoms -> doDependencyHint (\ x -> x {epochMap =
                                                                                       case break (== '=') pair of
                                                                                         (_, (_ : ['0'])) -> epochMap x
                                                                                         (cab, (_ : [d])) | isDigit d -> Map.insert (PackageName cab) (ord d - ord '0') (epochMap x)
                                                                                         _ -> error "usage: --epoch-map CABALNAME=DIGIT"}) atoms) "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["exec-map"] (ReqArg (\ s atoms -> doDependencyHint (\ x -> x {execMap = case break (== '=') s of
                                                                                           (cab, (_ : deb)) -> Map.insert cab (b deb) (execMap x)
                                                                                           _ -> error "usage: --exec-map CABALNAME=DEBNAME"}) atoms) "EXECNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (\ atoms -> doDependencyHint (\ x -> x { omitLTDeps = True }) atoms))
             "Don't generate the << dependency when we see a cabal equals dependency.",
      Option "" ["quilt"] (NoArg (\ x -> insertAtom Source (DebSourceFormat Quilt3) x))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "" ["builddir"] (ReqArg (\ s atoms -> setBuildDir (s </> "build") atoms) "PATH")
             "Subdirectory where cabal does its build, dist/build by default, dist-ghc when run by haskell-devscripts.  The build subdirectory is added to match the behavior of the --builddir option in the Setup script."
    ]

-- | Process a --executable command line argument
executableOption :: String -> (BinPkgName -> InstallFile -> a) -> a
executableOption arg f =
    case span (/= ':') arg of
      (sp, md) ->
          let (sd, name) = splitFileName sp in
          f (BinPkgName name)
            (InstallFile { execName = name
                         , destName = name
                         , sourceDir = case sd of "./" -> Nothing; _ -> Just sd
                         , destDir = case md of (':' : dd) -> Just dd; _ -> Nothing })

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
withEnvironmentFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withEnvironmentFlags atoms0 f =
    (getEnv "CABALDEBIAN" >>= f . compileArgs atoms0 . read) `catchIOError` (\ _ -> f atoms0)

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
    debianize "." (compileArgs defaultAtoms args)

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
debianize :: HasAtoms atoms => FilePath -> atoms -> IO ()
debianize top atoms =
    withEnvironmentFlags atoms $ \ atoms' ->
    do old <- inputDebianization "."
       (new, dataDir) <- debianizationWithIO top atoms' old
       outputDebianization (dryRun (flags atoms')) (validate (flags atoms')) (buildDir "dist-ghc/build" atoms') dataDir old new
