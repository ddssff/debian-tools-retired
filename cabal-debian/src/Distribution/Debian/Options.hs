-- | Command line option processing for building (formerly RPM, now)
-- Debian packages.

module Distribution.Debian.Options
    ( withFlags
    , getFlags
    , parseArgs
    , compileArgs
    ) where

import Control.Monad (when)
import Data.Char (toLower, isDigit, ord)
import qualified Data.Map as Map
import Data.Version (parseVersion)
import Debian.Relation (PkgName(..), BinPkgName(..))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Debian.Config (Flags(..), DebAction(Usage, Debianize, SubstVar), defaultFlags)
import Distribution.Debian.Server (Executable(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              usageInfo, getOpt')
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode (..))
import System.FilePath (splitFileName, (</>))
import System.IO (Handle, hPutStrLn, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.TDFA ((=~))

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
    [ Option "" ["executable"] (ReqArg (\ path x ->
                                            case span (/= ':') path of
                                              (sp, md) ->
                                                  let (sd, name) = splitFileName sp in
                                                  x { executablePackages =
                                                          Executable { execName = name
                                                                     , sourceDir = case sd of "./" -> Nothing; _ -> Just sd
                                                                     , destDir = case md of (':' : dd) -> Just dd; _ -> Nothing
                                                                     , execServer = Nothing } : executablePackages x }) "SOURCEPATH or SOURCEPATH:DESTDIR")
             "Create individual eponymous executable packages for these executables.  Other executables and data files are gathered into a single utils package.",
      Option "" ["ghc"] (NoArg (\x -> x { compilerFlavor = GHC }))
             "Compile with GHC",
      Option "" ["hugs"] (NoArg (\x -> x { compilerFlavor = Hugs }))
             "Compile with Hugs",
      Option "" ["jhc"] (NoArg (\x -> x { compilerFlavor = JHC }))
             "Compile with JHC",
      Option "" ["nhc"] (NoArg (\x -> x { compilerFlavor = NHC }))
             "Compile with NHC",
      Option "h?" ["help"] (NoArg (\x -> x { help = True }))
             "Show this help text",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> x { compilerVersion = Just (last (map fst (readP_to_S parseVersion ver)))}) "VERSION")
             "Version of GHC in build environment",
      Option "" ["disable-haddock"] (NoArg (\x -> x { haddock = False }))
             "Don't generate API documentation.  Use this if build is crashing due to a haddock error.",
      Option "" ["missing-dependency"] (ReqArg (\ name x -> x {missingDependencies = name : missingDependencies x}) "DEB")
             "Mark a package missing, do not add it to any dependency lists in the debianization.",
      Option "" ["disable-library-profiling"] (NoArg (\x -> x { debLibProf = False }))
             "Don't generate profiling libraries",
      Option "f" ["flags"] (ReqArg (\flags x -> x { configurationsFlags = configurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "v" ["verbose"] (ReqArg (\s x -> x { verbosity = read s }) "n")
             "Change build verbosity",
      Option "" ["maintainer"] (ReqArg (\maint x -> x { debMaintainer = Just maint }) "Maintainer Name <email addr>")
             "Override the Maintainer name and email in $DEBEMAIL/$EMAIL/$DEBFULLNAME/$FULLNAME",
      Option "" ["debianize"] (NoArg (\x -> x {debAction = Debianize}))
             "Generate a new debianization, replacing any existing one.  One of --debianize or --substvar is required.",
      Option "" ["build-dep"] (ReqArg (\ name x -> x {buildDeps = name : (buildDeps x)}) "Debian binary package name")
             "Specify a package to add to the build dependency list for this source package, e.g. '--build-dep libglib2.0-dev'.",
      Option "" ["dev-dep"] (ReqArg (\ name x -> x {extraDevDeps = name : (extraDevDeps x)}) "Debian binary package name")
             "Specify a package to add to the Depends: list of the -dev package, e.g. '--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep.",
      Option "" ["depends"] (ReqArg (\ arg x -> x {binaryPackageDeps = parseDeps arg ++ binaryPackageDeps x}) "deb:deb,deb:deb,...")
             "Generalized --dev-dep - specify pairs A:B of debian binary package names, each A gets a Depends: B",
      Option "" ["conflicts"] (ReqArg (\ arg x -> x {binaryPackageConflicts = parseDeps arg ++ binaryPackageConflicts x}) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Conflicts: B.  Note that B can have debian style version relations",
      Option "" ["map-dep"] (ReqArg (\ pair x -> x {extraLibMap = case break (== '=') pair of
                                                                    (cab, (_ : deb)) -> Map.insertWith (++) cab [b deb] (extraLibMap x)
                                                                    (_, "") -> error "usage: --dep-map CABALNAME=DEBIANNAME"}) "CABALNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Extra-Library field of the cabal file to a debian binary package name, e.g. --dep-map cryptopp=libcrypto-dev",
      Option "" ["deb-version"] (ReqArg (\ version x -> x {debVersion = Just version}) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (\ rev x -> x {revision = rev}) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"] (ReqArg (\ pair x -> x {epochMap =
                                                          case break (== '=') pair of
                                                            (_, (_ : ['0'])) -> epochMap x
                                                            (cab, (_ : [d])) | isDigit d -> Map.insert (PackageName cab) (ord d - ord '0') (epochMap x)
                                                            _ -> error "usage: --epoch-map CABALNAME=DIGIT"}) "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["substvar"] (ReqArg (\ name x -> x {debAction = SubstVar (read name)}) "Doc, Prof, or Dev")
             (unlines ["Write out the list of dependencies required for the dev, prof or doc package depending",
                       "on the argument.  This value can be added to the appropriate substvars file."]),
      Option "" ["exec-map"] (ReqArg (\ s x -> x {execMap = case break (== '=') s of
                                                              (cab, (_ : deb)) -> Map.insert cab (b deb) (execMap x)
                                                              _ -> error "usage: --exec-map CABALNAME=DEBNAME"}) "EXECNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (\x -> x { omitLTDeps = True }))
             "Don't generate the << dependency when we see a cabal equals dependency.",
      Option "" ["self-depend"] (NoArg (\ x -> x {selfDepend = True}))
             "Add a build dependency on libghc-cabal-debian-dev (this library.)",
      Option "" ["quilt"] (NoArg (\ x -> x {sourceFormat = "3.0 (quilt)"}))
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
b = BinPkgName . PkgName
