-- | Command line option processing for building (formerly RPM, now)
-- Debian packages.

module Options
    ( getFlags
    , parseArgs
    , Flags(..)
    , DebAction(..)
    ) where

import Control.Monad (when)
import Data.Char (toLower, isDigit, ord)
import qualified Data.Map as Map
import Data.Version (Version, parseVersion)
import Debian.Relation (PkgName(..), BinPkgName(..))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.ReadE (readEOrFail)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import Distribution.Verbosity (Verbosity, flagToVerbosity, normal)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              usageInfo, getOpt')
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode (..))
import System.IO (Handle, hPutStrLn, stderr, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.TDFA ((=~))

import PackageInfo (DebType)

getFlags :: IO Flags
getFlags = getArgs >>= parseArgs

parseArgs :: [String] -> IO Flags
parseArgs args = do
     let (os, args', unknown, errs) = getOpt' RequireOrder options args
         opts = foldl (flip ($)) emptyFlags os
     when (rpmHelp opts || debAction opts == Usage) $ do
       printHelp stdout
       exitWith ExitSuccess
     when (not (null errs)) $ do
       hPutStrLn stderr "Errors:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
     when (not (null unknown)) $ do
       hPutStrLn stderr "Unrecognised options:"
       mapM_ (hPutStrLn stderr) unknown
       exitWith (ExitFailure 1)
     when (not (null args')) $ do
       hPutStrLn stderr "Unrecognised arguments:"
       mapM_ (hPutStrLn stderr) args'
       exitWith (ExitFailure 1)
     return opts

-- | Why rpm?  This started as a program to generate RPM packages from cabal files.  Yep.
data Flags = Flags
    {
      rpmPrefix :: FilePath
    , rpmCompiler :: CompilerFlavor
    , rpmCompilerVersion :: Maybe Version
    , rpmConfigurationsFlags :: [(FlagName, Bool)]
    , rpmHaddock :: Bool
    , rpmHelp :: Bool
    , debLibProf :: Bool
    , rpmName :: Maybe String
    , rpmOptimisation :: Bool
    , rpmRelease :: Maybe String
    , rpmSplitObjs :: Bool
    , debOutputDir :: FilePath
    , buildRoot :: FilePath
    , rpmVerbosity :: Verbosity
    , rpmVersion :: Maybe String
    , debMaintainer :: Maybe String
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
    }
    deriving (Eq, Show)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Eq, Show)

emptyFlags :: Flags

emptyFlags = Flags
    {
      rpmPrefix = "/usr/lib/haskell-packages/ghc6"
    , rpmCompiler = GHC
    , rpmCompilerVersion = Nothing
    , rpmConfigurationsFlags = []
    , rpmHaddock = True
    , rpmHelp = False
    , debLibProf = True
    , rpmName = Nothing
    , rpmOptimisation = True
    , rpmRelease = Nothing
    , rpmSplitObjs = True
    , debOutputDir = "./debian"
    , buildRoot = "/"
    , rpmVerbosity = normal
    , rpmVersion = Nothing
    , debMaintainer = Nothing
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
    }

options :: [OptDescr (Flags -> Flags)]

options =
    [ Option "" ["executable"] (ReqArg (\ name x -> x { executablePackages = name : executablePackages x }) "NAME")
             "Create individual eponymous executable packages for these executables.  Other executables and data files are gathered into a single utils package.",
      Option "" ["prefix"] (ReqArg (\ path x -> x { rpmPrefix = path }) "PATH")
             "Pass this prefix if we need to configure the package",
      Option "" ["ghc"] (NoArg (\x -> x { rpmCompiler = GHC }))
             "Compile with GHC",
      Option "" ["hugs"] (NoArg (\x -> x { rpmCompiler = Hugs }))
             "Compile with Hugs",
      Option "" ["jhc"] (NoArg (\x -> x { rpmCompiler = JHC }))
             "Compile with JHC",
      Option "" ["nhc"] (NoArg (\x -> x { rpmCompiler = NHC }))
             "Compile with NHC",
      Option "h?" ["help"] (NoArg (\x -> x { rpmHelp = True }))
             "Show this help text",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> x { rpmCompilerVersion = Just (last (map fst (readP_to_S parseVersion ver)))}) "VERSION")
             "Version of GHC in build environment",
      Option "" ["name"] (ReqArg (\name x -> x { rpmName = Just name }) "NAME")
             "Override the default package name",
      Option "" ["disable-haddock"] (NoArg (\x -> x { rpmHaddock = False }))
             "Don't generate API documentation.  Use this if build is crashing due to a haddock error.",
      Option "" ["disable-library-profiling"] (NoArg (\x -> x { debLibProf = False }))
             "Don't generate profiling libraries",
      Option "" ["disable-optimization"] (NoArg (\x -> x { rpmOptimisation = False }))
             "Don't generate optimised code",
      Option "" ["disable-split-objs"] (NoArg (\x -> x { rpmSplitObjs = False }))
             "Don't split object files to save space",
      Option "f" ["flags"] (ReqArg (\flags x -> x { rpmConfigurationsFlags = rpmConfigurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
      Option "" ["debdir"] (ReqArg (\path x -> x { debOutputDir = path }) "DEBDIR")
             ("Override the default output directory (" ++ show (debOutputDir emptyFlags) ++ ")"),
      Option "" ["root"] (ReqArg (\ path x -> x { buildRoot = path }) "BUILDROOT")
             "Use the compiler information in the given build environment.",
      Option "v" ["verbose"] (ReqArg (\verb x -> x { rpmVerbosity = readEOrFail flagToVerbosity verb }) "n")
             "Change build verbosity",
      Option "" ["version"] (ReqArg (\vers x -> x { rpmVersion = Just vers }) "VERSION")
             "Override the default package version",
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
      Option "" ["map-dep"] (ReqArg (\ pair x -> x {depMap = case break (== '=') pair of
                                                               (cab, (_ : deb)) -> Map.insertWith (++) cab [BinPkgName (PkgName deb)] (depMap x)
                                                               (_, "") -> error "usage: --dep-map CABALNAME=DEBIANNAME"}) "CABALNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Extra-Library field of the cabal file to a debian binary package name, e.g. --dep-map cryptopp=libcrypto-dev",
      -- Option "" ["deb-name"] (ReqArg (\ name x -> x {debName = Just name}) "NAME")
      --        "Specify the base name of the debian package, the part between 'libghc-' and '-dev'.  Normally this is the downcased cabal name.",
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
                                                              (cab, (_ : deb)) -> Map.insert cab (BinPkgName (PkgName deb)) (execMap x)
                                                              _ -> error "usage: --exec-map CABALNAME=DEBNAME"}) "EXECNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (\x -> x { omitLTDeps = True }))
             "Don't generate the << dependency when we see a cabal equals dependency.",
      Option "" ["quilt"] (NoArg (\ x -> x {sourceFormat = "3.0 (quilt)"}))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "n" ["dry-run", "compare"] (NoArg (\ x -> x {compareOnly = True}))
             "Just compare the existing debianization to the one we would generate."
    ]

parseDeps :: String -> [(String, String)]
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
            (_, _, _, [a, b]) -> (a, b)
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
