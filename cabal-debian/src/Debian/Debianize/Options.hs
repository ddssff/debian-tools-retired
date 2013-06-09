module Debian.Debianize.Options
    ( compileArgs
    , options
    ) where

import Data.Char (toLower, isDigit, ord)
import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Set as Set (fromList, insert, union, singleton)
import Data.Version (parseVersion)
import Debian.Debianize.Atoms -- (Atoms, depends, conflicts)
import Debian.Debianize.Goodies (doExecutable)
import Debian.Debianize.Types (InstallFile(..), DebAction(..))
import Debian.Orphans ()
import Debian.Policy (SourceFormat(Quilt3), parseMaintainer)
import Debian.Relation (BinPkgName(..), SrcPkgName(..), Relation(..))
import Debian.Version (parseDebianVersion)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import Prelude hiding (readFile, lines, null, log, sum)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..), ArgOrder(RequireOrder), getOpt')
import System.FilePath ((</>), splitFileName)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.TDFA ((=~))

compileArgs :: [String] -> Atoms -> Atoms
compileArgs args atoms =
    case getOpt' RequireOrder options args of
      (os, [], [], []) -> foldl (flip ($)) atoms os
      (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                    ", Unrecognized: " ++ show unk ++
                                    ", Non-Options: " ++ show non)

-- | Options that modify other atoms.
options :: [OptDescr (Atoms -> Atoms)]
options =
    [ Option "v" ["verbose"] (ReqArg (\ s atoms -> setL verbosity (read s) atoms) "n")
             "Change build verbosity",
      Option "n" ["dry-run", "compare"] (NoArg (\ atoms -> setL dryRun True atoms))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (\ atoms -> setL debAction Usage atoms))
             "Show this help text",
      Option "" ["debianize"] (NoArg (\ atoms -> setL debAction Debianize atoms))
             "Generate a new debianization, replacing any existing one.  One of --debianize or --substvar is required.",
      Option "" ["substvar"] (ReqArg (\ name atoms -> setL debAction (SubstVar (read name)) atoms) "Doc, Prof, or Dev")
             (unlines ["Write out the list of dependencies required for the dev, prof or doc package depending",
                       "on the argument.  This value can be added to the appropriate substvars file."]),
      Option "" ["executable"] (ReqArg (\ path x -> executableOption path (\ bin e -> doExecutable bin e x)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             "Create individual eponymous executable packages for these executables.  Other executables and data files are gathered into a single utils package.",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> setL compilerVersion (Just (last (map fst (readP_to_S parseVersion ver)))) x) "VERSION")
             "Version of GHC in build environment",
      Option "" ["disable-haddock"] (NoArg (setL noDocumentationLibrary True))
             "Don't generate API documentation.  Use this if build is crashing due to a haddock error.",
      Option "" ["missing-dependency"] (ReqArg (\ name atoms -> modL missingDependencies (insert (BinPkgName name)) atoms) "DEB")
             "Mark a package missing, do not add it to any dependency lists in the debianization.",
      Option "" ["source-package-name"] (ReqArg (\ name x -> setL sourcePackageName (Just (SrcPkgName name)) x) "NAME")
             "Use this name for the debian source package.  Default is haskell-<cabalname>, where the cabal package name is downcased.",
      Option "" ["disable-library-profiling"] (NoArg (setL noProfilingLibrary True))
             "Don't generate profiling libraries",
      Option "f" ["flags"] (ReqArg (\ fs atoms -> modL cabalFlagAssignments (union (fromList (flagList fs))) atoms) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["maintainer"] (ReqArg (\ maint x -> setL maintainer (either (error ("Invalid maintainer string: " ++ show maint)) Just (parseMaintainer maint)) x) "Maintainer Name <email addr>")
             "Override the Maintainer name and email in $DEBEMAIL/$EMAIL/$DEBFULLNAME/$FULLNAME",
      Option "" ["build-dep"] (ReqArg (\ name atoms -> modL buildDeps (Set.insert (Rel (BinPkgName name) Nothing Nothing)) atoms) "Debian binary package name")
             "Specify a package to add to the build dependency list for this source package, e.g. '--build-dep libglib2.0-dev'.",
      Option "" ["build-dep-indep"] (ReqArg (\ name atoms -> modL buildDepsIndep (Set.insert (Rel (BinPkgName name) Nothing Nothing)) atoms) "Debian binary package name")
             "Specify a package to add to the architecture independent build dependency list for this source package, e.g. '--build-dep-indep perl'.",
      Option "" ["dev-dep"] (ReqArg (\ name atoms -> modL extraDevDeps (Set.insert (Rel (BinPkgName name) Nothing Nothing)) atoms) "Debian binary package name")
             "Specify a package to add to the Depends: list of the -dev package, e.g. '--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep.",
      Option "" ["depends"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL depends (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Generalized --dev-dep - specify pairs A:B of debian binary package names, each A gets a Depends: B",
      Option "" ["conflicts"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL conflicts (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Conflicts: B.  Note that B can have debian style version relations",
      Option "" ["replaces"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL replaces (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Replaces: B.  Note that B can have debian style version relations",
      Option "" ["provides"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL provides (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Specify pairs A:B of debian binary package names, each A gets a Provides: B.  Note that B can have debian style version relations",
      Option "" ["map-dep"] (ReqArg (\ pair atoms -> case break (== '=') pair of
                                                       (cab, (_ : deb)) -> modL extraLibMap (Map.insertWith Set.union cab (singleton (b deb))) atoms
                                                       (_, "") -> error "usage: --map-dep CABALNAME=DEBIANNAME") "CABALNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Extra-Library field of the cabal file to a debian binary package name, e.g. --dep-map cryptopp=libcrypto-dev",
      Option "" ["deb-version"] (ReqArg (\ version atoms -> setL debVersion (Just (parseDebianVersion version)) atoms) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (setL revision . Just) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"] (ReqArg (\ pair atoms -> case break (== '=') pair of
                                                         (_, (_ : ['0'])) -> atoms
                                                         (cab, (_ : [d])) | isDigit d -> modL epochMap (Map.insertWith (flip const) (PackageName cab) (ord d - ord '0')) atoms
                                                         _ -> error "usage: --epoch-map CABALNAME=DIGIT") "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["exec-map"] (ReqArg (\ s atoms -> case break (== '=') s of
                                                     (cab, (_ : deb)) -> modL execMap (Map.insertWith (flip const) cab (b deb)) atoms
                                                     _ -> error "usage: --exec-map CABALNAME=DEBNAME") "EXECNAME=DEBIANNAME")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (setL omitLTDeps True))
             "Don't generate the << dependency when we see a cabal equals dependency.",
      Option "" ["quilt"] (NoArg (setL sourceFormat (Just Quilt3)))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "" ["builddir"] (ReqArg (\ s atoms -> setL buildDir (Just (s </> "build")) atoms) "PATH")
             "Subdirectory where cabal does its build, dist/build by default, dist-ghc when run by haskell-devscripts.  The build subdirectory is added to match the behavior of the --builddir option in the Setup script."
    ]

anyrel :: BinPkgName -> Relation
anyrel x = Rel x Nothing Nothing

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

parseDeps :: String -> [(BinPkgName, Relation)]
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
            (_, _, _, [x, y]) -> (b x, anyrel (b y))
            _ -> error $ "Invalid dependency: " ++ show s

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

b :: String -> BinPkgName
b = BinPkgName
