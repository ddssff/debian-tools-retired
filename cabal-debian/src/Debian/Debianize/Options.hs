module Debian.Debianize.Options
    ( compileArgs
    , options
    ) where

import Data.Char (toLower, isDigit, ord)
import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Set as Set (fromList, insert, union, singleton)
import Data.Version (parseVersion)
import Debian.Debianize.Goodies (doExecutable)
import Debian.Debianize.Lenses -- (Atoms, depends, conflicts)
import Debian.Debianize.Types (InstallFile(..), DebAction(..))
import Debian.Debianize.Utility (read')
import Debian.Orphans ()
import Debian.Policy (SourceFormat(Quilt3), parseMaintainer)
import Debian.Relation (BinPkgName(..), SrcPkgName(..), Relations, Relation(..))
import Debian.Relation.String (parseRelations)
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
    [ Option "v" ["verbose"] (ReqArg (\ s atoms -> setL verbosity (read' (\ s' -> error $ "verbose: " ++ show s') s) atoms) "n")
             "Change the amount of progress messages generated",
      Option "n" ["dry-run", "compare"] (NoArg (\ atoms -> setL dryRun True atoms))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (\ atoms -> setL debAction Usage atoms))
             "Show this help text",
      Option "" ["executable"] (ReqArg (\ path x -> executableOption path (\ bin e -> doExecutable bin e x)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             (unlines [ "Create an individual binary package to hold this executable.  Other executables "
                      , " and data files are gathered into a single utils package named 'haskell-packagename-utils'."]),
      Option "" ["ghc-version"] (ReqArg (\ ver x -> setL compilerVersion (Just (last (map fst (readP_to_S parseVersion ver)))) x) "VERSION")
             (unlines [ "Version of GHC in build environment.  Without this option it is assumed that"
                      , "the version of GHC in the build environment is the same as the one in the"
                      , "environment in which cabal-debian is running. (the usual case.)  The GHC"
                      , "version is used to determine which packages are bundled with GHC - if a"
                      , "package is bundled with GHC it is not necessary to add a build dependency for"
                      , "that package to the debian/control file."]),
      Option "" ["disable-haddock"] (NoArg (setL noDocumentationLibrary True))
             (unlines [ "Don't generate API documentation packages, usually named"
                      , "libghc-packagename-doc.  Use this if your build is crashing due to a"
                      , "haddock bug."]),
      Option "" ["missing-dependency"] (ReqArg (\ name atoms -> modL missingDependencies (insert (BinPkgName name)) atoms) "DEB")
             (unlines [ "This is the counterpart to --disable-haddock.  It prevents a package"
                      , "from being added to the build dependencies.  This is necessary, for example,"
                      , "when a dependency package was built with the --disable-haddock option, because"
                      , "normally cabal-debian assumes that the -doc package exists and adds it as a"
                      , "build dependency."]),
      Option "" ["source-package-name"] (ReqArg (\ name x -> setL sourcePackageName (Just (SrcPkgName name)) x) "NAME")
             (unlines [ "Use this name for the debian source package, the name in the Source field at the top of the"
                      , "debian control file, and also at the very beginning of the debian/changelog file.  By default"
                      , "this is haskell-<cabalname>, where the cabal package name is downcased."]),
      Option "" ["disable-library-profiling"] (NoArg (setL noProfilingLibrary True))
             (unlines [ "Don't generate profiling (-prof) library packages.  This has been used in one case"
                      , "where the package code triggered a compiler bug."]),
      Option "" ["maintainer"] (ReqArg (\ maint x -> setL maintainer (either (error ("Invalid maintainer string: " ++ show maint)) Just (parseMaintainer maint)) x) "Maintainer Name <email addr>")
             (unlines [ "Override the Maintainer name and email given in $DEBEMAIL or $EMAIL or $DEBFULLNAME or $FULLNAME"]),
      Option "" ["build-dep"]
                 (ReqArg (\ name atoms ->
                              modL buildDeps
                                       (case parseRelations name of
                                          Right rss -> Set.insert rss
                                          Left err -> error ("cabal-debian option --build-dep " ++ show name ++ ": " ++ show err)) atoms) "Debian package relations")
                 (unlines [ "Add a dependency relation to the Build-Depends: field for this source package, e.g."
                          , ""
                          , "     --build-dep libglib2.0-dev"
                          , "     --build-dep 'libglib2.0-dev >= 2.2'" ]),
      Option "" ["build-dep-indep"]
                 (ReqArg (\ name atoms ->
                              modL buildDepsIndep
                                       (case parseRelations name of
                                          Right rss -> Set.insert rss
                                          Left err -> error ("cabal-debian option --build-dep-indep " ++ show name ++ ": " ++ show err)) atoms) "Debian binary package name")
                 (unlines [ "Similar to --build-dep, but the dependencies are added to Build-Depends-Indep, e.g.:"
                          , ""
                          , "    --build-dep-indep perl" ]),
      Option "" ["dev-dep"] (ReqArg (\ name atoms -> modL extraDevDeps (Set.insert (Rel (BinPkgName name) Nothing Nothing)) atoms) "Debian binary package name")
             (unlines [ "Add an entry to the Depends: field of the -dev package, e.g."
                      , "'--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep."]),
      Option "" ["depends"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL depends (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             (unlines [ "Generalized --dev-dep - specify pairs A:B of debian binary package names, each"
                      , "A gets a Depends: B.  Note that B can have debian style version relations"]),
      Option "" ["conflicts"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL conflicts (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Conflicts field.",
      Option "" ["replaces"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL replaces (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Replaces field.",
      Option "" ["provides"] (ReqArg (\ arg atoms -> foldr (\ (p, r) atoms' -> modL provides (Map.insertWith union p (singleton r)) atoms') atoms (parseDeps arg)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Provides field.",
      Option "" ["map-dep"] (ReqArg (\ pair atoms -> case break (== '=') pair of
                                                       (cab, (_ : deb)) -> modL extraLibMap (Map.insertWith Set.union cab (singleton (rels deb))) atoms
                                                       (_, "") -> error "usage: --map-dep CABALNAME=RELATIONS") "CABALNAME=RELATIONS")
             (unlines [ "Specify what debian package name corresponds with a name that appears in"
                      , "the Extra-Library field of a cabal file, e.g. --map-dep cryptopp=libcrypto-dev."
                      , "I think this information is present somewhere in the packaging system, but"
                      , "I'm not sure of the details."]),
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
                                                     (cab, (_ : deb)) -> modL execMap (Map.insertWith (flip const) cab (rels deb)) atoms
                                                     _ -> error "usage: --exec-map EXECNAME=RELATIONS") "EXECNAME=RELATIONS")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-lt-deps"] (NoArg (setL omitLTDeps True))
             (unlines [ "Remove all less-than dependencies from the generated control file.  Less-than"
                      , "dependencies are less useful and more troublesome for debian packages than cabal,"
                      , "because you can't install multiple versions of a given debian package.  For more"
                      , "google 'cabal hell'."]),
      Option "" ["quilt"] (NoArg (setL sourceFormat (Just Quilt3)))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "" ["builddir"] (ReqArg (\ s atoms -> setL buildDir (Just (s </> "build")) atoms) "PATH")
             (unlines [ "Subdirectory where cabal does its build, dist/build by default, dist-ghc when"
                      , "run by haskell-devscripts.  The build subdirectory is added to match the"
                      , "behavior of the --builddir option in the Setup script."]),

      Option "f" ["flags"] (ReqArg (\ fs atoms -> modL cabalFlagAssignments (union (fromList (flagList fs))) atoms) "FLAGS")
             (unlines [ "Flags to pass to the finalizePackageDescription function in"
                      , "Distribution.PackageDescription.Configuration when loading the cabal file."]),

      Option "" ["debianize"] (NoArg (\ atoms -> setL debAction Debianize atoms))
             "Deprecated - formerly used to get what is now the normal benavior.",
      Option "" ["substvar"] (ReqArg (\ name atoms -> setL debAction (SubstVar (read' (\ s -> error $ "substvar: " ++ show s) name)) atoms) "Doc, Prof, or Dev")
             (unlines [ "With this option no debianization is generated.  Instead, the list"
                      , "of dependencies required for the dev, prof or doc package (depending"
                      , "on the argument) is printed to standard output.  These can be added"
                      , "to the appropriate substvars file.  (This is an option whose use case"
                      , "is lost in the mists of time.)"])
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

rels :: String -> Relations
rels s =
    case parseRelations s of
      Right relss -> relss
      _ -> error $ "Parse error in debian relations: " ++ show s
