{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Cabal.Dependencies
    ( DependencyHints(..)
    , defaultDependencyHints
    , PackageType(..)
    , VersionSplits(..)
    , filterMissing' -- Debian.Cabal.SubstVars
    , cabalDependencies -- Debian.Cabal.SubstVars
    , selfDependency -- Debian.Debianize.Combinators
    , dependencies
    , allBuildDepends
    , debianName
    , PackageInfo(..)
    , debDeps
    , debNameFromType
    , debianBuildDeps
    , debianBuildDepsIndep
    ) where

import Data.Char (toLower, isSpace)
import Data.Function (on)
import Data.List (nub, minimumBy, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Version (showVersion)
import Debian.Cabal.Bundled (ghcBuiltIn)
import Debian.Control
import Debian.Debianize.Interspersed (Interspersed(foldInverted, leftmost, pairs), foldTriples)
import Debian.Debianize.Types.Debianization as Debian (DebType(Dev, Prof, Doc))
import Debian.Relation (Relations, Relation, BinPkgName, PkgName(pkgNameFromString))
import qualified Debian.Relation as D
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription as Cabal (PackageDescription(..), allBuildInfo, buildTools, pkgconfigDepends, extraLibs)
import Distribution.Simple.Compiler (Compiler)
import Distribution.Version (Version(Version), VersionRange, anyVersion, foldVersionRange', intersectVersionRanges, unionVersionRanges,
                             laterVersion, orLaterVersion, earlierVersion, orEarlierVersion, fromVersionIntervals, toVersionIntervals, withinVersion,
                             isNoVersion, asVersionIntervals)
import Distribution.Version.Invert (invertVersionRange)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

data DependencyHints
    = DependencyHints
      {
      ---------------------------------------------------------------------------
      -- Build Dependencies - Controling the Build-Depends and
      -- Build-Depends-Indep fields of the debian source package.
      ---------------------------------------------------------------------------
        buildDeps :: [BinPkgName]
      -- ^ Add a debian binary package to the debianization's list of
      -- build dependencies.  These will be in addition to dependencies
      -- derived from the cabal build-depends list.
      , missingDependencies :: [BinPkgName]
      -- ^ Mark a package missing, do not add it to any dependency lists
      -- in the debianization.  If some cabal build dependency foo has
      -- no documentation package (because it was built with the haddock
      -- option set to False) you would need to add libghc-foo-doc to
      -- this list.
      , extraLibMap :: Map.Map String [BinPkgName]
      -- ^ Specify which debian binary packages correspond to the
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

      ---------------------------------------------------------------------------
      -- Install Dependencies - controlling the Depends, Conflicts,
      -- Provides, and Replaces fields of the debian binary packages.
      ---------------------------------------------------------------------------
      , extraDevDeps :: [BinPkgName]
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

      ---------------------------------------------------------------
      -- Debian Binary Packages - Controlling which debian binary ---
      -- packages will be created.                                ---
      ---------------------------------------------------------------
      , debLibProf :: Bool
      -- ^ Should we generate? profiling libraries.  At certain times it
      -- was necessary to turn this off to work around compiler bugs.
      , haddock :: Bool
      -- ^ Should we generate the library documentation package?  Also
      -- used at times to avoid haddock bugs or, as the case may be,
      -- bugs in the haddock markup of some packages.

      -------------------------
      -- Version Numbers
      -------------------------
      , epochMap :: Map.Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @Map.insert epochMap "HTTP" 1@. 
      , revision :: String
      -- ^ Specify the revision string to use when converting the cabal
      -- version to debian.
      , debVersion :: Maybe DebianVersion
      -- ^ Specify the exact debian version of the resulting package,
      -- including epoch.  One use case is to work around the the
      -- "buildN" versions that are often uploaded to the debian and
      -- ubuntu repositories.  Say the latest cabal version of
      -- transformers is 0.3.0.0, but the debian repository contains
      -- version 0.3.0.0-1build3, we need to specify
      -- debVersion="0.3.0.0-1build3" or the version we produce will
      -- look older than the one already available upstream.
      , versionSplits :: D.PkgName name => PackageType -> [VersionSplits name]
      -- ^ Instances where the debian package name is different (for
      -- some range of version numbers) from the default constructed
      -- by mkPkgName.
      }

defaultDependencyHints :: DependencyHints
defaultDependencyHints =
    DependencyHints
    { haddock = True
    , missingDependencies = []
    , debLibProf = True
    , extraDevDeps = []
    , extraLibMap = Map.empty
    , binaryPackageDeps = []
    , binaryPackageConflicts = []
    , execMap = Map.empty
    , omitLTDeps = False
    , buildDeps = []
    , epochMap = Map.empty
    -- Setting revision to @-1~hackage1@ produces a version number
    -- that is slightly older looking than an initial version from
    -- debian, which will have revision @-1@.  However, it really
    -- ought to be a site policy.
    -- , revision = "-1~hackage1"
    , revision = ""
    , debVersion = Nothing
    , versionSplits = knownVersionSplits
    }

data PackageType = Source | Development | Profiling | Documentation | Utilities | Exec | {- ServerPackage | -} Extra deriving (Eq, Show)

data VersionSplits name
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: name
      , splits :: [(Version, name)] -- Assumed to be in version number order
      }

instance PkgName name => Interspersed (VersionSplits name) name Version where
    leftmost (VersionSplits {splits = []}) = error "Empty Interspersed instance"
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: D.PkgName name => PackageType -> [VersionSplits name]
knownVersionSplits typ =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = mkPkgName (PackageName "parsec2") typ
      , splits = [(Version [3] [], mkPkgName (PackageName "parsec3") typ)] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = mkPkgName (PackageName "quickcheck1") typ
      , splits = [(Version [2] [], mkPkgName (PackageName "quickcheck2") typ)] }
    ]

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: DependencyHints -> Compiler -> PackageType -> PackageName -> VersionRange -> Relations
dependencies hints compiler typ name cabalRange =
    map doBundled $ convert' (canonical (Or (catMaybes (map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(BinPkgName, VersionRange)]
      alts = case Map.lookup name (packageSplits (versionSplits hints) typ) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName name typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits' -> packageRangesFromVersionSplits splits'

      convert :: (BinPkgName, VersionRange) -> Maybe (Rels Relation)
      convert (dname, range) =
          if isNoVersion range'''
          then Nothing
          else Just $
               foldVersionRange'
                 (Rel (D.Rel dname Nothing Nothing))
                 (\ v -> Rel (D.Rel dname (Just (D.EEQ (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname (Just (D.SGR (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname (Just (D.SLT (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname (Just (D.GRE (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname (Just (D.LTE (dv v))) Nothing))
                 (\ x y -> And [Rel (D.Rel dname (Just (D.GRE (dv x))) Nothing), Rel (D.Rel dname (Just (D.SLT (dv y))) Nothing)])
                 (\ x y -> Or [x, y])
                 (\ x y -> And [x, y])
                 id
                 range'''
          where 
            -- Choose the simpler of the two
            range''' = canon (simpler range' range'')
            -- Unrestrict the range for versions that we know don't exist for this debian package
            range'' = canon (unionVersionRanges range' (invertVersionRange range))
            -- Restrict the range to the versions specified for this debian package
            range' = intersectVersionRanges cabalRange' range
            -- When we see a cabal equals dependency we need to turn it into
            -- a wildcard because the resulting debian version numbers have
            -- various suffixes added.
      cabalRange' =
          foldVersionRange'
            anyVersion
            withinVersion  -- <- Here we are turning equals into wildcard
            laterVersion
            earlierVersion
            orLaterVersion
            orEarlierVersion
            (\ lb ub -> intersectVersionRanges (orLaterVersion lb) (earlierVersion ub))
            unionVersionRanges
            intersectVersionRanges
            id
            cabalRange
      -- Convert a cabal version to a debian version, adding an epoch number if requested
      dv v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (epochMap hints)) ++ showVersion v)
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

      -- If a package is bundled with the compiler we make the
      -- compiler a substitute for that package.  If we were to
      -- specify the virtual package (e.g. libghc-base-dev) we would
      -- have to make sure not to specify a version number.
      doBundled :: [D.Relation] -> [D.Relation]
      doBundled rels | ghcBuiltIn compiler name = rels ++ [D.Rel (compilerPackageName typ) Nothing Nothing]
      doBundled rels = rels

      compilerPackageName Documentation = D.BinPkgName "ghc-doc"
      compilerPackageName Profiling = D.BinPkgName "ghc-prof"
      compilerPackageName Development = D.BinPkgName "ghc"
      compilerPackageName _ = D.BinPkgName "ghc" -- whatevs

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel {unRel :: a} deriving Show

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel rel) = And [Or [Rel rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . map Or $ sequence $ map (concat . map unOr . unAnd . canonical) $ rels

convert' :: Rels a -> [[a]]
convert' = map (map unRel . unOr) . unAnd . canonical

packageSplits :: (PackageType -> [VersionSplits BinPkgName]) -> PackageType -> Map.Map PackageName (VersionSplits BinPkgName)
packageSplits splits typ =
    foldr (\ splits' mp -> Map.insertWith multipleSplitsError (packageName splits') splits' mp)
          Map.empty
          (splits typ)
    where
      multipleSplitsError :: VersionSplits BinPkgName -> a -> b
      multipleSplitsError (VersionSplits {packageName = PackageName p}) _s2 =
          error ("Multiple splits for package " ++ show p)

packageRangesFromVersionSplits :: (PkgName name) => VersionSplits name -> [(name, VersionRange)]
packageRangesFromVersionSplits splits =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 splits

-- | Build a debian package name from a cabal package name and a
-- debian package type.  Unfortunately, this does not enforce the
-- correspondence between the PackageType value and the name type, so
-- it can return nonsense like (SrcPkgName "libghc-debian-dev").
mkPkgName :: PkgName name => PackageName -> PackageType -> name
mkPkgName (PackageName name) typ =
    pkgNameFromString $
             case typ of
                Source -> "haskell-" ++ base ++ ""
                Documentation -> "libghc-" ++ base ++ "-doc"
                Development -> "libghc-" ++ base ++ "-dev"
                Profiling -> "libghc-" ++ base ++ "-prof"
                Utilities -> "haskell-" ++ base ++ "-utils"
                Exec -> base
                -- ServerPackage -> base
                Extra -> base
    where
      base = map (fixChar . toLower) name
      -- Underscore is prohibited in debian package names.
      fixChar :: Char -> Char
      fixChar '_' = '-'
      fixChar c = toLower c

filterMissing' :: DependencyHints -> [[D.Relation]] -> [[D.Relation]]
filterMissing' hints rels =
    filter (/= []) (map (filter (\ (D.Rel name _ _) -> not (elem name (missingDependencies hints)))) rels)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs D.BinPkgName
    deriving (Eq, Show)

-- | In cabal a self dependency probably means the library is needed
-- while building the executables.  In debian it would mean that the
-- package needs an earlier version of itself to build, so we use this
-- to filter such dependencies out.
selfDependency :: PackageIdentifier -> Dependency_ -> Bool
selfDependency pkgId (BuildDepends (Dependency name _)) = name == pkgName pkgId
selfDependency _ _ = False

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

cabalDependencies :: DependencyHints -> PackageDescription -> [Dependency]
cabalDependencies hints pkgDesc =
    catMaybes $ map unboxDependency $ allBuildDepends hints (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc) (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc)

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: DependencyHints -> [Dependency] -> [Dependency] -> [Dependency] -> [String] -> [Dependency_]
allBuildDepends hints buildDepends buildTools pkgconfigDepends extraLibs =
    nub $ map BuildDepends buildDepends ++
          map BuildTools buildTools ++
          map PkgConfigDepends pkgconfigDepends ++
          map ExtraLibs (fixDeps extraLibs)
    where
      fixDeps :: [String] -> [D.BinPkgName]
      fixDeps xs = concatMap (\ cab -> fromMaybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] (Map.lookup cab (extraLibMap hints))) xs

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: DependencyHints -> Compiler -> Dependency_ -> D.Relations
buildDependencies hints compiler (BuildDepends (Dependency name ranges)) =
    dependencies hints compiler Development name ranges ++
    dependencies hints compiler Profiling name ranges
buildDependencies hints _compiler dep@(ExtraLibs _) =
    concat (map dependency $ adapt (execMap hints) dep)
buildDependencies hints _compiler dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          concat (map dependency $ adapt (execMap hints) dep)
      Nothing ->
          []

dependency :: D.BinPkgName -> D.Relations
dependency name = [[D.Rel name Nothing Nothing]]

adapt :: Map.Map String D.BinPkgName -> Dependency_ -> [D.BinPkgName]
adapt execMap (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt execMap (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency (PackageName pkg) _)) = [D.BinPkgName pkg]

-- There are two reasons this may not work, or may work
-- incorrectly: (1) the build environment may be a different
-- distribution than the parent environment (the environment the
-- autobuilder was run from), so the packages in that
-- environment might have different names, and (2) the package
-- we are looking for may not be installed in the parent
-- environment.
aptFile :: String -> [D.BinPkgName] -- Maybe would probably be more correct
aptFile pkg =
    unsafePerformIO $
    do ret <- readProcessWithExitCode "apt-file" ["-l", "search", pkg ++ ".pc"] ""
       return $ case ret of
                  (ExitSuccess, out, _) ->
                      case takeWhile (not . isSpace) out of
                        "" -> error $ "Unable to locate a package containing " ++ pkg ++ ", try using --exec-map " ++ pkg ++ "=<debname> or Map.insert " ++ show pkg ++ " (BinPkgName \"<debname>\") (execMap flags)"
                        s -> [D.BinPkgName s]
                  _ -> []

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: DependencyHints -> Compiler -> Dependency_ -> D.Relations
docDependencies hints compiler (BuildDepends (Dependency name ranges)) =
    dependencies hints compiler Documentation name ranges
docDependencies _ _ _ = []

-- | Function that applies the mapping from cabal names to debian
-- names based on version numbers.  If a version split happens at v,
-- this will return the ltName if < v, and the geName if the relation
-- is >= v.
debianName :: PkgName name => DependencyHints -> PackageType -> PackageIdentifier -> name
debianName hints typ pkgDesc =
    case filter (\ x -> pname == packageName x) (versionSplits hints $ typ) of
      [] -> def
      [splits] ->
          foldTriples' (\ ltName v geName debName ->
                           if pname /= packageName splits
                           then debName
                           else let split = parseDebianVersion (showVersion v) in
                                case version of
                                  Nothing -> geName
                                  Just (D.SLT v') | v' <= split -> ltName
                                  -- Otherwise use ltName only when the split is below v'
                                  Just (D.EEQ v') | v' < split -> ltName
                                  Just (D.LTE v') | v' < split -> ltName
                                  Just (D.GRE v') | v' < split -> ltName
                                  Just (D.SGR v') | v' < split -> ltName
                                  _ -> geName)
                       def
                       splits
      _ -> error $ "Multiple splits for cabal package " ++ string
    where
      foldTriples' :: (PkgName name) => (name -> Version -> name -> name -> name) -> name -> VersionSplits name -> name
      foldTriples' = foldTriples
      def = mkPkgName pname typ
      pname@(PackageName string) = pkgName pkgDesc
      version = (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion pkgDesc)))))

data PackageInfo = PackageInfo { libDir :: FilePath
                               , cabalName :: String
                               , cabalVersion :: String
                               , devDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , profDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , docDeb :: Maybe (D.BinPkgName, DebianVersion) }

-- Make a list of the debian devel packages corresponding to cabal packages
-- which are build dependencies
debDeps :: DebType -> DependencyHints -> Map.Map String PackageInfo -> PackageDescription -> Control' String -> D.Relations
debDeps debType hints cabalPackages pkgDesc control =
    case debType of
      Dev ->
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (devDeb p)
                            Nothing -> Nothing) (cabalDependencies hints pkgDesc))
      Prof ->
          maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) (debNameFromType control Dev) ++
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (profDeb p)
                            Nothing -> Nothing) (cabalDependencies hints pkgDesc))
      Doc ->
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                              case Map.lookup name cabalPackages :: Maybe PackageInfo of
                                Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (docDeb p)
                                Nothing -> Nothing) (cabalDependencies hints pkgDesc))

-- | Given a control file and a DebType, look for the binary deb with
-- the corresponding suffix and return its name.
debNameFromType :: Control' String -> DebType -> Maybe D.BinPkgName
debNameFromType control debType =
    case debType of
      Dev -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap D.BinPkgName $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: DependencyHints -> Compiler -> PackageDescription -> D.Relations
debianBuildDeps hints compiler pkgDesc =
          nub $
          [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
          (map anyrel' (buildDeps hints)) ++
          (if debLibProf hints then [anyrel "ghc-prof"] else []) ++
          (concat $ map (buildDependencies hints compiler)
                  $ filter (not . selfDependency (Cabal.package pkgDesc))
                  $ allBuildDepends hints (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc) (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))

debianBuildDepsIndep :: DependencyHints -> Compiler -> PackageDescription -> D.Relations
debianBuildDepsIndep hints compiler pkgDesc =
          nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies hints compiler)
                      $ filter (not . selfDependency (Cabal.package pkgDesc))
                      $ allBuildDepends hints (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc) (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))
