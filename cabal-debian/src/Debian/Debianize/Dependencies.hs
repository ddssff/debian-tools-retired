{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Dependencies
    ( cabalDependencies -- Debian.Cabal.SubstVars
    , selfDependency -- Debian.Debianize.Combinators
    , allBuildDepends
    , debDeps
    , debianBuildDeps
    , debianBuildDepsIndep
    , dependencies
    , debianName
    ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (nub, minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Version (Version, showVersion)
import Debian.Control
import Debian.Debianize.AtomsType (HasAtoms, PackageInfo(devDeb, profDeb, docDeb),
                                   noProfilingLibrary, noDocumentationLibrary, packageDescription, compiler, versionSplits,
                                   filterMissing, extraLibMap, buildDeps, buildDepsIndep, execMap, epochMap, packageInfo)
import Debian.Debianize.Bundled (ghcBuiltIn)
import Debian.Debianize.Interspersed (Interspersed(foldInverted), foldTriples)
import Debian.Debianize.Types.Debianization as Debian (Debianization)
import Debian.Debianize.Types.Dependencies (debNameFromType)
import Debian.Debianize.Types.PackageType (DebType(Dev, Prof, Doc), PackageType(..), mkPkgName, VersionSplits(..))
import qualified Debian.Relation as D
import Debian.Relation (Relations, Relation, BinPkgName, PkgName)
import Debian.Version (parseDebianVersion)
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription as Cabal (PackageDescription(..), allBuildInfo, buildTools, pkgconfigDepends, extraLibs)
import Distribution.Version (VersionRange, anyVersion, foldVersionRange', intersectVersionRanges, unionVersionRanges,
                             laterVersion, orLaterVersion, earlierVersion, orEarlierVersion, fromVersionIntervals, toVersionIntervals, withinVersion,
                             isNoVersion, asVersionIntervals)
import Distribution.Version.Invert (invertVersionRange)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

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

-- Make a list of the debian devel packages corresponding to cabal packages
-- which are build dependencies
debDeps :: HasAtoms atoms => DebType -> atoms -> Control' String -> D.Relations
debDeps debType atoms control =
    interdependencies ++ otherdependencies
    where
      interdependencies =
          case debType of
            Prof -> maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) (debNameFromType control Dev)
            _ -> []
      otherdependencies =
          catMaybes (map (\ (Dependency name _) ->
                          case packageInfo name atoms of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (case debType of
                                                                                                             Dev -> devDeb p
                                                                                                             Prof -> profDeb p
                                                                                                             Doc -> docDeb p)
                            Nothing -> Nothing) (cabalDependencies atoms))

cabalDependencies :: HasAtoms atoms => atoms -> [Dependency]
cabalDependencies atoms =
    catMaybes $ map unboxDependency $ allBuildDepends atoms
                  (Cabal.buildDepends (fromMaybe (error "cabalDependencies") $ packageDescription atoms))
                  (concatMap buildTools . allBuildInfo . fromMaybe (error "cabalDependencies") $ packageDescription atoms)
                  (concatMap pkgconfigDepends . allBuildInfo . fromMaybe (error "cabalDependencies") $ packageDescription atoms)
                  (concatMap extraLibs . allBuildInfo . fromMaybe (error "cabalDependencies") $ packageDescription atoms)

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: HasAtoms atoms => atoms -> [Dependency] -> [Dependency] -> [Dependency] -> [String] -> [Dependency_]
allBuildDepends atoms buildDepends buildTools pkgconfigDepends extraLibs =
    nub $ map BuildDepends buildDepends ++
          map BuildTools buildTools ++
          map PkgConfigDepends pkgconfigDepends ++
          map ExtraLibs (fixDeps extraLibs)
    where
      fixDeps :: [String] -> [D.BinPkgName]
      fixDeps xs = concatMap (\ cab -> maybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] Set.toList (Map.lookup cab (extraLibMap atoms))) xs

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: HasAtoms atoms => atoms -> D.Relations
debianBuildDeps deb =
    filterMissing deb $
    nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
            (map anyrel' (Set.toList (buildDeps deb))) ++
            (if noProfilingLibrary deb then [] else [anyrel "ghc-prof"]) ++
            cabalDeps (packageDescription deb)
    where
      cabalDeps Nothing = []
      cabalDeps (Just pkgDesc) =
          (concat $ map (buildDependencies deb)
                  $ filter (not . selfDependency (Cabal.package pkgDesc))
                  $ allBuildDepends
                          deb (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc))

debianBuildDepsIndep :: Debianization -> D.Relations
debianBuildDepsIndep deb =
    filterMissing deb $
    if noDocumentationLibrary deb
    then []
    else nub $ [anyrel "ghc-doc"] ++
               (map anyrel' (Set.toList (buildDepsIndep deb))) ++
               cabalDeps (packageDescription deb)
    where
      cabalDeps Nothing = []
      cabalDeps (Just pkgDesc) =
          (concat . map (docDependencies deb)
                      $ filter (not . selfDependency (Cabal.package pkgDesc))
                      $ allBuildDepends
                            deb (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                            (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: HasAtoms atoms => atoms -> Dependency_ -> D.Relations
docDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Documentation name ranges
docDependencies _ _ = []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: HasAtoms atoms => atoms -> Dependency_ -> D.Relations
buildDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Development name ranges ++
    dependencies atoms Profiling name ranges
buildDependencies atoms dep@(ExtraLibs _) =
    concat (map dependency $ adapt (execMap atoms) dep)
buildDependencies atoms dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          concat (map dependency $ adapt (execMap atoms) dep)
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

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: HasAtoms atoms => atoms -> PackageType -> PackageName -> VersionRange -> Relations
dependencies atoms typ name cabalRange =
    map doBundled $ convert' (canonical (Or (catMaybes (map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(BinPkgName, VersionRange)]
      alts = case Map.lookup name (packageSplits (versionSplits atoms)) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName name typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits' -> map (\ (n, r) -> (mkPkgName n typ, r)) (packageRangesFromVersionSplits splits')

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
      dv v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (epochMap atoms)) ++ showVersion v)
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

      -- If a package is bundled with the compiler we make the
      -- compiler a substitute for that package.  If we were to
      -- specify the virtual package (e.g. libghc-base-dev) we would
      -- have to make sure not to specify a version number.
      doBundled :: [D.Relation] -> [D.Relation]
      doBundled rels | ghcBuiltIn (compiler (error "dependencies") atoms) name = rels ++ [D.Rel (compilerPackageName typ) Nothing Nothing]
      doBundled rels = rels

      compilerPackageName Documentation = D.BinPkgName "ghc-doc"
      compilerPackageName Profiling = D.BinPkgName "ghc-prof"
      compilerPackageName Development = D.BinPkgName "ghc"
      compilerPackageName _ = D.BinPkgName "ghc" -- whatevs

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = map (map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel rel) = And [Or [Rel rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . map Or $ sequence $ map (concat . map unOr . unAnd . canonical) $ rels

packageSplits :: [VersionSplits] -> Map.Map PackageName VersionSplits
packageSplits splits =
    foldr (\ splits' mp -> Map.insertWith multipleSplitsError (packageName splits') splits' mp)
          Map.empty
          splits
    where
      multipleSplitsError :: VersionSplits -> a -> b
      multipleSplitsError (VersionSplits {packageName = PackageName p}) _s2 =
          error ("Multiple splits for package " ++ show p)

packageRangesFromVersionSplits :: VersionSplits -> [(PackageName, VersionRange)]
packageRangesFromVersionSplits splits =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 splits

-- | Function that applies the mapping from cabal names to debian
-- names based on version numbers.  If a version split happens at v,
-- this will return the ltName if < v, and the geName if the relation
-- is >= v.
debianName :: (HasAtoms atoms, PkgName name) => atoms -> PackageType -> PackageIdentifier -> name
debianName atoms typ pkgDesc =
    (\ pname -> mkPkgName pname typ) $
    case filter (\ x -> pname == packageName x) (versionSplits atoms) of
      [] -> pname
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
                       pname
                       splits
      _ -> error $ "Multiple splits for cabal package " ++ string
    where
      foldTriples' :: (PackageName -> Version -> PackageName -> PackageName -> PackageName) -> PackageName -> VersionSplits -> PackageName
      foldTriples' = foldTriples
      -- def = mkPkgName pname typ
      pname@(PackageName string) = pkgName pkgDesc
      version = (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion pkgDesc)))))
