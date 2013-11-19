{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Finalize2
    ( debianBuildDeps        -- Used in Debian.Debianize.Finalize
    , debianBuildDepsIndep   -- Used in Debian.Debianize.Finalize
    , binaryPackageDeps      -- Used in Debian.Debianize.Finalize
    , binaryPackageConflicts -- Used in Debian.Debianize.Finalize
    , binaryPackageProvides  -- Used in Debian.Debianize.Finalize
    , binaryPackageReplaces  -- Used in Debian.Debianize.Finalize
    ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.Lens.Lazy (getL)
import Data.List as List (map, minimumBy, nub)
import Data.Map as Map (lookup, Map)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set (member, toList)
import Data.Version (showVersion)
import Debian.Debianize.Bundled (ghcBuiltIn)
import Debian.Debianize.ControlFile as Debian (PackageType(..))
import Debian.Debianize.Files2 (mkPkgName, mkPkgName')
import qualified Debian.Debianize.Internal.Lenses as Lenses (buildDeps, buildDepsIndep, compiler, conflicts, debianNameMap, depends, epochMap, execMap, extraLibMap, missingDependencies, noDocumentationLibrary, noProfilingLibrary, packageDescription, provides, replaces)
import Debian.Debianize.Monad (Atoms)
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.Orphans ()
import Debian.Relation (BinPkgName, Relation, Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Version (parseDebianVersion)
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (allBuildInfo, buildTools, extraLibs, PackageDescription(..), pkgconfigDepends)
import Distribution.Version (anyVersion, asVersionIntervals, earlierVersion, foldVersionRange', fromVersionIntervals, intersectVersionRanges, isNoVersion, laterVersion, orEarlierVersion, orLaterVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
import Distribution.Version.Invert (invertVersionRange)
import Prelude hiding (unlines)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
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

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Atoms -> [Dependency] -> [Dependency] -> [Dependency] -> [String] -> [Dependency_]
allBuildDepends atoms buildDepends buildTools pkgconfigDepends extraLibs =
    nub $ map BuildDepends buildDepends ++
          map BuildTools buildTools ++
          map PkgConfigDepends pkgconfigDepends ++
          map ExtraLibs (fixDeps extraLibs)
    where
      fixDeps :: [String] -> [Relations]
      fixDeps xs = concatMap (\ cab -> maybe [[[D.Rel (D.BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                             Set.toList
                                             (Map.lookup cab (getL Lenses.extraLibMap atoms))) xs

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: Atoms -> D.Relations
debianBuildDeps deb =
    filterMissing deb $
    nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
            concat (Set.toList (getL Lenses.buildDeps deb)) ++
            (if getL Lenses.noProfilingLibrary deb then [] else [anyrel "ghc-prof"]) ++
            cabalDeps (getL Lenses.packageDescription deb)
    where
      cabalDeps Nothing = []
      cabalDeps (Just pkgDesc) =
          (concat $ map (buildDependencies deb)
                  $ filter (not . selfDependency (Cabal.package pkgDesc))
                  $ allBuildDepends
                          deb (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc))

debianBuildDepsIndep :: Atoms -> D.Relations
debianBuildDepsIndep deb =
    filterMissing deb $
    if getL Lenses.noDocumentationLibrary deb
    then []
    else nub $ [anyrel "ghc-doc"] ++
               concat (Set.toList (getL Lenses.buildDepsIndep deb)) ++
               cabalDeps (getL Lenses.packageDescription deb)
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
docDependencies :: Atoms -> Dependency_ -> D.Relations
docDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Documentation name ranges
docDependencies _ _ = []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: Atoms -> Dependency_ -> D.Relations
buildDependencies atoms (BuildDepends (Dependency name ranges)) =
    dependencies atoms Development name ranges ++
    dependencies atoms Profiling name ranges
buildDependencies atoms dep@(ExtraLibs _) =
    concat (adapt (getL Lenses.execMap atoms) dep)
buildDependencies atoms dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          concat (adapt (getL Lenses.execMap atoms) dep)
      Nothing ->
          []

adapt :: Map.Map String Relations -> Dependency_ -> [Relations]
adapt execMap (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt execMap (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency (PackageName pkg) _)) = [[[D.Rel (D.BinPkgName pkg) Nothing Nothing]]]

-- There are two reasons this may not work, or may work
-- incorrectly: (1) the build environment may be a different
-- distribution than the parent environment (the environment the
-- autobuilder was run from), so the packages in that
-- environment might have different names, and (2) the package
-- we are looking for may not be installed in the parent
-- environment.
aptFile :: String -> [Relations] -- Maybe would probably be more correct
aptFile pkg =
    unsafePerformIO $
    do ret <- readProcessWithExitCode "apt-file" ["-l", "search", pkg ++ ".pc"] ""
       return $ case ret of
                  (ExitSuccess, out, _) ->
                      case takeWhile (not . isSpace) out of
                        "" -> error $ "Unable to locate a debian package containing the build tool " ++ pkg ++
                                      ", try using --exec-map " ++ pkg ++ "=<debname> or execMap " ++ show pkg ++
                                      " [[Rel (BinPkgName \"<debname>\") Nothing Nothing]]"
                        s -> [[[D.Rel (D.BinPkgName s) Nothing Nothing]]]
                  _ -> []

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: Atoms -> PackageType -> PackageName -> VersionRange -> Relations
dependencies atoms typ name cabalRange =
    map doBundled $ convert' (canonical (Or (catMaybes (map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(BinPkgName, VersionRange)]
      alts = case Map.lookup name (getL Lenses.debianNameMap atoms) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName name typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits' -> map (\ (n, r) -> (mkPkgName' n typ, r)) (packageRangesFromVersionSplits splits')

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
      dv v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (getL Lenses.epochMap atoms)) ++ showVersion v)
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

      -- If a package is bundled with the compiler we make the
      -- compiler a substitute for that package.  If we were to
      -- specify the virtual package (e.g. libghc-base-dev) we would
      -- have to make sure not to specify a version number.
      doBundled :: [D.Relation] -> [D.Relation]
      doBundled rels | ghcBuiltIn (fromMaybe (error "dependencies") $ getL Lenses.compiler atoms) name = rels ++ [D.Rel (compilerPackageName typ) Nothing Nothing]
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

filterMissing :: Atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    filter (/= []) (List.map (filter (\ (D.Rel name _ _) -> not (Set.member name (getL Lenses.missingDependencies atoms)))) rels)

binaryPackageDeps :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageDeps b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL Lenses.depends atoms))

binaryPackageConflicts :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageConflicts b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL Lenses.conflicts atoms))

binaryPackageReplaces :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageReplaces b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL Lenses.replaces atoms))

binaryPackageProvides :: BinPkgName -> Atoms -> [[Relation]]
binaryPackageProvides b atoms = maybe [] (map (: []) . Set.toList) (Map.lookup b (getL Lenses.provides atoms))
