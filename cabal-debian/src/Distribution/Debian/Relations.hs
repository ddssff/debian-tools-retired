{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, PackageImports, ScopedTypeVariables,
             StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Support for generating Debianization from Cabal data.

module Distribution.Debian.Relations
    ( allBuildDepends
    , buildDependencies
    , docDependencies
    , cabalDependencies
    , versionSplits
    ) where

import Data.Char (isSpace)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Version (Version(Version))
import qualified Debian.Relation as D
import Distribution.Debian.Dependencies (PackageType(..), VersionSplits(..), dependencies, mkPkgName)
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription (PackageDescription(..), allBuildInfo, buildTools, pkgconfigDepends, extraLibs)
import Distribution.Version (anyVersion)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

cabalDependencies :: Map.Map String [D.BinPkgName] -> PackageDescription -> [Dependency]
cabalDependencies depMap pkgDesc = catMaybes $ map unboxDependency $ allBuildDepends depMap pkgDesc

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Map.Map String [D.BinPkgName] -> PackageDescription -> [Dependency_]
allBuildDepends depMap pkgDesc =
    nub $ map BuildDepends (buildDepends pkgDesc) ++
          concat (map (map BuildTools . buildTools) (allBuildInfo pkgDesc) ++
                  map
                    (map PkgConfigDepends . pkgconfigDepends)
                    (allBuildInfo pkgDesc) ++
                  map (map ExtraLibs . (fixDeps . extraLibs)) (allBuildInfo pkgDesc))
    where
      fixDeps :: [String] -> [D.BinPkgName]
      fixDeps xs = concatMap (\ cab -> fromMaybe [D.BinPkgName (D.PkgName ("lib" ++ cab ++ "-dev"))] (Map.lookup cab depMap)) xs

-- The build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.
buildDependencies :: Map.Map PackageName Int -> Map.Map String D.BinPkgName -> Compiler -> Dependency_ -> D.Relations
buildDependencies epochMap _ compiler (BuildDepends (Dependency name ranges)) =
    dependencies epochMap compiler versionSplits Development (Right name) ranges ++
    dependencies epochMap compiler versionSplits Profiling (Right name) ranges
buildDependencies epochMap execMap compiler dep@(ExtraLibs _) =
    concat (map (\ x -> dependencies epochMap compiler versionSplits Extra (Left x) anyVersion) $ adapt execMap dep)
buildDependencies epochMap execMap compiler dep =
    case unboxDependency dep of
      Just (Dependency _name ranges) ->
          concat (map (\ x -> dependencies epochMap compiler versionSplits Extra (Left x) ranges) $ adapt execMap dep)
      Nothing ->
          []

adapt :: Map.Map String D.BinPkgName -> Dependency_ -> [D.BinPkgName]
adapt execMap (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt execMap (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg execMap)
adapt _flags (ExtraLibs x) = [x]
{-
    maybe (error ("No mapping from library " ++ x ++ " to debian binary package name"))
              (map (\ s -> PackageName ("lib" ++ s ++ "-dev"))) (Map.lookup x (depMap flags))
-}
adapt _flags (BuildDepends (Dependency (PackageName pkg) _)) = [D.BinPkgName (D.PkgName pkg)]

-- |There are two reasons this may not work, or may work
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
                        (ExitSuccess, out, _) -> [D.BinPkgName (D.PkgName (takeWhile (not . isSpace) out))]
                        _ -> []

-- The documentation dependencies for a package include the documentation
-- package for any libraries which are build dependencies, so we have access
-- to all the cross references.
docDependencies :: Map.Map PackageName Int -> Compiler -> Dependency_ -> D.Relations
docDependencies epochMap compiler (BuildDepends (Dependency name ranges)) =
    dependencies epochMap compiler versionSplits Documentation (Right name) ranges
docDependencies _ _ _ = []

data Dependency_
  = BuildDepends Dependency
      | BuildTools Dependency
      | PkgConfigDepends Dependency
      | ExtraLibs D.BinPkgName
    deriving (Eq, Show)

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- | These are the instances of debian names changing that we know about.
versionSplits :: PackageType -> [VersionSplits]
versionSplits typ =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = mkPkgName "parsec2" typ
      , splits = [(Version [3] [], mkPkgName "parsec3" typ)] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = mkPkgName "quickcheck1" typ
      , splits = [(Version [2] [], mkPkgName "quickcheck2" typ)] }
    ]
