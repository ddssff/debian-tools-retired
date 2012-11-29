{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, PackageImports, ScopedTypeVariables,
             StandaloneDeriving, TupleSections, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Support for generating Debianization from Cabal data.

module Distribution.Debian.Relations
    ( selfDependency
    , allBuildDepends
    , buildDependencies
    , docDependencies
    , cabalDependencies
    , extraDebianLibs
    ) where

import Data.Char (isSpace)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Debian.Relation as D
import Distribution.Debian.Dependencies (PackageType(..), dependencies)
import Distribution.Debian.Splits (versionSplits)
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..), Dependency(..))
import Distribution.PackageDescription (PackageDescription(..), allBuildInfo, buildTools, pkgconfigDepends, extraLibs)
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
selfDependency :: PackageDescription -> Dependency_ -> Bool
selfDependency pkgDesc (BuildDepends (Dependency name _)) = name == pkgName (package pkgDesc)
selfDependency _ _ = False

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

cabalDependencies :: Map.Map String [D.BinPkgName] -> PackageDescription -> [Dependency]
cabalDependencies extraLibMap pkgDesc = catMaybes $ map unboxDependency $ allBuildDepends extraLibMap pkgDesc

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Map.Map String [D.BinPkgName] -> PackageDescription -> [Dependency_]
allBuildDepends extraLibMap pkgDesc =
    nub $ map BuildDepends (buildDepends pkgDesc) ++
          concat (map (map BuildTools . buildTools) (allBuildInfo pkgDesc) ++
                  map (map PkgConfigDepends . pkgconfigDepends) (allBuildInfo pkgDesc) ++
                  map (map ExtraLibs . (fixDeps . extraLibs)) (allBuildInfo pkgDesc))
    where
      fixDeps :: [String] -> [D.BinPkgName]
      fixDeps xs = concatMap (\ cab -> fromMaybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] (Map.lookup cab extraLibMap)) xs

extraDebianLibs :: Map.Map String [D.BinPkgName] -> PackageDescription -> D.Relations
extraDebianLibs extraLibMap pkgDesc =
    map anyrel $ fixDeps $ nub $ concatMap extraLibs $ allBuildInfo $ pkgDesc
    where
      fixDeps :: [String] -> [D.BinPkgName]
      fixDeps xs = concatMap (\ cab -> fromMaybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] (Map.lookup cab extraLibMap)) xs

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: Map.Map PackageName Int -> Map.Map String D.BinPkgName -> Compiler -> Dependency_ -> D.Relations
buildDependencies epochMap _ compiler (BuildDepends (Dependency name ranges)) =
    dependencies epochMap compiler versionSplits Development name ranges ++
    dependencies epochMap compiler versionSplits Profiling name ranges
buildDependencies _epochMap execMap _compiler dep@(ExtraLibs _) =
    concat (map dependency $ adapt execMap dep)
buildDependencies _epochMap execMap _compiler dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          concat (map dependency $ adapt execMap dep)
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
                  (ExitSuccess, out, _) -> [D.BinPkgName (takeWhile (not . isSpace) out)]
                  _ -> []

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: Map.Map PackageName Int -> Compiler -> Dependency_ -> D.Relations
docDependencies epochMap compiler (BuildDepends (Dependency name ranges)) =
    dependencies epochMap compiler versionSplits Documentation name ranges
docDependencies _ _ _ = []

anyrel :: D.BinPkgName -> [D.Relation]
anyrel x = [D.Rel x Nothing Nothing]

