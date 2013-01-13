{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Dependencies
    ( cabalDependencies -- Debian.Cabal.SubstVars
    , selfDependency -- Debian.Debianize.Combinators
    , allBuildDepends
    , debDeps
    , debianBuildDeps
    , debianBuildDepsIndep
    ) where

import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Debian.Control
import Debian.Debianize.Types.Atoms (noProfilingLibrary, noDocumentationLibrary, packageDescription, compiler)
import Debian.Debianize.Types.Debianization as Debian (Debianization)
import Debian.Debianize.Types.Dependencies (DependencyHints(..), PackageInfo(..), devDeb, debNameFromType, dependencies)
import Debian.Debianize.Types.PackageType (DebType(Dev, Prof, Doc), PackageType(..))
import qualified Debian.Relation as D
import Debian.Version (parseDebianVersion)
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription as Cabal (PackageDescription(..), allBuildInfo, buildTools, pkgconfigDepends, extraLibs)
import Distribution.Simple.Compiler (Compiler)
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

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: DependencyHints -> Compiler -> Debianization -> D.Relations
debianBuildDeps hints compiler deb =
    nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
            (map anyrel' (buildDeps hints)) ++
            (if noProfilingLibrary deb then [] else [anyrel "ghc-prof"]) ++
            (concat $ map (buildDependencies hints compiler)
                    $ filter (not . selfDependency (Cabal.package pkgDesc))
                    $ allBuildDepends
                          hints (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc))
    where
      pkgDesc = packageDescription (error "debianBuildDeps: no PackageDescription") deb

debianBuildDepsIndep :: DependencyHints -> Debianization -> D.Relations
debianBuildDepsIndep hints deb =
    if noDocumentationLibrary deb
    then []
    else nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies hints (compiler (error "debianBuildDeps: no PackageDescription") deb))
                      $ filter (not . selfDependency (Cabal.package pkgDesc))
                      $ allBuildDepends
                            hints (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                            (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))
    where
      pkgDesc = packageDescription (error "debianBuildDeps: no PackageDescription") deb

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: DependencyHints -> Compiler -> Dependency_ -> D.Relations
docDependencies hints compiler (BuildDepends (Dependency name ranges)) =
    dependencies hints compiler Documentation name ranges
docDependencies _ _ _ = []

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

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]
