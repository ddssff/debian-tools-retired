{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.SubstVars2
    ( cabalDependencies      -- Used in Debian.Debianize.SubstVars
    , debNameFromType        -- Used in Debian.Debianize.SubstVars
    , filterMissing          -- Used in Debian.Debianize.SubstVars
    , debDeps                -- Used in Debian.Debianize.SubstVars
    ) where

import Data.Lens.Lazy (getL)
import Data.List as List (isSuffixOf, map, nub)
import Data.Map as Map (lookup)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set (member, toList)
import Debian.Control (Control'(unControl), ControlFunctions(lookupP, stripWS), Field'(Field))
import qualified Debian.Debianize.Internal.Lenses as Lenses (extraLibMap, missingDependencies, packageDescription, packageInfo)
import Debian.Debianize.Monad (Atoms)
import Debian.Debianize.Types (DebType(..), PackageInfo(devDeb, profDeb, docDeb))
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), Relation, Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(Rel), Relations, VersionReq(GRE))
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription as Cabal (allBuildInfo, buildTools, extraLibs, PackageDescription(..), pkgconfigDepends)
import Prelude hiding (unlines)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- Make a list of the debian devel packages corresponding to cabal packages
-- which are build dependencies
debDeps :: DebType -> Atoms -> Control' String -> D.Relations
debDeps debType atoms control =
    interdependencies ++ otherdependencies
    where
      interdependencies =
          case debType of
            Prof -> maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) (debNameFromType control Dev)
            _ -> []
      otherdependencies =
          catMaybes (map (\ (Dependency name _) ->
                          case Map.lookup name (getL Lenses.packageInfo atoms) of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (case debType of
                                                                                                             Dev -> devDeb p
                                                                                                             Prof -> profDeb p
                                                                                                             Doc -> docDeb p)
                            Nothing -> Nothing) (cabalDependencies atoms))

cabalDependencies :: Atoms -> [Dependency]
cabalDependencies atoms =
    catMaybes $ map unboxDependency $ allBuildDepends atoms
                  (Cabal.buildDepends (fromMaybe (error "cabalDependencies") $ getL Lenses.packageDescription atoms))
                  (concatMap buildTools . allBuildInfo . fromMaybe (error "cabalDependencies") $ getL Lenses.packageDescription atoms)
                  (concatMap pkgconfigDepends . allBuildInfo . fromMaybe (error "cabalDependencies") $ getL Lenses.packageDescription atoms)
                  (concatMap extraLibs . allBuildInfo . fromMaybe (error "cabalDependencies") $ getL Lenses.packageDescription atoms)

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

-- | Given a control file and a DebType, look for the binary deb with
-- the corresponding suffix and return its name.
debNameFromType :: Control' String -> DebType -> Maybe BinPkgName
debNameFromType control debType =
    case debType of
      Dev -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))

filterMissing :: Atoms -> [[Relation]] -> [[Relation]]
filterMissing atoms rels =
    filter (/= []) (List.map (filter (\ (D.Rel name _ _) -> not (Set.member name (getL Lenses.missingDependencies atoms)))) rels)
