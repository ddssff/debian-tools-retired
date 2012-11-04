module Distribution.Debian.PackageInfo
    ( PackageInfo(..)
    , DebType
    , debName
    , debDeps
    ) where

import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, catMaybes)
import Debian.Control
import qualified Debian.Relation as D
import Debian.Version (DebianVersion)
import Distribution.Debian.Relations (cabalDependencies)
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription (PackageDescription(..))

data PackageInfo = PackageInfo { libDir :: FilePath
                               , cabalName :: String
                               , cabalVersion :: String
                               , devDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , profDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , docDeb :: Maybe (D.BinPkgName, DebianVersion) }

data DebType = Dev | Prof | Doc deriving (Eq, Read, Show)

debName :: Control' String -> DebType -> Maybe D.BinPkgName
debName control debType =
    case debType of
      Dev -> fmap (D.BinPkgName . D.PkgName) $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap (D.BinPkgName . D.PkgName) $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap (D.BinPkgName . D.PkgName) $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))

-- Make a list of the debian devel packages corresponding to cabal packages
-- which are build dependencies
debDeps :: DebType -> Map.Map String [D.BinPkgName] -> Map.Map String PackageInfo -> PackageDescription -> Control' String -> D.Relations
debDeps debType extraLibMap cabalPackages pkgDesc control =
    case debType of
      Dev ->
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (devDeb p)
                            Nothing -> Nothing) (cabalDependencies extraLibMap pkgDesc))
      Prof ->
          maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) (debName control Dev) ++
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (profDeb p)
                            Nothing -> Nothing) (cabalDependencies extraLibMap pkgDesc))
      Doc ->
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                              case Map.lookup name cabalPackages :: Maybe PackageInfo of
                                Just p -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (docDeb p)
                                Nothing -> Nothing) (cabalDependencies extraLibMap pkgDesc))
