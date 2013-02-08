-- | Combinator functions for the Debianization type.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Combinators
    ( addExtraLibDependencies
    , defaultAtoms
    ) where

import Data.Lens.Lazy (getL, setL, modL)
import Data.List as List (nub, intercalate)
import Data.Map as Map (lookup)
import Data.Maybe
import Data.Monoid (mempty, (<>))
import Data.Set as Set (toList)
import Data.Text as Text (Text, pack)
import Debian.Debianize.Atoms as Atoms (Atoms, packageDescription, control, versionSplits, extraLibMap, epochMap)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), PackageType(..))
import Debian.Debianize.Dependencies (debianName)
import Debian.Debianize.Types (knownVersionSplits, knownEpochMappings)
import Debian.Debianize.Utility (trim)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, init, unlines, log)

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: Atoms -> Atoms
addExtraLibDependencies deb =
    modL control (\ y -> y {binaryPackages = map f (binaryPackages (getL control deb))}) deb
    where
      f :: BinaryDebDescription -> BinaryDebDescription
      f bin
          | debianName deb Development (Cabal.package pkgDesc) == Debian.package bin
              = bin { relations = g (relations bin) }
      f bin = bin
      g :: Debian.PackageRelations -> Debian.PackageRelations
      g rels = rels { Debian.depends = Debian.depends rels ++
                                map anyrel' (concatMap (\ cab -> maybe [BinPkgName ("lib" ++ cab ++ "-dev")] Set.toList (Map.lookup cab (getL extraLibMap deb)))
                                                       (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)) }
      pkgDesc = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ getL packageDescription deb

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]

{-
oldFilterMissing :: [BinPkgName] -> Atoms -> Atoms
oldFilterMissing missing deb =
    modL control e deb
    where
      e src = src { Debian.buildDepends = f (Debian.buildDepends src)
                  , Debian.buildDependsIndep = f (Debian.buildDependsIndep src)
                  , binaryPackages = map g (binaryPackages src) }
      f rels = filter (/= []) (map (filter (\ (Rel name _ _) -> not (elem name missing))) rels)
      g bin = bin { relations = h (relations bin) }
      h rels = PackageRelations { Debian.depends = f (Debian.depends rels)
                                , recommends = f (recommends rels)
                                , suggests = f (suggests rels)
                                , preDepends = f (preDepends rels)
                                , breaks = f (breaks rels)
                                , conflicts = f (conflicts rels)
                                , provides = f (provides rels)
                                , replaces = f (replaces rels)
                                , builtUsing = f (builtUsing rels) }
-}

defaultAtoms :: Atoms
defaultAtoms =
    setL epochMap knownEpochMappings $
    setL versionSplits knownVersionSplits $
    mempty
