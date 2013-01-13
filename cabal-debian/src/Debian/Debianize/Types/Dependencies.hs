{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.Types.Dependencies
    ( DependencyHints(..)
    , defaultDependencyHints
    , VersionSplits(..)
    , filterMissing' -- Debian.Cabal.SubstVars
    , dependencies
    , debianName
    , PackageInfo(..)
    , debNameFromType
    ) where

import Data.Function (on)
import Data.List (minimumBy, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Version (Version, showVersion)
import Debian.Cabal.Bundled (ghcBuiltIn)
import Debian.Control
import Debian.Debianize.Interspersed (Interspersed(foldInverted), foldTriples)
import Debian.Debianize.Types.PackageType (DebType(Dev, Prof, Doc), PackageType(..), mkPkgName, VersionSplits(..), knownVersionSplits)
import Debian.Orphans ()
import Debian.Relation (Relations, Relation, BinPkgName, PkgName)
import qualified Debian.Relation as D
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..))
import Distribution.Simple.Compiler (Compiler)
import Distribution.Version (VersionRange, anyVersion, foldVersionRange', intersectVersionRanges, unionVersionRanges,
                             laterVersion, orLaterVersion, earlierVersion, orEarlierVersion, fromVersionIntervals, toVersionIntervals, withinVersion,
                             isNoVersion, asVersionIntervals)
import Distribution.Version.Invert (invertVersionRange)

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
{-
      , debLibProf :: Bool
      -- ^ Should we generate? profiling libraries.  At certain times it
      -- was necessary to turn this off to work around compiler bugs.
      , haddock :: Bool
      -- ^ Should we generate the library documentation package?  Also
      -- used at times to avoid haddock bugs or, as the case may be,
      -- bugs in the haddock markup of some packages.
-}

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
      , versionSplits :: [VersionSplits]
      -- ^ Instances where the debian package name is different (for
      -- some range of version numbers) from the default constructed
      -- by mkPkgName.
      } deriving (Eq, Ord, Show)

defaultDependencyHints :: DependencyHints
defaultDependencyHints =
    DependencyHints
    { missingDependencies = []
{-
    , debLibProf = True
    , haddock = True
-}
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
      alts = case Map.lookup name (packageSplits (versionSplits hints)) of
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

filterMissing' :: DependencyHints -> [[D.Relation]] -> [[D.Relation]]
filterMissing' hints rels =
    filter (/= []) (map (filter (\ (D.Rel name _ _) -> not (elem name (missingDependencies hints)))) rels)

-- | Function that applies the mapping from cabal names to debian
-- names based on version numbers.  If a version split happens at v,
-- this will return the ltName if < v, and the geName if the relation
-- is >= v.
debianName :: PkgName name => DependencyHints -> PackageType -> PackageIdentifier -> name
debianName hints typ pkgDesc =
    (\ pname -> mkPkgName pname typ) $
    case filter (\ x -> pname == packageName x) (versionSplits hints) of
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

data PackageInfo = PackageInfo { libDir :: FilePath
                               , cabalName :: String
                               , cabalVersion :: String
                               , devDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , profDeb :: Maybe (D.BinPkgName, DebianVersion)
                               , docDeb :: Maybe (D.BinPkgName, DebianVersion) }

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
