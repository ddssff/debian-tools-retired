{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Distribution.Debian.Dependencies
    ( PackageType(..)
    , VersionSplits(..)
    , dependencies
    , mkPkgName
    -- , debianName
    , debianSourcePackageName
    , DebianBinPackageName
    , debianDevPackageName
    , debianProfPackageName
    , debianDocPackageName
    , debianExtraPackageName
    , debianUtilsPackageName
    ) where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (intersperse, minimumBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Version (showVersion)
import Debian.Relation (Relations, Relation, BinPkgName(BinPkgName), PkgName(PkgName), VersionReq(..), SrcPkgName(..))
import qualified Debian.Relation as D
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Distribution.Debian.Bundled (ghcBuiltIn)
import Distribution.Debian.DebHelper ({- instances only -})
import Distribution.Debian.Interspersed (Interspersed(..))
import Distribution.Package (PackageName(PackageName))
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Version (Version(..), VersionRange(..), anyVersion, foldVersionRange', intersectVersionRanges, unionVersionRanges,
                             laterVersion, orLaterVersion, earlierVersion, orEarlierVersion, fromVersionIntervals, toVersionIntervals, withinVersion,
                             isNoVersion, asVersionIntervals)
import Distribution.Version.Invert (invertVersionRange)
import Text.PrettyPrint (text, hcat , (<>), empty)
import Text.PrettyPrint.Class (Pretty(pretty))

data PackageType = Source | Development | Profiling | Documentation | Utilities | Extra deriving (Eq, Show)

data VersionSplits
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: PkgName
      , splits :: [(Version, PkgName)] -- Assumed to be in version number order
      }

instance Interspersed VersionSplits PkgName Version where
    leftmost (VersionSplits {splits = []}) = error "Empty Interspersed instance"
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: Map.Map PackageName Int -> Compiler -> (PackageType -> [VersionSplits]) -> PackageType -> PackageName -> VersionRange -> Relations
dependencies epochMap compiler versionSplits typ name@(PackageName string) cabalRange =
    map doBundled $ convert' (canonical (Or (catMaybes (map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(PkgName, VersionRange)]
      alts = case Map.lookup name (packageSplits versionSplits typ) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName string typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits -> packageRangesFromVersionSplits splits

      convert :: (PkgName, VersionRange) -> Maybe (Rels Relation)
      convert (dname, range) =
          let dname' = BinPkgName dname in
          if isNoVersion range'''
          then Nothing
          else Just $
               foldVersionRange'
                 (Rel (D.Rel dname' Nothing Nothing))
                 (\ v -> Rel (D.Rel dname' (Just (D.EEQ (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname' (Just (D.SGR (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname' (Just (D.SLT (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname' (Just (D.GRE (dv v))) Nothing))
                 (\ v -> Rel (D.Rel dname' (Just (D.LTE (dv v))) Nothing))
                 (\ x y -> And [Rel (D.Rel dname' (Just (D.GRE (dv x))) Nothing), Rel (D.Rel dname' (Just (D.SLT (dv y))) Nothing)])
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
      dv v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name epochMap) ++ showVersion v)
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

      compilerPackageName Documentation = D.BinPkgName (D.PkgName "ghc-doc")
      compilerPackageName Profiling = D.BinPkgName (D.PkgName "ghc-prof")
      compilerPackageName Development = D.BinPkgName (D.PkgName "ghc")
      compilerPackageName _ = D.BinPkgName (D.PkgName "ghc") -- whatevs

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel {unRel :: a} deriving Show

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel rel) = And [Or [Rel rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . map Or $ sequence $ map (concat . map unOr . unAnd . canonical) $ rels

convert' :: Rels a -> [[a]]
convert' = map (map unRel . unOr) . unAnd . canonical

packageSplits :: (PackageType -> [VersionSplits]) -> PackageType -> Map.Map PackageName VersionSplits
packageSplits versionSplits typ =
    foldr (\ splits mp -> Map.insertWith multipleSplitsError (packageName splits) splits mp)
          Map.empty
          (versionSplits typ)
    where
      multipleSplitsError (VersionSplits {packageName = PackageName p}) _s2 =
          error ("Multiple splits for package " ++ show p)

packageRangesFromVersionSplits :: VersionSplits -> [(PkgName, VersionRange)]
packageRangesFromVersionSplits splits =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 splits

instance Pretty VersionRange where
    pretty range =
        foldVersionRange'
          (text "*")
          (\ v -> text "=" <> pretty v)
          (\ v -> text ">" <> pretty v)
          (\ v -> text "<" <> pretty v)
          (\ v -> text ">=" <> pretty v)
          (\ v -> text "<=" <> pretty v)
          (\ x _ -> text "=" <> pretty x <> text ".*") -- not exactly right
          (\ x y -> text "(" <> x <> text " || " <> y <> text ")")
          (\ x y -> text "(" <> x <> text " && " <> y <> text ")")
          (\ x -> text "(" <> x <> text ")")
          range

instance Pretty Version where
    pretty = text . showVersion

instance Pretty a => Pretty [a] where
    pretty xs = text "[" <> hcat (intersperse (text ", ") (map pretty xs)) <> text "]"

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = text "(" <> pretty a <> text ", " <> pretty b <> text ")"

instance Pretty D.Relation where
    pretty (D.Rel name ver arch) =
        pretty name <> maybe empty pretty ver <> maybe empty pretty arch

instance Pretty D.VersionReq where
    pretty (D.EEQ v) = text "=" <> pretty v
    pretty (D.SLT v) = text "<" <> pretty v
    pretty (D.LTE v) = text "<=" <> pretty v
    pretty (D.GRE v) = text ">=" <> pretty v
    pretty (D.SGR v) = text ">" <> pretty v

instance Pretty D.ArchitectureReq where
    pretty (D.ArchOnly ss) = text "[" <> hcat (intersperse (text ",") (map text ss)) <> text "]"
    pretty (D.ArchExcept ss) = text "[!" <> hcat (intersperse (text ",") (map text ss)) <> text "]"

instance Pretty DebianVersion where
    pretty = text . show

instance Show D.Relation where
    show = show . pretty
instance Show D.ArchitectureReq where
    show = show . pretty

deriving instance Show VersionReq
instance Show DebianVersion where
    show = show . prettyDebianVersion

debianSourcePackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> SrcPkgName
debianSourcePackageName versionSplits name version = SrcPkgName (debianName Source versionSplits name version)

debianDevPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianDevPackageName versionSplits name version = BinPkgName (debianName Development versionSplits name version)

debianProfPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianProfPackageName versionSplits name version = BinPkgName (debianName Profiling versionSplits name version)

debianDocPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianDocPackageName versionSplits name version = BinPkgName (debianName Documentation versionSplits name version)

type DebianBinPackageName = PackageName -> Maybe VersionReq -> BinPkgName

debianExtraPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianExtraPackageName versionSplits name version = BinPkgName (debianName Extra versionSplits name version)

debianUtilsPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianUtilsPackageName versionSplits name version = BinPkgName (debianName Utilities versionSplits name version)

-- | Return the basename of the debian package for a given version
-- relation.  If the version split happens at v, this will return the
-- ltName if < v and the geName if the relation is >= v.
debianName :: PackageType -> (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> PkgName
debianName typ versionSplits pname@(PackageName name) version =
    case filter (\ x -> pname == packageName x) (versionSplits typ) of
      [] -> def
      [splits] ->
          foldTriples' (\ ltName v geName debName ->
                           if pname /= packageName splits
                           then debName
                           else let split = parseDebianVersion (showVersion v) in
                                case version of
                                  Nothing -> geName
                                  Just (SLT v') | v' <= split -> ltName
                                  -- Otherwise use ltName only when the split is below v'
                                  Just (EEQ v') | v' < split -> ltName
                                  Just (LTE v') | v' < split -> ltName
                                  Just (GRE v') | v' < split -> ltName
                                  Just (SGR v') | v' < split -> ltName
                                  _ -> geName)
                       def
                       splits
      _ -> error $ "Multiple splits for cabal package " ++ name
    where
      foldTriples' :: (PkgName -> Version -> PkgName -> PkgName -> PkgName) -> PkgName -> VersionSplits -> PkgName
      foldTriples' = foldTriples
      def = mkPkgName name typ


-- | Build a debian package name from a cabal package name and a
-- debian package type.
mkPkgName :: String -> PackageType -> PkgName
mkPkgName name typ =
    PkgName $ case typ of
                Source -> "haskell-" ++ base ++ ""
                Documentation -> "libghc-" ++ base ++ "-doc"
                Development -> "libghc-" ++ base ++ "-dev"
                Profiling -> "libghc-" ++ base ++ "-prof"
                Utilities -> "haskell-" ++ base ++ "-utils"
                Extra -> base
    where
      base = map (fixChar . toLower) name
      -- Underscore is prohibited in debian package names.
      fixChar :: Char -> Char
      fixChar '_' = '-'
      fixChar c = toLower c
