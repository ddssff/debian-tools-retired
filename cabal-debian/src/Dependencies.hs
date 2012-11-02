{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Dependencies
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
import Distribution.Package (PackageName(PackageName))
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Version (Version(..), VersionRange(..), anyVersion, foldVersionRange', intersectVersionRanges, unionVersionRanges,
                             laterVersion, orLaterVersion, earlierVersion, orEarlierVersion, fromVersionIntervals, toVersionIntervals, withinVersion,
                             isNoVersion, asVersionIntervals)
import InvertVersionRange (invertVersionRange)
import Text.PrettyPrint (text, hcat , (<>), empty)
import Text.PrettyPrint.Class (Pretty(pretty))

import Bundled (ghcBuiltIn)
import Interspersed (Interspersed(..))

data PackageType = Source | Development | Profiling | Documentation | Utilities | Extra deriving (Eq, Show)

data VersionSplits
    = VersionSplits {
        packageName :: PackageName
      , oldestPackage :: BinPkgName
      , splits :: [(Version, BinPkgName)] -- Assumed to be in version number order
      }

instance Interspersed VersionSplits BinPkgName Version where
    leftmost (VersionSplits {splits = []}) = error "Empty Interspersed instance"
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: Map.Map PackageName Int -> Compiler -> (PackageType -> [VersionSplits]) -> PackageType -> Either BinPkgName PackageName -> VersionRange -> Relations
dependencies _ _ _ _ (Left name) _ = [[D.Rel name Nothing Nothing]]
dependencies epochMap compiler versionSplits typ (Right name@(PackageName string)) cabalRange =
    map doBundled $ convert' (canonical (Or (catMaybes (map convert alts))))
    where

      -- Compute a list of alternative debian dependencies for
      -- satisfying a cabal dependency.  The only caveat is that
      -- we may need to distribute any "and" dependencies implied
      -- by a version range over these "or" dependences.
      alts :: [(BinPkgName, VersionRange)]
      alts = case Map.lookup name (packageSplits versionSplits typ) of
               -- If there are no splits for this package just return the single dependency for the package
               Nothing -> [(mkPkgName string typ, cabalRange')]
               -- If there are splits create a list of (debian package name, VersionRange) pairs
               Just splits -> packageRangesFromVersionSplits splits

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

packageRangesFromVersionSplits :: VersionSplits -> [(BinPkgName, VersionRange)]
packageRangesFromVersionSplits splits =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 splits

-- | Build a debian package name from a cabal package name and a
-- debian package type.
mkPkgName :: String -> PackageType -> BinPkgName
mkPkgName base typ =
    BinPkgName . PkgName $ prefix typ ++ map toLower base ++ suffix typ
    where
      suffix Source = ""
      suffix Documentation = "-doc"
      suffix Development = "-dev"
      suffix Profiling = "-prof"
      suffix Utilities = "-utils"
      suffix Extra = ""

      prefix Source = "haskell-"
      prefix Documentation = "libghc-"
      prefix Development = "libghc-"
      prefix Profiling = "libghc-"
      prefix Utilities = "haskell-"
      prefix Extra = ""

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

instance Pretty D.BinPkgName where
    pretty (D.BinPkgName p) = text "deb:" <> (pretty p)

instance Pretty D.PkgName where
    pretty (D.PkgName p) = text p

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
debianSourcePackageName versionSplits name version = SrcPkgName (D.unBinPkgName (debianName Source versionSplits name version))

debianDevPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianDevPackageName versionSplits name version = debianName Development versionSplits name version

debianProfPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianProfPackageName versionSplits name version = debianName Profiling versionSplits name version

debianDocPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianDocPackageName versionSplits name version = debianName Documentation versionSplits name version

type DebianBinPackageName = PackageName -> Maybe VersionReq -> BinPkgName

debianExtraPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianExtraPackageName versionSplits name version = debianName Extra versionSplits name version

debianUtilsPackageName :: (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
debianUtilsPackageName versionSplits name version = debianName Utilities versionSplits name version

-- | Return the basename of the debian package for a given version
-- relation.  If the version split happens at v, this will return the
-- ltName is < v and the geName if the relation is >= v.  It also handles
-- a special case for the name of the haskell-src-exts package.
debianName :: PackageType -> (PackageType -> [VersionSplits]) -> PackageName -> Maybe VersionReq -> BinPkgName
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
      foldTriples' :: (BinPkgName -> Version -> BinPkgName -> BinPkgName -> BinPkgName) -> BinPkgName -> VersionSplits -> BinPkgName
      foldTriples' = foldTriples
      def = mkPkgName (map fixChar name) typ

fixChar :: Char -> Char
fixChar '_' = '-'
fixChar c = toLower c
