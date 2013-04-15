{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Debian.Debianize.Types.VersionSplits
    ( VersionSplits
    , packageRangesFromVersionSplits
    , makePackage
    , insertSplit
    , doSplits
    , knownVersionSplits
    ) where

import Data.Version (Version(Version), showVersion)
import Debian.Debianize.Interspersed (Interspersed(leftmost, pairs, foldInverted), foldTriples)
import Data.Map as Map (Map, fromList)
import Debian.Orphans ()
import qualified Debian.Relation as D
import Debian.Version (parseDebianVersion)
import Distribution.Package (PackageName(PackageName))
import Distribution.Version (VersionRange, anyVersion, intersectVersionRanges, earlierVersion, orLaterVersion)
import Prelude hiding (init, unlines, log)

-- | Describes a mapping from cabal package name and version to debian
-- package names.  For example, versions of the cabal QuickCheck
-- package less than 2 are mapped to "quickcheck1", while version 2 or
-- greater is mapped to "quickcheck2".
data VersionSplits
    = VersionSplits {
        oldestPackage :: String
      -- ^ The name given to versions older than the oldest split.
      , splits :: [(Version, String)]
      -- ^ Each pair is The version where the split occurs, and the
      -- name to use for versions greater than or equal to that
      -- version.  This list assumed to be in (must be kept in)
      -- ascending version number order.
      } deriving (Eq, Ord, Show)

makePackage :: String -> VersionSplits
makePackage name = VersionSplits {oldestPackage = name, splits = []}

-- | Split the version range and give the older packages a new name.
insertSplit :: Version -> String -> VersionSplits -> VersionSplits
insertSplit ver@(Version _ _) ltname sp@(VersionSplits {}) =
    -- (\ x -> trace ("insertSplit " ++ show (ltname, ver, sp) ++ " -> " ++ show x) x) $
    case splits sp of
      -- This is the oldest split, change oldestPackage and insert a new head pair
      (ver', _) : _ | ver' > ver -> sp {oldestPackage = ltname, splits = (ver, oldestPackage sp) : splits sp}
      [] -> sp {oldestPackage = ltname, splits = [(ver, oldestPackage sp)]}
      -- Not the oldest split, insert it in its proper place.
      _ -> sp {splits = reverse (insert (reverse (splits sp)))}
    where
      -- Insert our new split into the reversed list
      insert ((ver', name') : more) =
          if ver' < ver
          then (ver, name') : (ver', ltname) : more
          else (ver', name') : insert more
      -- ver' is older, change oldestPackage
      insert [] = [(ver, oldestPackage sp)]
      -- ltname = base ++ "-" ++ (show (last ns - 1))

instance Interspersed VersionSplits String Version where
    leftmost (VersionSplits {splits = []}) = error "Empty Interspersed instance"
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

packageRangesFromVersionSplits :: VersionSplits -> [(String, VersionRange)]
packageRangesFromVersionSplits s =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 s

doSplits :: VersionSplits -> Maybe D.VersionReq -> String
doSplits s version =
    foldTriples' (\ ltName v geName _ ->
                           let split = parseDebianVersion (showVersion v) in
                                case version of
                                  Nothing -> geName
                                  Just (D.SLT v') | v' <= split -> ltName
                                  -- Otherwise use ltName only when the split is below v'
                                  Just (D.EEQ v') | v' < split -> ltName
                                  Just (D.LTE v') | v' < split -> ltName
                                  Just (D.GRE v') | v' < split -> ltName
                                  Just (D.SGR v') | v' < split -> ltName
                                  _ -> geName)
                 (oldestPackage s)
                 s
    where
      foldTriples' :: (String -> Version -> String -> String -> String) -> String -> VersionSplits -> String
      foldTriples' = foldTriples

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: Map PackageName VersionSplits
knownVersionSplits =
    Map.fromList
    [ (PackageName "parsec", VersionSplits {oldestPackage = "parsec2", splits = [(Version [3] [], "parsec3")]})
    , (PackageName "QuickCheck", VersionSplits {oldestPackage = "quickcheck1", splits = [(Version [2] [], "quickcheck2")]})
    -- This just gives a special case cabal to debian name mapping.
    , (PackageName "gtk2hs-buildtools", VersionSplits {oldestPackage = "gtk2hs-buildtools", splits = []}) ]
