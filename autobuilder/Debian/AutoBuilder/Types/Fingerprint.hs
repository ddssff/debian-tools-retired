{-# OPTIONS_GHC -Wall -Werror #-}
module Debian.AutoBuilder.Types.Fingerprint
    ( Fingerprint
    , packageFingerprint
    , showFingerprint
    , dependencyChanges
    , showDependencies
    , targetFingerprint
    , BuildDecision(..)
    , buildDecision
    ) where

import Control.Applicative.Error (maybeRead)
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate, intersperse, find, partition, nub)
import qualified Data.Map as Map
import Data.Maybe(fromJust, isJust, isNothing)
import qualified Data.Set as Set
import Debian.AutoBuilder.Types.Buildable (Target(tgt, cleanSource), Buildable(download), targetRelaxed, targetControl, relaxDepends)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.RetrieveMethodOld as O
import Debian.Changes (logVersion)
import Debian.Control (lookupP, unControl, stripWS)
import qualified Debian.Control.String as S
import qualified Debian.GenBuildDeps as G
import Debian.Relation (Relation(Rel), BinPkgName(..), PkgName(..))
import Debian.Repo.Repository (readPkgVersion, showPkgVersion)
import Debian.Repo.SourceTree (DebianSourceTreeC(entry), SourcePackageStatus(..))
import Debian.Repo.Types (SourcePackage(sourceParagraph, sourcePackageID), PkgVersion(PkgVersion, getName, getVersion), BinaryPackage(packageID), PackageID(packageVersion), binaryPackageName, prettyPkgVersion)
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.VersionPolicy(dropTag, parseTag)
import Extra.Misc(columns)

-- | This type represents a package's fingerprint, (formerly its
-- revision string,) which includes three pieces of information: how
-- it was retrieved, the version number of the resulting Debian source
-- package, and the names and version numbers of the build
-- dependencies against which it was or is about to be built.
data Fingerprint
    = Fingerprint P.RetrieveMethod
                  -- The method which was used to retrieve the source code.
                  (Maybe DebianVersion)
                  -- The version number in the changelog of the freshly downloaded
                  -- package, before any suffix is added by the autobuilder.
                  [PkgVersion]
                  -- The names and version numbers of the build dependencies which
                  -- were present when the package was build.
                  (Maybe DebianVersion)
                  -- This will be the same as the version field plus
                  -- the suffix that was added by the autobuilder.
    | NoFingerprint

readMethod :: String -> Maybe P.RetrieveMethod
readMethod s =
    case maybeRead s :: Maybe P.RetrieveMethod of
      Just method -> Just method
      -- when this code is taken out it will trigger a rebuild of all
      -- packages that haven't been rebuild since 5 Mar 2012.
      Nothing -> case maybeRead s :: Maybe O.RetrieveMethod of
                   Nothing -> Nothing
                   Just method -> Just (convert method)
    where
      convert (O.Apt a b _) = P.Apt a b
      convert (O.Bzr a) = P.Bzr a
      convert (O.Cd a b) = P.Cd a (convert b)
      convert (O.Darcs a _) = P.Darcs a
      convert (O.DebDir a b) = P.DebDir (convert a) (convert b)
      convert (O.Debianize a _) = P.Debianize (P.Hackage a)
      convert (O.Dir a) = P.Dir a
      convert (O.Hackage a _) = P.Hackage a
      convert (O.Hg a) = P.Hg a
      convert (O.Proc a) = P.Proc (convert a)
      convert (O.Quilt a b) = P.Quilt (convert a) (convert b)
      convert (O.SourceDeb a) = P.SourceDeb (convert a)
      convert (O.Svn a) = P.Svn a
      convert (O.Tla a) = P.Tla a
      convert (O.Twice a) = P.Twice (convert a)
      convert (O.Uri a b) = P.Uri a b

packageFingerprint :: Maybe SourcePackage -> Fingerprint
packageFingerprint Nothing = NoFingerprint
packageFingerprint (Just package) =
    maybe NoFingerprint (parseRevision . B.unpack) (S.fieldValue "Fingerprint" . sourceParagraph $ package)
    where
      parseRevision s =
          case reads s :: [(String, String)] of
            [(method, etc)] ->
                case readMethod method of
                  Nothing -> NoFingerprint
                  Just method' ->
                      case words etc of
                        (sourceVersion : buildDeps)
                          | not (elem '=' sourceVersion) ->
                              Fingerprint method' (Just (parseDebianVersion sourceVersion)) (map readPkgVersion buildDeps) (Just . packageVersion . sourcePackageID $ package)
                        buildDeps -> Fingerprint method' Nothing (map readPkgVersion buildDeps) (Just . packageVersion . sourcePackageID $ package)
            _ -> NoFingerprint

showFingerprint :: Fingerprint -> S.Field
showFingerprint (Fingerprint method (Just sourceVersion) versions _) =
    S.Field ("Fingerprint", " " ++ show (show method) ++ " " ++ show (prettyDebianVersion sourceVersion) ++ " " ++ intercalate " " (map showPkgVersion versions))
showFingerprint _ = error "missing fingerprint info"

showDependencies :: Fingerprint -> [String]
showDependencies (Fingerprint _ _ deps _) = map showPkgVersion deps
showDependencies _ = []

dependencyChanges :: Fingerprint -> Fingerprint -> String
dependencyChanges old new =
    depChanges changedDeps
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = map concat . columns . map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (Set.fromList (deps new)) (Set.fromList (deps new)))
      showDepChange newDep =
          case filter (hasName (getName newDep)) (deps old) of
            [] -> [" " ++ unPkgName (unBinPkgName (getName newDep)) ++ ": ", "(none)", " -> ", show (prettyDebianVersion (getVersion newDep))]
            (oldDep : _) -> [" " ++ unPkgName (unBinPkgName (getName newDep)) ++ ": ", show (prettyDebianVersion (getVersion oldDep)), " -> ", show (prettyDebianVersion (getVersion newDep))]
      hasName name = ((== name) . getName)
      prefix = "\n    "
      deps (Fingerprint _ _ x _) = x
      deps NoFingerprint = []

targetFingerprint :: Target -> [BinaryPackage] -> Fingerprint
targetFingerprint target buildDependencySolution =
    Fingerprint sourceRevision (Just sourceVersion) sourceDependencies Nothing
    where
      -- Compute the Revision: string for the source tree.  This
      -- string will appear in the .dsc file for the package, and will
      -- then be copied into the Sources.gz file of the distribution.
      -- For a TLA target this is the current revision name, by
      -- default it is simply the debian version number.  The version
      -- number in the source tree should not have our vendor tag,
      -- that should only be added by the autobuilder.
      sourceRevision = {-T.revision-} T.method (download (tgt target))
      sourceVersion = logVersion sourceLog
      sourceLog = entry . cleanSource $ target
      sourceDependencies = map makeVersion buildDependencySolution
-- |Convert to a simple name and version record to interface with older
-- code.

makeVersion :: BinaryPackage -> PkgVersion
makeVersion package =
    PkgVersion { getName = binaryPackageName package
               , getVersion = packageVersion (packageID package) }

-- |Represents a decision whether to build a package, with a text juststification.
data BuildDecision
    = Yes String
    | No String
    | Arch String	-- Needs a -B build, architecture dependent files only
    | Auto String	-- Needs a 'automated' rebuild, with a generated version number and log entry
    | Error String	-- A fatal condition was encountered - e.g. a build dependency became older since last build

instance Show BuildDecision where
    show (Yes reason) = "Yes - " ++ reason
    show (No reason) = "No - " ++ reason
    show (Arch reason) = "Yes - " ++ reason
    show (Auto reason) = "Yes - " ++ reason
    show (Error reason) = "Error - " ++ reason

-- |Decide whether to build a package.  We will build if the revision
-- is different from the revision of the uploaded source, or if any of
-- the build dependencies are newer than the versions which were
-- encoded into the uploaded version's control file.
buildDecision :: P.CacheRec
              -> Target
              -> Fingerprint            -- The fingerprint of the most recent build
              -> Fingerprint            -- The fingerprint of the source package
              -> SourcePackageStatus	-- ^ The status of the version in the repository with respect
                                        -- to the architecture we are building - either all binary packages
                                        -- are available, or none, or only the architecture independent.
              -> BuildDecision
buildDecision cache target _ _ _ | elem (T.handle (download (tgt target))) (P.forceBuild (P.params cache)) = Yes "--force-build option is set"
buildDecision _ _ NoFingerprint (Fingerprint _ (Just sourceVersion) _ _) _ =
    Yes ("Initial build of version " ++ show (prettyDebianVersion sourceVersion))
buildDecision _ _ (Fingerprint oldMethod _ _ _) (Fingerprint newMethod _ _ _) _
    | oldMethod /= newMethod = Yes ("Retrieve method changed: " ++ show oldMethod ++ " -> " ++ show newMethod)
buildDecision _ _ _ NoFingerprint _ = error "Missing source fingerprint"
buildDecision _ _ _ (Fingerprint _ Nothing _ _) _ = error "Missing source version"
buildDecision cache target (Fingerprint _ oldSrcVersion builtDependencies repoVersion) -- I suspect oldSrcVersion is always equal to repoVersion
                           (Fingerprint _ (Just sourceVersion) sourceDependencies _)
                           releaseStatus =
    case isJust oldSrcVersion of
      True ->
          case compare sourceVersion (fromJust oldSrcVersion) of
            GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
            LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
            EQ -> sameSourceTests
      False ->
          case compare (dropTag allTags sourceVersion) (dropTag allTags (fromJust repoVersion)) of
            GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust repoVersion)) ++ ")")
            LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust repoVersion)) ++ ")")
            EQ ->
                case dropTag allTags sourceVersion == sourceVersion of
                  False -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is tagged, and old source version was not recorded")
                  True -> sameSourceTests
    where
      vendorTag = P.vendorTag (P.params cache)
      oldVendorTags = P.oldVendorTags (P.params cache)
      -- discardTarget = Set.member (targetName target) (P.discard (P.params cache))
      allowBuildDependencyRegressions = P.allowBuildDependencyRegressions (P.params cache)
      -- Build decision tests for when the version number of the
      -- source hasn't changed.  Note that the source itself may have
      -- changed, but we don't ask the SCCS whether that has happened.
      -- This is a design decision which avoids building from source
      -- that might have been checked in but isn't ready to be
      -- uploaded to the repository.  Note that if the build
      -- dependencies change the package will be built anyway, so we
      -- are not completely protected from this possibility.
      sameSourceTests =
          case releaseStatus of
            Indep missing | missing /= [] && not (notArchDep (targetControl target)) ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            _ | badDependencies /= [] && not allowBuildDependencyRegressions ->
                  Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++ 
                         concat (intersperse ", " (map (\ ver -> show (fmap prettyPkgVersion (builtVersion ver)) ++ " -> " ++ show (prettyPkgVersion ver)) badDependencies)))
              | badDependencies /= [] ->
                  Auto ("Build dependency regression: " ++ 
                        concat (intersperse ", " (map (\ ver -> show (fmap prettyPkgVersion (builtVersion ver)) ++ " -> " ++ show (prettyPkgVersion ver)) badDependencies)))
              | autobuiltDependencies /= [] && isNothing oldSrcVersion ->
		  -- If oldSrcVersion is Nothing, the autobuilder didn't make the previous build
                  -- so there are no recorded build dependencies.  In that case we don't really
                  -- know whether a build is required, so we could go either way.  The decision
                  -- here is to only built if some of the build dependencies were built by the
                  -- autobuilder (so their version numbers have been tagged by it.)
                  Auto ("Build dependency status unknown:\n" ++ buildDependencyChangeText autobuiltDependencies)
              | (revvedDependencies ++ newDependencies) /= [] && isJust oldSrcVersion ->
                  -- If the package *was* previously built by the autobuilder we rebuild when any
                  -- of its build dependencies are revved or new ones appear.
                  Auto ("Build dependencies changed:\n" ++ buildDependencyChangeText (revvedDependencies ++ newDependencies))
            Indep _ | notArchDep (targetControl target) ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " of architecture independent package is already in release.")
            Indep missing ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            All ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " is already in release.")
            _ ->
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      notArchDep control =
          all (== "all") . map (maybe "all" strip) . map (lookupP "Architecture") . unControl $ control
          where strip (S.Field (_, s)) = stripWS s
                strip (S.Comment _) = ""
      buildDependencyChangeText dependencies =
          "  " ++ intercalate "\n  " changes
          where
            changes = map (\ (built, new) -> show (fmap prettyPkgVersion built) ++ " -> " ++ show (prettyPkgVersion new)) (zip builtVersions dependencies)
            builtVersions = map findDepByName dependencies
            findDepByName new = find (\ old -> getName new == getName old) builtDependencies
      -- The list of the revved and new dependencies which were built by the autobuilder.
      autobuiltDependencies = filter isTagged (revvedDependencies ++ newDependencies)
      isTagged :: PkgVersion -> Bool
      isTagged dep = isJust . snd . parseTag allTags . getVersion $ dep
      allTags :: [String]
      allTags = vendorTag : oldVendorTags
      -- If we are deciding whether to rebuild the same version of the source package,
      -- this function checks the status of the build dependencies.  If any are older
      -- now than when the package was built previously, it is a fatal error.  Probably
      -- the sources.list changed so that build dependency versions are no longer
      -- available, or some of the build dependencies were never built for the current
      -- build architecture.  If any are younger, we need to rebuild the package.
      -- buildDependencyStatus :: ([PkgVersion], [PkgVersion], [PkgVersion], [PkgVersion])
      (badDependencies, revvedDependencies, newDependencies, _unchangedDependencies) =
          (bad, changed, new, unchanged)
          where
            -- If any dependency is older than the one we last built with it is an error.
            (bad, notBad) = partition isOlder sourceDependencies'
            isOlder x = maybe False (\ built -> getVersion built > getVersion x) (builtVersion x)
            -- If a dependency is newer it generally triggers a rebuild.
            (changed, notChanged) = partition isNewer notBad
            isNewer x = maybe False (\ built -> getVersion built < getVersion x) (builtVersion x)
	    -- Dependencies which we have never seen before also generally trigger a rebuild.
            (new, unchanged) = partition (isNothing . builtVersion) notChanged
	    -- What version of this dependency was most recently used to build?
      builtVersion x = maybe Nothing (\ ver -> Just (PkgVersion (getName x) ver)) (Map.findWithDefault Nothing (getName x) builtDeps)
      builtDeps = Map.fromList (map (\ p -> (getName p, Just (getVersion p))) builtDependencies)
      -- Remove any package not mentioned in the relaxed dependency list
      -- from the list of build dependencies which can trigger a rebuild.
      sourceDependencies' = filter (\ x -> elem (getName x) (packageNames (targetRelaxed (relaxDepends cache (tgt target)) target))) sourceDependencies
      -- All the package names mentioned in a dependency list
      packageNames :: G.DepInfo -> [BinPkgName]
      packageNames info {-(_, deps, _)-} = nub (map (\ (Rel name _ _) -> name) (concat (G.relations info)))
