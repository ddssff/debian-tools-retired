-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
-- 
-- Author: David Fox <ddssff@gmail.com>
{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS -Wall -Werror -fwarn-unused-imports -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.AutoBuilder.Target
    ( changelogText	-- Tgt -> Maybe String -> [PkgVersion] -> String
    , buildTargets
    , showTargets
    ) where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..))
import Control.Exception (SomeException, try, evaluate)
import Control.Monad.RWS(MonadIO(..), MonadTrans(..), when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List(intersperse, intercalate, intersect, isSuffixOf,
                 nub, partition, sortBy)
import Data.Maybe(catMaybes, fromJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.Time(NominalDiffTime)
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Types.Buildable (Buildable(..), Target(tgt, cleanSource, targetDepends), targetName, prepareTarget, targetRelaxed, targetControl, relaxDepends, failing, debianSourcePackageName)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import Debian.AutoBuilder.Types.Fingerprint (Fingerprint, packageFingerprint, showFingerprint, dependencyChanges, targetFingerprint, showDependencies, BuildDecision(..), buildDecision)
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Changes (prettyChanges, ChangesFile(changeRelease, changeInfo, changeFiles, changeDir),
                       ChangedFileSpec(changedFileSize, changedFileName, changedFileMD5sum, changedFileSHA1sum, changedFileSHA256sum),
                       ChangeLogEntry(logWho, logVersion, logDists, logDate, logComments))
import Debian.Control
-- import Debian.Control
import qualified Debian.GenBuildDeps as G
import Debian.Relation (BinPkgName(..), SrcPkgName(..), PkgName(..), prettyRelation, prettyBinPkgName)
import Debian.Relation.ByteString(Relations, Relation(..))
import Debian.Release (Arch, releaseName')
import Debian.Repo.SourceTree (buildDebs)
import Debian.Sources (SliceName(..))
import Debian.Repo (chrootEnv, syncEnv, syncPool, updateEnv)
import Debian.Repo.Cache (binaryPackages, buildArchOfEnv, sourcePackages, aptSourcePackagesSorted)
import Debian.Repo.Dependencies (simplifyRelations, solutions)
import Debian.Repo.Changes (save, uploadLocal)
import Debian.Repo.Insert (scanIncoming, showErrors)
import Debian.Repo.Monad (tryAB)
import Debian.Repo.OSImage (OSImage, updateLists)
import Debian.Repo.Package (binaryPackageSourceVersion, sourcePackageBinaryNames)
import Debian.Repo.SourceTree (SourceTreeC(..), DebianSourceTreeC(..),
                               DebianBuildTree, addLogEntry, copyDebianBuildTree,
                               findChanges, findOneDebianBuildTree, SourcePackageStatus(..))
import Debian.Repo.Monad (AptIOT)
import Debian.Repo.Types (SourcePackage(sourceParagraph, sourcePackageID),
                          AptCache(rootDir, aptBinaryPackages), EnvRoot(rootPath),
                          PackageID(packageVersion), LocalRepository, PkgVersion(..),
                          BinaryPackage(packageInfo), prettyPkgVersion)
import Debian.Time(getCurrentLocalRFC822Time)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.VersionPolicy(parseTag, setTag)
import Extra.Files(replaceFile)
import "Extra" Extra.List(dropPrefix)
import Extra.Misc(columns)
import System.Directory (doesFileExist, doesDirectoryExist, removeDirectory, createDirectoryIfMissing)
import System.Exit(ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.Posix.Files(fileSize, getFileStatus)
import System.Unix.Chroot (useEnv)
import System.Process (CmdSpec(ShellCommand, RawCommand), CreateProcess(cwd))
import System.Process.Progress (unpackOutputs, mergeToStdout, keepStdout, keepResult, collectOutputs,
                                keepResult, runProcessF, runProcess, quieter, qPutStrLn, ePutStr, ePutStrLn)
import System.Process.Read (Chars(toString), readModifiedProcess)
import Text.PrettyPrint (Doc, text, (<>))
import Text.PrettyPrint.Class (pretty)
import Text.Printf(printf)
import Text.Regex(matchRegex, mkRegex)

instance Ord Target where
    compare = compare `on` debianSourcePackageName

prettySimpleRelation :: Maybe PkgVersion -> Doc
prettySimpleRelation rel = maybe (text "Nothing") (\ v -> prettyBinPkgName (getName v) <> text "=" <> prettyDebianVersion (getVersion v)) rel

-- |Generate the details section of the package's new changelog entry
-- based on the target type and version info.  This includes the
-- revision info and build dependency versions in a human readable
-- form.  FIXME: this should also include revision control log
-- entries.
changelogText :: Buildable -> Fingerprint -> Fingerprint -> String
changelogText buildable old new = ("  * " ++ T.logText (download buildable) ++ "\n" ++ dependencyChanges old new ++ "\n")

-- |Generate the string of build dependency versions:
-- package1=version1 package2=version2 ...
_formatVersions :: [PkgVersion] -> String
_formatVersions buildDeps =
    prefix ++
    intercalate prefix (map (show . prettyPkgVersion) buildDeps) ++
    "\n"
    where prefix = "\n    "

prepareTargets :: P.CacheRec -> OSImage -> Relations -> [Buildable] -> AptIOT IO [Target]
prepareTargets cache cleanOS globalBuildDeps targetSpecs =
    do results <- lift $ mapM (prepare (length targetSpecs)) (zip [1..] targetSpecs)
       let (failures, targets) = partitionEithers results
       let msg = "Could not prepare " ++ show (length failures) ++ " targets:\n" ++
                 concatMap (\ (n, e) -> printf "%4d. " n ++ show e ++ "\n") (zip [(1::Int)..] failures)
       case null failures of
         True -> return targets
         False -> ePutStr msg >> error msg
    where
      prepare :: Int -> (Int, Buildable) -> IO (Either SomeException Target)
      prepare count (index, tgt) =
          do qPutStrLn (printf "[%2d of %2d] %s" index count (show (T.method (download tgt))))
             quieter 2 (try (prepareTarget cache globalBuildDeps cleanOS tgt) >>=
                             either (\ (e :: SomeException) ->
                                         ePutStrLn (printf "[%2d of %2d] - could not prepare %s: %s"
                                                           index count (show (T.method (download tgt))) (show e)) >>
                                         return (Left e))
                                    (return . Right))

{-
partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing xs =
    loop xs ([], [])
    where
      loop [] (fs, xs) = (fs, xs)
      loop (Success x : more) (fs, xs) = loop more (fs, x : xs)
      loop (Failure f : more) (fs, xs) = loop more (f : fs, xs)
-}

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (AptCache t) => P.CacheRec -> OSImage -> Relations -> LocalRepository -> t -> [Buildable] -> AptIOT IO (LocalRepository, [Target])
buildTargets _ _ _ localRepo _ [] = return (localRepo, [])
buildTargets cache cleanOS globalBuildDeps localRepo poolOS targetSpecs =
    do
      qPutStrLn "\nAssembling source trees:\n"
      targets <- prepareTargets cache cleanOS globalBuildDeps targetSpecs
      qPutStrLn "\nBuilding all targets:"
      failed <- buildLoop cache globalBuildDeps localRepo poolOS cleanOS targets
      return (localRepo, failed)
    where
      -- targetList <- lift $ countAndPrepareTargets cache globalBuildDeps cleanOS targetSpecs
      --buildAll cleanOS targetList globalBuildDeps

-- Execute the target build loop until all the goals (or everything) is built
-- FIXME: Use sets instead of lists
buildLoop :: (AptCache t) => P.CacheRec -> Relations -> LocalRepository -> t -> OSImage -> [Target] -> AptIOT IO [Target]
buildLoop cache globalBuildDeps localRepo poolOS cleanOS' targets =
    Set.toList <$> loop cleanOS' (Set.fromList targets) Set.empty
    where
      -- This loop computes the ready targets and builds one.
      loop :: OSImage -> Set.Set Target -> Set.Set Target -> AptIOT IO (Set.Set Target)
      loop _ unbuilt failed | Set.null unbuilt = return failed
      loop cleanOS' unbuilt failed =
          ePutStrLn "Computing ready targets..." >>
          case readyTargets cache (goals (Set.toList unbuilt)) (Set.toList unbuilt) of
            [] -> return failed
            triples -> do quieter (-1) $ qPutStrLn (makeTable triples)
                          let ready = Set.fromList $ map (\ (x, _, _) -> x) triples
                          loop2 cleanOS' (Set.difference unbuilt ready) failed triples
      -- Out of ready targets, re-do the dependency computation
      loop2 :: OSImage
            -> Set.Set Target -- unbuilt: targets which have not been built and are not ready to build
            -> Set.Set Target -- failed: Targets which either failed to build or were blocked by a target that failed to build
            -> [(Target, [Target], [Target])] -- ready: the list of known buildable targets
            -> AptIOT IO (Set.Set Target)
      loop2 cleanOS' unbuilt failed [] =
          loop cleanOS' unbuilt failed
      loop2 cleanOS' unbuilt failed ((target, blocked, _) : ready') =
          do ePutStrLn (printf "[%2d of %2d] TARGET: %s - %s"
                        (length targets - (Set.size unbuilt + length ready')) (length targets) (targetName target) (show (T.method (download (tgt target)))))
             -- Build one target.
             result <- if Set.member (targetName target) (P.discard (P.params cache))
                       then return (Failure ["--discard option set"])
                       else tryAB (buildTarget cache cleanOS' globalBuildDeps localRepo poolOS target) >>= return . either (\ (e :: SomeException) -> Failure [show e]) id
             failing -- On failure the target and its dependencies get
                     -- added to failed.
                     (\ errs ->
                          do ePutStrLn ("Package build failed:\n " ++ intercalate "\n " errs ++ "\n" ++
                                        "Discarding " ++ targetName target ++ " and its dependencies:\n  " ++
                                        concat (intersperse "\n  " (map targetName blocked)))
                             let -- Remove the dependencies of the failed packages from unbuilt
                                 unbuilt' = Set.difference unbuilt (Set.fromList blocked)
                                 -- Add the target and its dependencies to failed
                                 failed' = Set.insert target  . Set.union (Set.fromList blocked) $ failed
                             loop2 cleanOS' unbuilt' failed' ready')
                     -- On success the target is discarded and its
                     -- dependencies are added to unbuilt.
                     (\ mRepo ->
                          do cleanOS'' <- maybe (return cleanOS')
                                               (\ _ -> updateEnv cleanOS' >>= either (\ e -> error ("Failed to update clean OS:\n " ++ show e)) return)
                                               mRepo
                             -- Add to unbuilt any blocked packages that weren't already failed by
                             -- some other build
                             let unbuilt' = Set.union unbuilt (Set.difference (Set.fromList blocked) failed)
                             loop2 cleanOS'' unbuilt' failed ready')
                     result
      -- If no goals are given in the build parameters, assume all
      -- known targets are goals.
      goals targets =
          case P.goals (P.params cache) of
            [] -> targets
            goalNames -> filter (\ target -> elem (targetName target) goalNames) targets

      -- Find the sources.list for the distribution we will be building in.
      --indent s = setStyle (addPrefix stderr s)
      --debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun (P.debug params))

makeTable :: [(Target, [Target], [Target])] -> String
makeTable triples =
    unlines . map (intercalate " ") . columns $ goalsLine ++ [[""]] ++ readyLines
    where
      goalsLine = []
      readyLines = map readyLine triples
      readyLine (ready, blocked, _other) =
          [" Ready:", targetName ready, "Blocking " ++ show (length blocked) ++ ": [" ++ intercalate ", " (map targetName blocked) ++ "]"]

-- |Compute the list of targets that are ready to build from the build
-- dependency relations.  The return value is a list of target lists,
-- where the first element of each list is ready to build, and the
-- other elements are blocked by the first target.
--
-- It is not possible to precompute a build order for all the targets,
-- because we don't know ahead of time exactly what packages will be
-- built or even what their versions will be.  Furthermore, it is not
-- even desirable to do it that way, because then when we are actually
-- building the packages and one fails we no longer have enough
-- information to decide which other packages might still be
-- buildable.
--
-- Therefore, the algorithm we want to use is one where we look at
-- set of targets, choose one that can be built, build it, remove it
-- from the target set, and repeat until all targets are built.  We
-- can build a graph of the "has build dependency" relation and find
-- any node that has no inbound arcs and (maybe) build that.
readyTargets :: P.CacheRec -> [Target] -> [Target] -> [(Target, [Target], [Target])]
readyTargets _ [] _ = []
readyTargets cache goals targets =
    -- q12 "Choosing next target" $
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    case G.buildable depends targets of
      (G.CycleInfo arcs) -> error (cycleMessage cache arcs)
      info ->
          case sortBy (compareReady goals) . G.readyTriples $ info of
            [] -> []
            triples -> triples
    where
      -- We choose the next target using the relaxed dependency set
      depends :: Target -> Target -> Ordering
      depends target1 target2 = G.compareSource (targetRelaxed (relaxDepends cache (tgt target1)) target1) (targetRelaxed (relaxDepends cache (tgt target2)) target2)
      -- Choose the next target to build.  Look for targets which are
      -- in the goal list, or which block packages in the goal list.
      -- Among those, prefer the target which blocks the most
      -- packages.  If there are goal targets but none of them are
      -- ready to build or directly block
      -- targets include a goal as readyamongoals none of the
      compareReady :: [Target] -> (Target, [Target], [Target]) ->  (Target, [Target], [Target]) -> Ordering
      compareReady goals' (aReady, aBlocked, _) (bReady, bBlocked, _) =
          -- Prefer targets which include a goal
          case compare (length bGoals) (length aGoals) of
            -- Otherwise, prefer the target which blocks the most other targets
            EQ -> compare (length bBlocked) (length aBlocked)
            x -> x
          where
            aGoals = intersect goals' (aReady : aBlocked)
            bGoals = intersect goals' (bReady : bBlocked)

cycleMessage :: P.CacheRec -> [(Target, Target)] -> String
cycleMessage cache arcs =
    "Dependency cycles formed by these edges need to be broken:\n  " ++
    unlines (map (intercalate " ")
             (columns (["these binary packages", "from this source package", "", "force a rebuild of"] :
                       (map arcTuple arcs)))) ++
    "\nAdd one or more of these lines (but as few as possible) to your configuration file:\n  " ++
    intercalate "\n  " (map relaxLine (nub (concat (map pairs arcs))))
    where
      arcTuple (pkg, dep) =
          let rels = targetRelaxed (relaxDepends cache (tgt pkg)) pkg in
          [(show (intersect (binaryNames pkg dep) (binaryNamesOfRelations rels))), targetName dep, " -> ", targetName pkg]
      relaxLine :: (BinPkgName, SrcPkgName) -> String
      relaxLine (bin, src) = "Relax-Depends: " ++ unPkgName (unBinPkgName bin) ++ " " ++ unPkgName (unSrcPkgName src)
      pairs :: (Target, Target) -> [(BinPkgName, SrcPkgName)]
      pairs (pkg, dep) =
          map (\ bin -> (bin, G.sourceName (targetDepends pkg))) binaryDependencies
              where binaryDependencies = intersect (binaryNames pkg dep) (binaryNamesOfRelations (targetRelaxed (relaxDepends cache (tgt pkg)) pkg))
      binaryNamesOfRelations :: G.DepInfo -> [BinPkgName]
      binaryNamesOfRelations info =
          concat (map (map (\ (Rel name _ _) -> name)) (G.relations info))
      binaryNames :: Target -> Target -> [BinPkgName]
      binaryNames pkg dep = G.binaryNames (targetRelaxed (relaxDepends cache (tgt pkg)) dep)

showTargets :: P.Packages -> String
showTargets targets =
    unlines (heading :
             map (const '-') heading :
             map concat (columns (reverse (snd (P.foldPackages (\ name spec _flags (count, rows) -> (count + 1, [printf "%4d. " count, name, " ", show spec] : rows)) targets (1 :: Int, []))))))
    where
      heading = show (P.packageCount targets) ++ " Targets:"

-- Decide whether a target needs to be built and, if so, build it.
buildTarget ::
    (AptCache t) =>
    P.CacheRec ->			-- configuration info
    OSImage ->				-- cleanOS
    Relations ->			-- The build-essential relations
    LocalRepository ->			-- The local repository the packages will be uploaded to
    t ->
    Target ->
    AptIOT IO (Failing (Maybe LocalRepository))	-- The local repository after the upload (if it changed), or an error message
buildTarget cache cleanOS globalBuildDeps repo poolOS target =
    do
      _cleanOS' <- lift (quieter 2 $ syncPool cleanOS)
      -- Get the control file from the clean source and compute the
      -- build dependencies
      let debianControl = targetControl target
      arch <- liftIO $ buildArchOfEnv (rootDir cleanOS)
      let solns = buildDepSolutions' arch (map (BinPkgName . PkgName) (P.preferred (P.params cache))) cleanOS globalBuildDeps debianControl
      case solns of
        Failure excuses -> do let excuses' = ("Couldn't satisfy build dependencies" : excuses)
                              qPutStrLn (intercalate "\n " excuses')
                              return $ Failure excuses'
        Success [] -> error "Internal error 4"
        Success ((_count, sourceDependencies) : _) ->
            do -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, _message) = getReleaseControlInfo cleanOS target
               let repoVersion = fmap (packageVersion . sourcePackageID) releaseControlInfo
                   oldFingerprint = packageFingerprint releaseControlInfo
               -- Get the changelog entry from the clean source
               let sourceLog = entry . cleanSource $ target
               let sourceVersion = logVersion sourceLog
                   newFingerprint = targetFingerprint target sourceDependencies
               let spkgs = aptSourcePackagesSorted poolOS [G.sourceName (targetDepends target)]
                   buildTrumped = elem (targetName target) (P.buildTrumped (P.params cache))
                   newVersion = computeNewVersion cache spkgs (if buildTrumped then Nothing else releaseControlInfo) sourceVersion
                   decision = buildDecision cache target oldFingerprint newFingerprint releaseStatus
               ePutStrLn ("Build decision: " ++ show decision)
               -- quieter (const 0) $ qPutStrLn ("newVersion: " ++ show (fmap prettyDebianVersion newVersion))
               -- quieter (const 0) $ qPutStrLn ("Release status: " ++ show releaseStatus)
               case newVersion of
                 Failure messages ->
                    return (Failure messages)
                 Success version ->
                     -- If we are doing an arch only build, the version number needs to match the
                     -- version number of the architecture independent package already uploaded.
                     let buildVersion = case decision of
                                          Arch _ -> repoVersion
                                          _ -> Just version in
                     case decision of
                       Error message -> return (Failure [message])
                       No _ -> return (Success Nothing)
                       _ ->  buildPackage cache cleanOS buildVersion oldFingerprint newFingerprint sourceLog target releaseStatus repo >>=
                             return . failing Failure (Success . Just)

-- | Build a package and upload it to the local repository.
buildPackage :: P.CacheRec -> OSImage -> Maybe DebianVersion -> Fingerprint -> Fingerprint -> ChangeLogEntry -> Target -> SourcePackageStatus -> LocalRepository -> AptIOT IO (Failing LocalRepository)
buildPackage cache cleanOS newVersion oldFingerprint newFingerprint sourceLog target status repo =
    checkDryRun >>
    lift prepareImage >>=
    failing (return . Failure) logEntry >>=
    failing (return . Failure) (quieter (-1) . build) >>=
    failing (return . Failure) find >>=
    failing (return . Failure) upload
    where
      checkDryRun = when (P.dryRun (P.params cache))
                      (do qPutStrLn "Not proceeding due to -n option."
                          liftIO (exitWith ExitSuccess))
      prepareImage = prepareBuildImage cache cleanOS newFingerprint buildOS target
      logEntry buildTree =
          case P.noClean (P.params cache) of
            False -> liftIO $ maybeAddLogEntry buildTree newVersion >> return (Success buildTree)
            True -> return (Success buildTree)
      build :: DebianBuildTree -> AptIOT IO (Failing (DebianBuildTree, NominalDiffTime))
      build buildTree =
          do -- The --commit flag does not appear until dpkg-dev-1.16.1,
             -- so we need to check this version number.  We also
             -- don't want to leave the patches subdirectory here
             -- unless we actually created a patch.
             _ <- liftIO $ useEnv' root (\ _ -> return ())
                             (-- Get the version number of dpkg-dev in the build environment
                              runProcessF id (ShellCommand ("dpkg -s dpkg-dev | sed -n 's/^Version: //p'")) L.empty >>= return . head . words . L.unpack . L.concat . keepStdout >>= \ installed ->
                              -- If it is >= 1.16.1 we may need to run dpkg-source --commit.
                              runProcess id (ShellCommand ("dpkg --compare-versions '" ++ installed ++ "' ge 1.16.1")) L.empty >>= return . (== [ExitSuccess]) . keepResult >>= \ newer ->
                              when newer (doesDirectoryExist (path' </> "debian/patches") >>= doDpkgSource)
                              {- when newer (do createDirectoryIfMissing True (path' </> "debian/patches")
                                             -- Create the patch if there are any changes
                                             _ <- lazyProcessF "dpkg-source" ["--commit", ".", "autobuilder.diff"] (Just path') Nothing L.empty
                                             -- If the patch was not created, remove the directory
                                             exists <- doesFileExist (path' </> "debian/patches/autobuilder.diff")
                                             when (not exists) (removeDirectory (path' </> "debian/patches"))) -}
                             )
             result <- liftIO $ try (T.buildWrapper (download (tgt target))
                                     (buildDebs (P.noClean (P.params cache)) False (P.setEnv (P.params cache)) buildOS buildTree status))
             case result of
               Left (e :: SomeException) -> return (Failure [show e])
               Right elapsed -> return (Success (buildTree, elapsed))
          where
            doDpkgSource False =
                createDirectoryIfMissing True (path' </> "debian/patches") >>
                doDpkgSource' >>
                doesFileExist (path' </> "debian/patches/autobuilder.diff") >>= \ exists ->
                when (not exists) (removeDirectory (path' </> "debian/patches"))
            doDpkgSource True =
                doDpkgSource' >>
                return ()
            doDpkgSource' = readModifiedProcess (\ p -> p {cwd = Just path'}) (RawCommand "dpkg-source" ["--commit", ".", "autobuilder.diff"]) L.empty
            path' = fromJust (dropPrefix root path)
            path = debdir buildTree
            root = rootPath (rootDir buildOS)
      find (buildTree, elapsed) =
          liftIO $ try (findChanges buildTree) >>=
                   return . either (\ (e :: SomeException) -> Failure [show e]) (\ changesFile -> Success (changesFile, elapsed))
      upload :: (ChangesFile, NominalDiffTime) -> AptIOT IO (Failing LocalRepository)
      upload (changesFile, elapsed) = doLocalUpload elapsed changesFile
      -- Depending on the strictness, build dependencies either
      -- get installed into the clean or the build environment.
      maybeAddLogEntry _ Nothing = return ()
      maybeAddLogEntry buildTree (Just newVersion) = getCurrentLocalRFC822Time >>= return . makeLogEntry newVersion >>= (flip addLogEntry) buildTree
      makeLogEntry newVersion date =
          sourceLog { logVersion = newVersion,
                      logDists = [P.buildRelease (P.params cache)],
                      logWho = P.autobuilderEmail (P.params cache),
                      logDate = date,
                      logComments = init (logComments sourceLog) ++ changelogText (tgt target) oldFingerprint newFingerprint }
      setDistribution name changes =
          let (Paragraph fields) = changeInfo changes in
          let info' = map (setDist name) fields in
          changes { changeInfo = Paragraph info'
                  , changeRelease = name }
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", ' ' : releaseName' name)
                setDist _ other = other
      doLocalUpload :: NominalDiffTime -> ChangesFile -> AptIOT IO (Failing LocalRepository)
      doLocalUpload elapsed changesFile =
          do
            (changesFile' :: ChangesFile) <-
		-- Set the Distribution field in the .changes file to the one
                -- specified by the autobuilder Build-Release parameter.
                return (setDistribution (P.buildRelease (P.params cache)) changesFile) >>=
                -- Insert information about the build into the .changes file.
                liftIO . updateChangesFile elapsed >>=
                -- Insert the revision info into the .dsc file and update
                -- the md5sum of the .dsc file in the .changes file.
                liftIO . setRevisionInfo newFingerprint
            -- Upload to the local apt repository
            lift $ uploadLocal repo changesFile'
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            (_, errors) <- scanIncoming True Nothing repo
            case errors of
              -- Update lists to reflect the availability of the package we just built
              [] -> liftIO (updateLists cleanOS) >> return (Success repo)
              _ -> return (Failure ["Local upload failed:\n " ++ showErrors (map snd errors)])
      buildOS = Debian.Repo.chrootEnv cleanOS (P.dirtyRoot cache)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: P.CacheRec -> OSImage -> Fingerprint -> OSImage -> Target -> IO (Failing DebianBuildTree)
prepareBuildImage cache cleanOS sourceFingerprint buildOS target | P.strictness (P.params cache) == P.Lax =
    -- Install dependencies directly into the clean environment
    installDependencies cleanOS (cleanSource target) buildDepends sourceFingerprint >>=
    failing (return . Failure) (prepareTree noClean)
    where
      prepareTree True _ =
          (\ x -> qPutStrLn "Finding build tree" >> quieter 1 x) $
          findOneDebianBuildTree newPath >>=
          return . maybe (Failure ["No build tree at " ++ show newPath]) Success
      prepareTree False _ =
          (\ x -> qPutStrLn "Copying build tree..." >> quieter 1 x) $
          Debian.Repo.syncEnv cleanOS buildOS >>=
          const (try (copyDebianBuildTree (cleanSource target) newPath)) >>=
          return . either (\ (e :: SomeException) -> Failure [show e]) Success
      buildDepends = (P.buildDepends (P.params cache))
      noClean = P.noClean (P.params cache)
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) oldPath)
      oldPath = topdir . cleanSource $ target
prepareBuildImage cache cleanOS sourceFingerprint buildOS target =
    -- Install dependencies directly into the build environment
    findTree noClean >>=
    failing (return . Failure) downloadDeps >>=
    failing (return . Failure) (syncEnv noClean) >>=
    failing (return . Failure) installDeps
    where
      -- findTree :: Bool -> IO (Failing DebianBuildTree)
      findTree False =
          (\ x -> qPutStrLn "Finding build tree" >> quieter 1 x) $
              try (copyDebianBuildTree (cleanSource target) newPath) >>=
              return . either (\ (e :: SomeException) -> Failure [show e]) Success
      findTree True =
          findOneDebianBuildTree newPath >>= \ tree ->
          return $ maybe (Failure ["prepareBuildImage: could not find build tree in " ++ newPath]) Success tree

      downloadDeps buildTree = downloadDependencies cleanOS buildTree buildDepends sourceFingerprint >>=
                               failing (return . Failure) (const (return (Success buildTree)))

      syncEnv False buildTree =
          (\ x -> qPutStrLn "Syncing buildOS" >> quieter 1 x) $
              Debian.Repo.syncEnv cleanOS buildOS >>= (\ os -> return (Success (os, buildTree)))
      syncEnv True buildTree =
          return (Success (buildOS, buildTree))

      installDeps (buildOS, buildTree) = installDependencies buildOS buildTree buildDepends sourceFingerprint >>=
                                         failing (return . Failure) (const (return (Success buildTree)))
      buildDepends = P.buildDepends (P.params cache)
      noClean = P.noClean (P.params cache)
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) (topdir (cleanSource target)))

-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.
getReleaseControlInfo :: OSImage -> Target -> (Maybe SourcePackage, SourcePackageStatus, String)
getReleaseControlInfo cleanOS target =
    case zip sourcePackages (map (isComplete binaryPackages) sourcePackagesWithBinaryNames) of
      (info, status@Complete) : _ -> (Just info, All, message status)
      (info, status@(Missing missing)) : _ -> (Just info, Indep missing, message status)
      _ -> (Nothing, None, message Complete)
    where
      packageName = G.sourceName (targetDepends target)
      message status =
          intercalate "\n"
                  (["  Source Package Versions: " ++ show (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages) (map sourcePackageBinaryNames sourcePackages)) ++
                   missingMessage status ++
                   ["  Binary Package Versions: " ++ show (map (second prettyDebianVersion . binaryPackageVersion) binaryPackages),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages) (map (availableDebNames binaryPackages) sourcePackages)))
      missingMessage Complete = []
      missingMessage (Missing missing) = ["  Missing Binary Package Names: "] ++ map (\ p -> "   " ++ unPkgName (unBinPkgName p)) missing
      sourcePackagesWithBinaryNames = zip sourcePackages (map sourcePackageBinaryNames sourcePackages)
      binaryPackages = Debian.Repo.Cache.binaryPackages cleanOS (nub . concat . map sourcePackageBinaryNames $ sourcePackages)
      sourcePackages = sortBy compareVersion . Debian.Repo.Cache.sourcePackages cleanOS $ [packageName]
      sourcePackageVersion package =
          case ((fieldValue "Package" . sourceParagraph $ package), (fieldValue "Version" . sourceParagraph $ package)) of
            (Just name, Just version) -> (B.unpack name, parseDebianVersion (B.unpack version))
            _ -> error "Missing Package or Version field"
      binaryPackageVersion package =
          case ((fieldValue "Package" . packageInfo $ package), (fieldValue "Version" . packageInfo $ package)) of
            (Just name, Just version) -> (BinPkgName (PkgName (B.unpack name)), parseDebianVersion (B.unpack version))
            _ -> error "Missing Package or Version field"
      compareVersion a b = case ((fieldValue "Version" . sourceParagraph $ a), (fieldValue "Version" . sourceParagraph $ b)) of
                             (Just a', Just b') -> compare (parseDebianVersion . B.unpack $ b') (parseDebianVersion . B.unpack $ a')
                             _ -> error "Missing Version field"
      -- The source package is complete if the correct versions of the
      -- required binary packages are all available, either as debs or
      -- udebs.  Because it is easier to check for available debs, we
      -- do that first and only check for udebs if some names are missing.
      isComplete :: [BinaryPackage] -> (SourcePackage, [BinPkgName]) -> Status
      isComplete binaryPackages (sourcePackage, requiredBinaryNames) =
          if Set.difference missingDebs udebs == Set.empty {- && (unableToCheckUDebs || missingUdebs == Set.empty) -}
          then Complete
          else Missing (Set.toList missingDebs ++ Set.toList missingUdebs)
          where
            (_readyDebs, missingDebs) = Set.partition (`Set.member` availableDebs) required
            (_readyUdebs, missingUdebs) =
                if unableToCheckUDebs
                then (Set.empty, Set.empty)
                else Set.partition (`Set.member` (Set.union availableDebs availableUDebs)) required
            required = Set.fromList requiredBinaryNames
            -- Which binary packages produced from this source package are available?
            availableDebs = Set.fromList (availableDebNames binaryPackages sourcePackage)
            availableUDebs = Set.fromList (availableUDebNames sourcePackage)
      udebs :: Set.Set BinPkgName
      udebs = foldr collect Set.empty (T.flags (download (tgt target)))
      collect :: P.PackageFlag -> Set.Set BinPkgName -> Set.Set BinPkgName
      collect (P.UDeb name) udebs = Set.insert (BinPkgName (PkgName name)) udebs
      collect _ udebs = udebs
      -- A binary package is available either if it appears in the
      -- package index, or if it is an available udeb.
      availableDebNames :: [BinaryPackage] -> SourcePackage -> [BinPkgName]
      availableDebNames binaryPackages sourcePackage =
          map fst . map binaryPackageVersion . filter checkSourceVersion $ binaryPackages
          where checkSourceVersion binaryPackage = maybe False ((==) sourceVersion) (binaryPackageSourceVersion binaryPackage)
                sourceVersion = sourcePackageVersion sourcePackage
      --  or (if it is a udeb) if it simply exists on the
      -- server and has the correct filename.  There is no way to
      -- decide whether a package is a udeb from the package indexes.
      unableToCheckUDebs = True
      availableUDebNames :: SourcePackage -> [BinPkgName]
      availableUDebNames _sourcePackage = (error "availableUDebNames")

data Status = Complete | Missing [BinPkgName]

-- |Compute a new version number for a package by adding a vendor tag
-- with a number sufficiently high to trump the newest version in the
-- dist, and distinct from versions in any other dist.
computeNewVersion :: P.CacheRec -> [SourcePackage] -> Maybe SourcePackage -> DebianVersion -> Failing DebianVersion
computeNewVersion cache
                  available		-- All the versions that exist in the pool in any dist,
					-- the new version number must not equal any of these.
                  current		-- The control file paragraph for the currently uploaded
                                        -- version in this dist.  The new version must be newer
                                        -- than this.
                  sourceVersion =	-- Version number in the changelog entry of the checked-out
                                        -- source code.  The new version must also be newer than this.
    case P.doNotChangeVersion (P.params cache) of
      True -> Success sourceVersion
      False ->
          let vendor = P.vendorTag (P.params cache)
              oldVendors = P.oldVendorTags (P.params cache)
              release = if (P.isDevelopmentRelease (P.params cache)) then
                            Nothing else
                            (Just (sliceName (P.baseRelease (P.params cache))))
              extra = P.extraReleaseTag (P.params cache) 
              aliases = \ x -> maybe x id (lookup x (P.releaseAliases (P.params cache))) in
          case parseTag (vendor : oldVendors) sourceVersion of

            (_, Just tag) -> Failure ["Error: the version string in the changelog has a vendor tag (" ++ show tag ++
                                      ".)  This is prohibited because the autobuilder needs to fully control suffixes" ++
                                      " of this form.  This makes it difficult for the author to know what version" ++
                                      " needs to go into debian/changelog to trigger a build by the autobuilder," ++
                                      " particularly since each distribution may have different auto-generated versions."]
            (_, Nothing) -> setTag aliases vendor oldVendors release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion >>=
                            checkVersion
    where
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      currentVersion =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)
      checkVersion :: DebianVersion -> Failing DebianVersion
      checkVersion result =
          maybe (Success result)
                (\ v -> if result <= v
                        then Failure ["Autobuilder bug: new version number " ++ show (prettyDebianVersion result) ++ " is not newer than current version number " ++ show (prettyDebianVersion v)]
                        else Success result)
                currentVersion

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions' :: Arch -> [BinPkgName] -> OSImage -> Relations -> Control -> Failing [(Int, [BinaryPackage])]
buildDepSolutions' arch preferred os globalBuildDeps debianControl =
    -- q12 "Searching for build dependency solution" $
    -- We don't discard any dependencies here even if they are
    -- mentioned in Relax-Depends, that only applies to deciding
    -- whether to build, once we are building we need to install all
    -- the dependencies.  Hence this empty list.
    case G.buildDependencies debianControl of
      Left s -> Failure [s]
      Right info ->
          let relations' = G.relations info ++ globalBuildDeps
              relations'' = simplifyRelations packages relations' preferred arch in
          -- Do not stare directly into the solutions!  Your head will
          -- explode (because there may be a lot of them.)  Also, this
          -- will be slow if solutions is not compiled.
          case Debian.Repo.Dependencies.solutions packages (filter (not . alwaysSatisfied) relations'') 100000 of
            Left error -> Failure [error, message relations' relations'']
            Right solutions -> {- quieter (+ 1) $ qPutStrLn (message relations' relations'') >> return -} Success solutions
    where
      alwaysSatisfied xs = any isNothing xs && all isNothing xs
      packages = aptBinaryPackages os
      message relations' relations'' =
          "Build dependency relations:\n " ++
          concat (intersperse "\n " (map (\ (a, b) -> show (map prettyRelation a) ++ " -> " ++ show (map prettySimpleRelation b))
                                              (zip relations' relations'')))
      -- Group and merge the relations by package.  This can only be done
      -- to AND relations that include a single OR element, but these are
      -- extremely common.  (Not yet implemented.)
{-
      mergeRelations :: Relations -> Relations
      mergeRelations relations = relations
          let pairs = zip (map namesOf relations) relations in
          let pairs' = sortBy ((==) . fst) pairs in
          let pairs'' = groupBy (\ a b -> fst a == fst b) pairs' in
          map concat (map merge pairs'')
          where
            merge ([name], rels) = ([name], [rel])
            mergeAnds ([name], (Rel _ (Just v1) a) : (Rel _ Nothing _) : more) = merge ([name], (Rel _ (Just v1) a) : more)
            mergeAnds ([name], (Rel _ Nothing a) : (Rel _ Nothing _) : more) = merge ([name], (Rel _ Nothing a) : more)
          let (simple, compound) = partition ((== 1) . length . fst) pairs in
          
          let (simple, compound) = partition ((== 1) . length . nub . map nameOf) relations in
          undefined
          where
            namesOf relation = nub (map nameOf)
            nameOf (Rel name _ _) = name
-}

-- In ghc610, using readFile on pseudo files in /proc hangs.  Use this instead.
--rf path = lazyCommand ("cat '" ++ path ++ "'") L.empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

parseProcCpuinfo :: IO [(String, String)]
parseProcCpuinfo =
    readFile "/proc/cpuinfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/cpuinfo"

parseProcMeminfo :: IO [(String, String)]
parseProcMeminfo =
    readFile "/proc/meminfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/meminfo"

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll a ((a', b) : pairs) | a == a' = b : (lookupAll a pairs)
lookupAll a (_ : pairs) = lookupAll a pairs

-- |Add Build-Info field to the .changes file
updateChangesFile :: NominalDiffTime -> ChangesFile -> IO ChangesFile
updateChangesFile elapsed changes =
    (\ x -> qPutStrLn "Updating changes file" >> quieter 1 x) $
    do
      let (Paragraph fields) = changeInfo changes
{-    autobuilderVersion <- processOutput "dpkg -s autobuilder | sed -n 's/^Version: //p'" >>=
                            return . either (const Nothing) Just >>=
                            return . maybe Nothing (listToMaybe . lines) -}
      hostname <- runProcessF id (ShellCommand "hostname") L.empty >>= return . listToMaybe . lines . L.unpack . L.concat . keepStdout
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      machine <- runProcessF id (ShellCommand "uname -m") L.empty >>= return . listToMaybe . lines . L.unpack . L.concat . keepStdout
      let buildInfo = ["Autobuilder-Version: " ++ V.autoBuilderVersion] ++
                      ["Time: " ++ show elapsed] ++
                      maybeField "Memory: " (lookup "MemTotal" memInfo) ++
                      maybeField "CPU: " (lookup "model name" cpuInfo) ++
                      ["CPU count: " ++ (show . length . lookupAll "processor" $ cpuInfo)] ++
                      maybeField "OS Architecture: " machine ++
                      maybeField "CPU MHz: " (lookup "cpu MHz" cpuInfo) ++
                      maybeField "CPU cache: " (lookup "cache size" cpuInfo) ++
                      maybeField "Host: " hostname
      let fields' = sinkFields (== "Files")
                    (Paragraph $ fields ++ [Field ("Build-Info", "\n " ++ intercalate "\n " buildInfo)])
      -- let changes' = changes {changeInfo = Paragraph fields'}
      -- replaceFile (Debian.Repo.path changes') (show (Control [fields']))
      return changes {changeInfo = fields'}
    where
      maybeField tag value = maybe [] ((: []) . (tag ++)) value

-- |Move this to {-Debian.-} Control
sinkFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> Fingerprint -> IO (Failing String)
downloadDependencies os source extra sourceFingerprint =

    do -- qPutStrLn "Downloading build dependencies"
       quieter 1 $ qPutStrLn $ "Dependency package versions:\n " ++ intercalate "\n  " (showDependencies sourceFingerprint)
       qPutStrLn ("Downloading build dependencies into " ++ rootPath (rootDir os))
       (code, out, _, _) <- useEnv' (rootPath root) forceList (runProcess id (ShellCommand command) L.empty) >>=
                            return . unpackOutputs . mergeToStdout
       case code of
         [ExitSuccess] -> return (Success out)
         code -> return (Failure ["FAILURE: " ++ command ++ " -> " ++ show code ++ "\nOutput:\n" ++ out])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True --download-only " ++ intercalate " " (showDependencies sourceFingerprint ++ extra)
      path = pathBelow (rootPath root) (topdir source)
      root = rootDir os

pathBelow :: FilePath -> FilePath -> FilePath
pathBelow root path =
    maybe (error message) id (dropPrefix root path)
    where message = "Expected a path below " ++ root ++ ", saw " ++ path

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> Fingerprint -> IO (Failing L.ByteString)
installDependencies os source extra sourceFingerprint =
    do qPutStrLn $ "Installing build dependencies into " ++ rootPath (rootDir os)
       (code, out, _, _) <- Proc.withProc os (useEnv' (rootPath root) forceList $ runProcess id (ShellCommand command) L.empty) >>= return . collectOutputs . mergeToStdout
       case code of
         [ExitSuccess] -> return (Success out)
         code -> ePutStrLn ("FAILURE: " ++ command ++ " -> " ++ show code ++ "\n" ++ toString out) >>
                 return (Failure ["FAILURE: " ++ command ++ " -> " ++ show code])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True " ++ intercalate " " (showDependencies sourceFingerprint ++ extra)
      --aptGetCommand = "apt-get --yes build-dep -o APT::Install-Recommends=False " ++ sourcpackagename
      path = pathBelow (rootPath root) (topdir source)
      root = rootDir os

-- | This should probably be what the real useEnv does.
useEnv' :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv' rootPath force action = quieter 1 $ useEnv rootPath force $ quieter (-1) action

-- |Set a "Revision" line in the .dsc file, and update the .changes
-- file to reflect the .dsc file's new md5sum.  By using our newdist
-- program to update the pool, this line from the .dsc file is then
-- included in the package's entry in the Sources.gz file.  Then we
-- can compare the revision from the uploaded package with the current
-- TLA revision to decide whether to build.
setRevisionInfo :: Fingerprint -> ChangesFile -> IO ChangesFile
setRevisionInfo fingerprint changes {- @(Changes dir name version arch fields files) -} =
    (\ x -> qPutStrLn "Setting revision info" >> quieter 1 x) $
    case partition (isSuffixOf ".dsc" . changedFileName) (changeFiles changes) of
      ([file], otherFiles) ->
          do
            let dscFilePath = changeDir changes ++ "/" ++ changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            replaceFile dscFilePath (show (pretty newDscFile))
            md5 <- md5sum dscFilePath
            sha1 <- sha1sum dscFilePath
            sha256 <- sha256sum dscFilePath
            case (md5, sha1, sha256) of
              (Success md5, Success sha1, Success sha256) ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = md5, changedFileSHA1sum = sha1, changedFileSHA256sum = sha256, changedFileSize = size}])}
                    Debian.Repo.Changes.save changes'
                    return changes'
              e -> error (show e)
      -- A binary only build will have no .dsc file
      ([], _) -> return changes
      (several, _) -> error ("Multiple .dsc files found in source package: " ++ intercalate ", " (map (show . prettyChanges) several))
    where
      addField (Control (Paragraph sourceInfo : binaryInfo)) =
          Control (newSourceInfo : binaryInfo)
          where newSourceInfo = raiseFields (/= "Files") (Paragraph (sourceInfo ++ [showFingerprint fingerprint]))
      addField (Control []) = error "Invalid control file"

-- | Run a checksum command on a file, return the resulting checksum as text.
doChecksum :: String -> (String -> String) -> FilePath -> IO (Failing String)
doChecksum cmd f path =
    runProcess id (RawCommand cmd' [path]) L.empty >>=
    return . either (doError (cmd' ++ " " ++ path)) (Success . f) . toEither . unpackOutputs
    where cmd' = "/usr/bin/" ++ cmd

md5sum :: FilePath -> IO (Failing String)
md5sum = doChecksum "md5sum" (take 32)
sha1sum :: FilePath -> IO (Failing String)
sha1sum = doChecksum "sha1sum" (take 40)
sha256sum :: FilePath -> IO (Failing String)
sha256sum = doChecksum "sha256sum" (take 64)

toEither :: ([ExitCode], a, String, [IOError]) -> Either ([ExitCode], a, String, [IOError]) a
toEither ([ExitSuccess], text, "", _) = Right text
toEither x = Left x

doError :: Show a => String -> ([ExitCode], t, a, [IOError]) -> Failing a
doError cmd ([ExitFailure n], _, _, _) = Failure ["Error " ++ show n ++ " running '" ++ cmd ++ "'"]
doError cmd (_, _, s, _) = Failure ["Unexpected error output from " ++ cmd ++ ": " ++ show s]

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
