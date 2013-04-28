{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables, TypeFamilies #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..), maybeRead)
import Control.Exception(SomeException, try, AsyncException(UserInterrupt), fromException, toException)
import Control.Monad(foldM, when, unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, throw)
import Control.Monad.State(MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.List as List (intercalate, null)
import Data.Maybe(fromMaybe)
import Data.Set as Set (Set, insert, empty, fromList, toList, null, difference)
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildTarget (retrieve)
import Debian.AutoBuilder.Env (cleanEnv, dependEnv, buildEnv)
import Debian.AutoBuilder.Monads.Deb (MonadDeb)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Target(buildTargets, showTargets)
import Debian.AutoBuilder.Types.Buildable (Target, targetName, Buildable, asBuildable)
import qualified Debian.AutoBuilder.Types.CacheRec as C
import Debian.AutoBuilder.Types.Download (Download)
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Debianize (Atoms)
import Debian.Release (parseSection', releaseName')
import Debian.Sources (SliceName(..))
import Debian.Repo.AptImage(prepareAptEnv)
import Debian.Repo.Cache(updateCacheSources)
import Debian.Repo.Insert(deleteGarbage)
import Debian.Repo.Monads.Apt (MonadApt(getApt), runAptT, getRepoMap)
import Debian.Repo.Monads.Top (MonadTop(askTop), sub, runTopT)
import Debian.Repo.OSImage(OSImage, buildEssential, prepareEnv, chrootEnv)
import Debian.Repo.Release(prepareRelease)
import Debian.Repo.Repository(uploadRemote, verifyUploadURI)
import Debian.Repo.Slice(appendSliceLists, inexactPathSlices, releaseSlices, repoSources)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types (EnvRoot(EnvRoot, rootPath), EnvPath(..), outsidePath)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (Repository, LocalRepository(repoRoot), Layout(Flat), prepareLocalRepository, flushLocalRepository, NamedSliceList(..), SliceList(slices), sliceReleaseNames)
import Debian.URI(URI(uriScheme, uriPath, uriAuthority), URIAuth(uriUserInfo, uriRegName, uriPort), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Prelude hiding (null)
import System.Environment (getArgs, getEnv)
import System.Directory(createDirectoryIfMissing, doesDirectoryExist)
import System.Posix.Files(removeLink)
import System.Exit(ExitCode(..), exitWith)
import qualified System.IO as IO
import System.IO.Error(isDoesNotExistError)
import System.Process (shell)
import System.Process.Progress (Output, timeTask, defaultVerbosity, runProcessF, withModifiedVerbosity, quieter, noisier, qPutStrLn, qPutStr, ePutStrLn, ePutStr)
import System.Unix.Directory(removeRecursiveSafely)
import Text.Printf ( printf )
import Text.PrettyPrint.ANSI.Leijen (pretty)

main :: Atoms -> (FilePath -> String -> P.ParamRec) -> IO ()
main atoms myParams =
    do IO.hPutStrLn IO.stderr "Autobuilder starting..."
       args <- getArgs
       home <- getEnv "HOME"
       -- Compute all the ParamRecs implied by the command line
       -- argument, using myParams to create each default ParamRec
       -- value.
       let recs = P.getParams args (myParams home)
       case any P.doHelp recs of
         True -> IO.hPutStr IO.stderr (P.usage "Usage: ")
         False ->
             do let paramSets = map (\ params -> params {P.buildPackages = P.buildTargets params (P.knownPackages params)}) recs
                results <- runAptT (foldM (doParameterSet atoms) [] paramSets) `catch` handle
                IO.hFlush IO.stdout
                IO.hFlush IO.stderr
                -- The result of processing a set of parameters is either an
                -- exception or a completion code.  Here we print a summary and
                -- exit with a suitable result code.
                -- ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results)))
                case partitionFailing results of
                  ([], _) -> return ()
                  _ ->
                      ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results))) >>
                      exitWith (ExitFailure 1)
    where showResult (Failure ss) = intercalate "\n  " ("Failure:" : ss)
          showResult (Success _) = "Ok"
          partitionFailing :: [Failing a] -> ([[String]], [a])
          partitionFailing xs = p ([], []) xs
              where p result [] = result
                    p (fs, ss) (Failure f : more) = p (f : fs, ss) more
                    p (fs, ss) (Success s : more) = p (fs, s : ss) more
          handle (e :: SomeException) = IO.hPutStrLn IO.stderr ("Exception: " ++ show e) >> throw e

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.  Stop on first failure.
doParameterSet :: MonadApt m => Atoms -> [Failing ([Output L.ByteString], NominalDiffTime)] -> P.ParamRec -> m [Failing ([Output L.ByteString], NominalDiffTime)]
doParameterSet defaultAtoms results params =
    case () of
      _ | not (Set.null badForceBuild) ->
            error $ "Invalid forceBuild target name(s): " ++ intercalate ", " (map P.unTargetName (toList badForceBuild))
        | not (Set.null badBuildTrumped) ->
            error $ "Invalid buildTrumped target name(s): " ++ intercalate ", " (map P.unTargetName (toList badBuildTrumped))
        | not (Set.null badGoals) ->
            error $ "Invalid goal target name(s): " ++ intercalate ", " (map P.unTargetName (toList badGoals))
        | not (Set.null badDiscards) ->
            error $ "Invalid discard target name(s): " ++ intercalate ", " (map P.unTargetName (toList badDiscards))
        | any isFailure results ->
            return results
      _ ->
          noisier (P.verbosity params)
            (do top <- liftIO $ P.computeTopDir params
                withLock (top ++ "/lockfile") (runTopT top (quieter 2 (P.buildCache params) >>= runParameterSet defaultAtoms)))
            `IO.catch` (\ (e :: SomeException) -> return (Failure [show e])) >>=
          (\ result -> return (result : results))
    where
      badForceBuild = difference (fromList (P.forceBuild params)) allTargetNames
      badBuildTrumped = difference (fromList (P.buildTrumped params)) allTargetNames
      badGoals = difference (fromList (P.goals params)) allTargetNames
      badDiscards = difference (P.discard params) allTargetNames
      -- Set of bogus target names in the forceBuild list
      -- badTargetNames names = difference names allTargetNames
      isFailure (Failure _) = True
      isFailure _ = False
      allTargetNames :: Set P.TargetName
      allTargetNames = P.foldPackages (\ name _ _ result -> insert name result) (P.buildPackages params) empty

prepareDependOS :: MonadDeb m => P.ParamRec -> NamedSliceList -> LocalRepository -> m OSImage
prepareDependOS params buildRelease localRepo =
    do dependRoot <- dependEnv (P.buildRelease params)
       exists <- liftIO $ doesDirectoryExist (rootPath dependRoot)
       when (not exists || P.flushDepends params)
            (do cleanRoot <- cleanEnv (P.buildRelease params)
                _ <- prepareEnv cleanRoot
                                buildRelease
                                (Just localRepo)
                                (P.flushRoot params)
                                (P.ifSourcesChanged params)
                                (P.includePackages params)
                                (P.excludePackages params)
                                (P.components params)
                _ <- rsync ["-x"] (rootPath cleanRoot) (rootPath dependRoot)
                return ())
       prepareEnv dependRoot
                  buildRelease
                  (Just localRepo)
                  False
                  (P.ifSourcesChanged params)
                  (P.includePackages params)
                  (P.excludePackages params)
                  (P.components params)

runParameterSet :: MonadDeb m => Atoms -> C.CacheRec -> m (Failing ([Output L.ByteString], NominalDiffTime))
runParameterSet defaultAtoms cache =
    do
      top <- askTop
      liftIO doRequiredVersion
      liftIO doVerifyBuildRepo
      when (P.showParams params) (withModifiedVerbosity (const defaultVerbosity) (liftIO doShowParams))
      when (P.showSources params) (withModifiedVerbosity (const defaultVerbosity) (liftIO doShowSources))
      when (P.flushAll params) (liftIO $ doFlush top)
      liftIO checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      dependOS <- prepareDependOS params buildRelease localRepo
      _ <- updateCacheSources (P.ifSourcesChanged params) dependOS
      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      globalBuildDeps <- liftIO $ buildEssential dependOS
      -- Get a list of all sources for the local repository.
      localSources <- (\ x -> qPutStrLn "Getting local sources" >> quieter 1 x) $
          case parseURI ("file://" ++ envPath (repoRoot localRepo)) of
            Nothing -> error $ "Invalid local repo root: " ++ show (repoRoot localRepo)
            Just uri -> repoSources (Just . envRoot . repoRoot $ localRepo) uri
      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      let poolSources = NamedSliceList { sliceListName = SliceName (sliceName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <-prepareAptEnv top (P.ifSourcesChanged params) poolSources
      (failures, targets) <- retrieveTargetList dependOS >>= return . partitionEithers
      when (not $ List.null $ failures) (error $ unlines $ "Some targets could not be retrieved:" : map ("  " ++) failures)
      buildResult <- buildTargets cache dependOS globalBuildDeps localRepo poolOS targets
      -- If all targets succeed they may be uploaded to a remote repo
      result <- (upload buildResult >>= liftIO . newDist) `IO.catch` (\ (e :: SomeException) -> return (Failure [show e]))
      updateRepoCache
      return result
    where
      params = C.params cache
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease params))
      buildRepoSources = C.buildRepoSources cache
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion V.autoBuilderVersion
              rqvs = P.requiredVersion params in
          case filter (\ (v, _) -> v > abv) rqvs of
            [] -> quieter 1 $ qPutStrLn $ "Installed autobuilder version " ++ show (prettyDebianVersion abv) ++ " newer than required: " ++ show (map (first prettyDebianVersion) rqvs)
            reasons ->
                do ePutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                ePutStr (" Version >= " ++ show (prettyDebianVersion v) ++ " is required" ++ maybe "" ((++) ":") s)

      doVerifyBuildRepo :: IO ()
      doVerifyBuildRepo =
          when (not (any (== (P.buildRelease params)) (concatMap (uncurry sliceReleaseNames) (slices (C.buildRepoSources cache)))))
               (case P.uploadURI params of
                  Just uri ->
                      let ssh = case uriAuthority uri of
                                  Just auth -> uriUserInfo auth ++ uriRegName auth ++ uriPort auth
                                  Nothing -> "user@hostname"
                          rel = releaseName' (P.buildRelease params)
                          top = uriPath uri in -- "/home/autobuilder/deb-private/debian"
                      error $ "Build repository does not exist on remote server: " ++ rel ++ "\nUse newdist there to create it:" ++
                              "\n  ssh " ++ ssh ++ " " ++ P.newDistProgram params ++ " --root=" ++ top ++ " --create-release=" ++ rel ++
                              "\n  ssh " ++ ssh ++ " " ++ P.newDistProgram params ++ " --root=" ++ top ++ " --create-section=" ++ rel ++ ",main" ++
                              "\nYou will also need to remove the local file ~/.autobuilder/repoCache.")
      doShowParams = ePutStr $ "Configuration parameters:\n" ++ P.prettyPrint params
      doShowSources =
          either (error . show) doShow (P.findSlice cache (SliceName (releaseName' (P.buildRelease params))))
          where
            doShow sources =
                do qPutStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   qPutStrLn . show . pretty . sliceList $ sources
                   exitWith ExitSuccess
      doFlush top =
          do qPutStrLn "Flushing cache"
             removeRecursiveSafely top
             createDirectoryIfMissing True top
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do qPutStr "You must be superuser to run the autobuilder (to use chroot environments.)"
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          P.localPoolDir cache >>= \ dir ->
          (\ x -> qPutStrLn ("Preparing local repository " ++ dir) >> quieter 1 x) $
          do let path = EnvPath (EnvRoot "") dir
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if P.flushPool params then flushLocalRepository else return)
             qPutStrLn $ "Preparing release main in local repository at " ++ outsidePath path
             release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
             case P.cleanUp params of
               True -> deleteGarbage repo
               False -> return repo
      retrieveTargetList :: MonadDeb m => OSImage -> m [Either String Buildable]
      retrieveTargetList dependOS =
          retrieveTargetList' dependOS >>=
          mapM (either (return . Left) (\ download -> liftIO (try (asBuildable download)) >>= return. either (\ (e :: SomeException) -> Left (show e)) Right))
      retrieveTargetList' :: MonadDeb m => OSImage -> m [Either String Download]
      retrieveTargetList' dependOS =
          do qPutStr ("\n" ++ showTargets allTargets ++ "\n")
             buildOS <- chrootEnv dependOS <$> buildEnv (P.buildRelease (C.params cache))
             when (P.report params) (ePutStrLn . doReport $ allTargets)
             qPutStrLn "Retrieving all source code:\n"
             countTasks' (map (\ (target :: P.Packages) ->
                                   (show (P.spec target), (Right <$> retrieve defaultAtoms buildOS cache target) `IO.catch` handleRetrieveException target))
                              (P.foldPackages (\ name spec flags l -> P.Package name spec flags : l) allTargets []))
          where
            allTargets = P.buildPackages (C.params cache)
            handleRetrieveException :: MonadDeb m => P.Packages -> SomeException -> m (Either String Download)
            handleRetrieveException target e =
                case (fromException (toException e) :: Maybe AsyncException) of
                  Just UserInterrupt ->
                      throw e -- break out of loop
                  _ -> let message = ("Failure retrieving " ++ show (P.spec target) ++ ":\n  " ++ show e) in
                       liftIO (IO.hPutStrLn IO.stderr message) >> return (Left message)
      upload :: MonadApt m => (LocalRepository, [Target]) -> m [Failing ([Output L.ByteString], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> liftIO (uploadRemote repo uri)
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map (P.unTargetName . targetName) failed))
             qPutStrLn msg
             case P.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist :: [Failing ([Output L.ByteString], NominalDiffTime)] -> IO (Failing ([Output L.ByteString], NominalDiffTime))
      newDist _results
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ P.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . P.createRelease $ params)) in
                             qPutStrLn "Running newdist on remote repository" >>
                             try (timeTask (runProcessF (shell cmd) L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             qPutStr "Running newdist on a local repository" >>
                             try (timeTask (runProcessF (shell cmd) L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success ([], (fromInteger 0)))
      updateRepoCache :: MonadDeb m => m ()
      updateRepoCache =
          do path <- sub "repoCache"
             live <- getApt >>= return . getRepoMap
             repoCache <- liftIO $ loadCache path
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live repoCache
             liftIO (removeLink path `IO.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map RepoKey Repository)
            loadCache path =
                readFile path `IO.catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

-- | Perform a list of tasks with log messages.
countTasks' :: MonadIO m => [(String, m a)] -> m [a]
countTasks' tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          liftIO (IO.hPutStrLn IO.stderr (printf "[%2d of %2d] %s:" index count message)) >>
          task >>= \ a ->
          return a

doReport :: P.Packages -> String
doReport =
    intercalate "\n" . doReport'
    where
      doReport' :: P.Packages -> [String]
      doReport' P.NoPackage = []
      doReport' p@(P.Packages {}) = concatMap doReport' (P.list p)
      doReport' p@(P.Package {}) =
          patched (P.spec p) ++ pinned (P.flags p)
          where
            patched :: P.RetrieveMethod -> [String]
            patched (P.Patch _ _) = [P.unTargetName (P.name p) ++ " is patched"]
            patched (P.Cd _ x) = patched x
            patched (P.DataFiles x y _) = patched x ++ patched y
            patched (P.DebDir x y) = patched x ++ patched y
            patched (P.Debianize x) = patched x
            patched (P.Proc x) = patched x
            patched (P.Quilt x y) = patched x ++ patched y
            patched (P.SourceDeb x) = patched x
            patched (P.Twice x) = patched x
            patched _ = []
            pinned :: [P.PackageFlag] -> [String]
            pinned [] = []
            pinned (P.CabalPin v : more) = [P.unTargetName (P.name p) ++ " is pinned at version " ++ v] ++ pinned more
            pinned (_ : more) = pinned more
