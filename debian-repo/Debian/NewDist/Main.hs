{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
-- | Replacement for debpool.
module Main where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Debian.Arch (Arch(Binary, Source), ArchCPU(..), ArchOS(..), prettyArch)
import Debian.Changes (ChangesFile(..))
import Debian.Config (option)
import Debian.NewDist.Options (Params(install, rootParam, printVersion, layout, dryRun, binaryOrphans, cleanUp, signRepo, notifyEmail, senderEmail, keyName, aliases, sections, architectures, releases, expire, removePackages), homeParams, optSpecs)
import Debian.NewDist.Version (myVersion)
import Debian.Relation (BinPkgName)
import Debian.Release (ReleaseName, releaseName', parseReleaseName, Section, parseSection')
import Debian.Repo.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), envPath)
import Debian.Repo.LocalRepository (LocalRepository, Layout, repoRoot, prepareLocalRepository, setRepositoryCompatibility)
import Debian.Repo.PackageID (PackageID, makeBinaryPackageID)
import Debian.Repo.PackageIndex (PackageIndex(PackageIndex))
import Extra.Email (sendEmails)
import Debian.Repo.Prelude.GPGSign (PGPKey(Default, Key))
import Debian.Repo.Prelude.Lock (withLock)
import Debian.Repo.Release (Release, parseArchitectures, releaseName, releaseAliases, releaseComponents, releaseArchitectures)
import Debian.Repo.State (MonadRepos, runReposT)
import Debian.Repo.State.Package (scanIncoming, deleteSourcePackages, deleteTrumped, deleteBinaryOrphans, deleteGarbage, InstallResult(Ok), explainError, resultToProblems, showErrors, MonadInstall, evalInstall)
import Debian.Repo.State.Release (findReleases, prepareRelease, writeRelease, signRelease, mergeReleases)
import Debian.Version (parseDebianVersion, prettyDebianVersion)
import Prelude hiding (putStr, putStrLn, putChar)
import System.Console.GetOpt (ArgOrder(Permute), getOpt, usageInfo)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO as IO (putStrLn, hFlush, stderr)
import System.Process.Progress (quieter, qPutStrLn)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (mkRegex, splitRegex)

main :: IO ()
main =
    do args <- getArgs
       params <- homeParams
       flags <- case getOpt Permute (map option optSpecs) args of
                  (o, _n, []) -> return $ foldl (flip id) params o
                  (_, _, errs) -> error (concat errs ++ usageInfo "Usage:" (map option optSpecs))
       qPutStrLn ("Flags:\n  " ++ (show flags))
       let lockPath = rootParam flags </> "newdist.lock"
       liftIO $ createDirectoryIfMissing True (rootParam flags)
       case printVersion flags of
         False -> withLock lockPath (runReposT (runFlags flags))
         True -> IO.putStrLn myVersion >> exitWith ExitSuccess

-- dry :: Params -> IO () -> IO ()
-- dry params action = if dryRun params then return () else action

runFlags :: MonadRepos m => Params -> m ()
runFlags flags =
    do createReleases flags
       repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       evalInstall (do rels <- findReleases repo
                       _ <- deletePackages (dryRun flags) rels flags keyname
                       liftIO $ setRepositoryCompatibility repo
                       when (install flags) $ do
                         results <- scanIncoming False keyname repo
                         liftIO $ sendEmails senderAddr emailAddrs (map (email repo) results)
                         liftIO $ exitOnError results
                       when (expire flags)  $ deleteTrumped (dryRun flags) keyname rels >> return ()
                       when (binaryOrphans flags)  $ deleteBinaryOrphans (dryRun flags) keyname rels >> return ()
                       when (cleanUp flags) $ deleteGarbage >> return ()
                       when (signRepo flags) $ liftIO (mapM_ (\ rel -> writeRelease repo rel >>= signRelease keyname repo rel) rels))
         repo keyname
    where
      emailAddrs :: [(String, String)]
      emailAddrs =
          catMaybes . map parseEmail . concat . map (splitRegex (mkRegex "[ \t]*,[ \t]*")) . notifyEmail $ flags
      senderAddr :: (String, String)
      senderAddr = maybe ("autobuilder", "somewhere") id . maybe Nothing parseEmail . senderEmail $ flags
      email :: LocalRepository -> (ChangesFile, InstallResult) -> (String, [String])
      email repo (changesFile, Ok) =
          let subject = ("newdist: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++
                         " now available in " ++ releaseName' (changeRelease changesFile) ++
                         " (" ++ show (prettyArch (changeArch changesFile)) ++")")
              body = ("Repository " ++ envPath (repoRoot repo)) : [] : (lines $ show $ pretty $ changeInfo changesFile) in
    	  (subject, body)
      email _repo (changesFile, e) =
          let subject = ("newdist failure: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++
                         " failed to install in " ++ releaseName' (changeRelease changesFile))
              body = concat (map (lines . explainError) (resultToProblems e)) in
          (subject, body)
      keyname =
          case (keyName flags, signRepo flags) of
            (Just "none", _) -> Nothing
            (_, False) -> Nothing
            (Nothing, True) -> Just {-Debian.Repo.Prelude.GPGSign.-}Default
            (Just x, True) -> Just ({-Debian.Repo.Prelude.GPGSign.-}Key x)
      parseEmail s = case break (== '@') s of
                       (user, ('@' : host)) -> Just (user, host)
                       _ -> Nothing

-- | Make sure the debian releases which are referenced by the command
-- line flags exist.
createReleases :: MonadRepos m => Params -> m ()
createReleases flags =
    do repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       rels <- findReleases repo
       mapM_ (createRelease repo (archList flags)) (map parseReleaseName . releases $ flags)
       mapM_ (createAlias repo) (aliases flags)
       mapM_ (createSectionOfRelease repo rels) (sections flags)
    where
      createSectionOfRelease repo rels arg =
          case break (== ',') arg of
            (rel, ',' : sectName) ->
                case filter (\ release -> releaseName release == parseReleaseName rel) rels of
                  [release] -> createSection repo release (parseSection' sectName)
                  [] -> error $ "createReleases: Invalid release name: " ++ rel
                  _ -> error "Internal error 1"
            _ ->
                error $ "Invalid argument to --create-section: " ++ arg
      createSection :: MonadRepos m => LocalRepository -> Release -> Section -> m Release
      createSection repo release section' =
          case filter ((==) section') (releaseComponents release) of
            [] -> prepareRelease repo (releaseName release) (releaseAliases release)
                    (releaseComponents release ++ [section'])  (releaseArchitectures release)
            _ -> return release

root :: Params -> EnvPath
root flags = EnvPath (EnvRoot "") (rootParam flags)

archList :: Params -> [Arch]
archList flags = maybe defaultArchitectures (parseArchitectures . pack) $ architectures flags

defaultArchitectures :: [Arch]
defaultArchitectures = [Binary (ArchOS "linux") (ArchCPU "i386"), Binary (ArchOS "linux") (ArchCPU "amd64")]

createRelease :: MonadRepos m => LocalRepository -> [Arch] -> ReleaseName -> m Release
createRelease repo archList' name =
    do rels <- findReleases repo
       case filter (\ release -> elem name (releaseName release : releaseAliases release)) rels of
         [] -> prepareRelease repo name [] [parseSection' "main"] archList'
         [release] -> return release
         _ -> error "Internal error 2"

createAlias :: MonadRepos m => LocalRepository -> String -> m Release
createAlias repo arg =
    case break (== '=') arg of
      (rel, ('=' : alias)) ->
          do rels <- findReleases repo
             case filter ((==) (parseReleaseName rel) . releaseName) rels of
               [] -> error $ "Attempt to create alias in non-existant release: " ++ rel
               [release] -> 
                   case elem (parseReleaseName alias) (releaseAliases release) of
                     False -> prepareRelease repo (parseReleaseName rel) (releaseAliases release ++ [parseReleaseName alias])
                               (releaseComponents release) (releaseArchitectures release)
                     True -> return release
               _ -> error $ "Internal error 3"
      _ -> error $ "Invalid argument to --create-alias: " ++ arg

exitOnError :: [(ChangesFile, InstallResult)] -> IO ()
exitOnError results | any (not . (== Ok) . snd) results =
    do qPutStrLn (showErrors (map snd results))
       liftIO $ IO.hFlush IO.stderr
       liftIO $ exitWith (ExitFailure 1)
exitOnError _ = return ()

-- |Return the list of releases in the repository at root, creating
-- the ones in the dists list with the given components and
-- architectures.
getReleases :: MonadRepos m => EnvPath -> Maybe Layout -> [ReleaseName] -> [Section] -> [Arch] -> m Release
getReleases root' layout' dists section' archList' =
    do repo <- prepareLocalRepository root' layout'
       existingReleases <- findReleases repo
       requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
       return $ mergeReleases repo (existingReleases ++ requiredReleases)

deletePackages :: MonadInstall m => Bool -> [Release] -> Params -> Maybe PGPKey -> m [Release]
deletePackages dry rels flags keyname =
    deleteSourcePackages dry keyname toRemove
    where
      toRemove :: [(Release, PackageIndex, PackageID BinPkgName)]
      toRemove = map parsePackage $ removePackages flags
      parsePackage :: String -> (Release, PackageIndex, PackageID BinPkgName)
      -- Parse a string in the form <dist>,<packagename>=<versionnumber>
      parsePackage s =
          case splitRegex (mkRegex "[,=]") s of
            [dist, component, name, ver] ->
                maybe (error ("Can't find release: " ++ dist))
                      (\ release -> (release,
                                     PackageIndex (parseSection' component) Source,
                                     makeBinaryPackageID name (parseDebianVersion ver)))
                      (findReleaseByName (parseReleaseName dist))
            x -> error ("Invalid remove spec: " ++ show x)
      findReleaseByName :: ReleaseName -> Maybe Release
      findReleaseByName dist =
          case filter (\ rel -> releaseName rel == dist) rels of
            [] -> Nothing
            [release] -> (Just release)
            _ -> error ("Multiple releases with name " ++ releaseName' dist)
