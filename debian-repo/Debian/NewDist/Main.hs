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
import Debian.NewDist.Options (Params(..), homeParams, optSpecs)
import Debian.NewDist.Version (myVersion)
import Debian.Relation (BinPkgName)
import Debian.Release (ReleaseName, releaseName', parseReleaseName, Section, parseSection')
import Debian.Repo.Delete (deleteSourcePackages, deleteTrumped, deleteGarbage)
import Debian.Repo.Insert (scanIncoming, InstallResult, explainError, resultToProblems, showErrors)
import Debian.Repo.Monads.Apt (MonadApt, runAptIO)
import Debian.Repo.Release (findReleases, prepareRelease, signReleases, mergeReleases)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath, envPath)
import Debian.Repo.Types.PackageIndex (PackageIndex(PackageIndex), PackageID, makeBinaryPackageID)
import Debian.Repo.Types.Release (Release, parseArchitectures, releaseName, releaseAliases, releaseComponents, releaseArchitectures)
import Debian.Repo.Types.Repository (LocalRepository, Layout, repoRoot, prepareLocalRepository, setRepositoryCompatibility, fromLocalRepository)
import Debian.Version (parseDebianVersion, prettyDebianVersion)
import Extra.Email (sendEmails)
import Extra.GPGSign (PGPKey(Default, Key))
import Extra.Lock (withLock)
import Prelude hiding (putStr, putStrLn, putChar)
import System.Console.GetOpt (ArgOrder(Permute), getOpt, usageInfo)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
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
       quieter 1 (qPutStrLn ("Flags:\n  " ++ (show flags)))
       let lockPath = outsidePath (root flags) ++ "/newdist.lock"
       liftIO $ createDirectoryIfMissing True (outsidePath (root flags))
       case printVersion flags of
         False -> withLock lockPath (runAptIO (runFlags flags))
         True -> IO.putStrLn myVersion >> exitWith ExitSuccess

-- dry :: Params -> IO () -> IO ()
-- dry params action = if dryRun params then return () else action

runFlags :: MonadApt m => Params -> m ()
runFlags flags =
    do createReleases flags
       repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       rels <- findReleases repo
       _ <- liftIO (deletePackages (dryRun flags) repo rels flags keyname)
       liftIO $ setRepositoryCompatibility repo
       when (install flags)
                ((scanIncoming False keyname repo) >>=
                     \ (ok, errors) -> (liftIO (sendEmails senderAddr emailAddrs (map (successEmail repo) ok) >>
                                                sendEmails senderAddr emailAddrs (map (\ (changes, e) -> failureEmail changes e) errors) >>
                                                exitOnError (map snd errors))))
       when (expire flags)  $ liftIO (deleteTrumped (dryRun flags) keyname repo rels) >> return ()
       when (cleanUp flags) $ deleteGarbage repo >> return ()
       when (signRepo flags) $ liftIO (signReleases keyname (map (repo,) rels))
    where
      emailAddrs :: [(String, String)]
      emailAddrs =
          catMaybes . map parseEmail . concat . map (splitRegex (mkRegex "[ \t]*,[ \t]*")) . notifyEmail $ flags
      senderAddr :: (String, String)
      senderAddr = maybe ("autobuilder", "somewhere") id . maybe Nothing parseEmail . senderEmail $ flags
      successEmail :: LocalRepository -> ChangesFile -> (String, [String])
      successEmail repo changesFile =
          let subject = ("newdist: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++ 
                         " now available in " ++ releaseName' (changeRelease changesFile) ++
                         " (" ++ show (prettyArch (changeArch changesFile)) ++")")
              body = ("Repository " ++ envPath (repoRoot repo)) : [] : (lines $ show $ pretty $ changeInfo changesFile) in
    	  (subject, body)
      failureEmail :: ChangesFile -> InstallResult -> (String, [String])
      failureEmail changesFile e =
          let subject = ("newdist failure: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++ 
                         " failed to install in " ++ releaseName' (changeRelease changesFile))
              body = concat (map (lines . explainError) (resultToProblems e)) in
          (subject, body)
      keyname =
          case (keyName flags, signRepo flags) of
            (Just "none", _) -> Nothing
            (_, False) -> Nothing
            (Nothing, True) -> Just Extra.GPGSign.Default
            (Just x, True) -> Just (Extra.GPGSign.Key x)
      parseEmail s = case break (== '@') s of
                       (user, ('@' : host)) -> Just (user, host)
                       _ -> Nothing

-- | Make sure the debian releases which are referenced by the command
-- line flags exist.
createReleases :: MonadApt m => Params -> m ()
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
      createSection :: MonadApt m => LocalRepository -> Release -> Section -> m Release
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

createRelease :: MonadApt m => LocalRepository -> [Arch] -> ReleaseName -> m Release
createRelease repo archList' name =
    do rels <- findReleases repo
       case filter (\ release -> elem name (releaseName release : releaseAliases release)) rels of
         [] -> prepareRelease repo name [] [parseSection' "main"] archList'
         [release] -> return release
         _ -> error "Internal error 2"

createAlias :: MonadApt m => LocalRepository -> String -> m Release
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

exitOnError :: [InstallResult] -> IO ()
exitOnError [] = return ()
exitOnError errors =
    do qPutStrLn (showErrors errors)
       liftIO $ IO.hFlush IO.stderr
       liftIO $ exitWith (ExitFailure 1)

-- |Return the list of releases in the repository at root, creating
-- the ones in the dists list with the given components and
-- architectures.
getReleases :: MonadApt m => EnvPath -> Maybe Layout -> [ReleaseName] -> [Section] -> [Arch] -> m Release
getReleases root' layout' dists section' archList' =
    do repo <- prepareLocalRepository root' layout'
       existingReleases <- findReleases repo
       requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
       return $ mergeReleases (fromLocalRepository repo) (existingReleases ++ requiredReleases)

deletePackages :: Bool -> LocalRepository -> [Release] -> Params -> Maybe PGPKey -> IO [Release]
deletePackages dry repo rels flags keyname =
    deleteSourcePackages dry keyname repo toRemove
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
