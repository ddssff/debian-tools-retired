{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.LocalRepository
    ( LocalRepository(..)
    , Layout(..)
    , repoRoot
    , repoLayout
    , repoReleaseInfoLocal
    , poolDir'
    , readLocalRepo
    , prepareLocalRepository
    , copyLocalRepo -- repoCD
    , flushLocalRepository
    , setRepositoryCompatibility
    ) where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad (filterM, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.List (groupBy, isPrefixOf, partition, sort)
import Data.Maybe (catMaybes)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangedFileSpec(changedFileSection), ChangesFile(changeInfo))
import qualified Debian.Control.Text as T (Control'(Control), ControlFunctions(parseControl), fieldValue)
import Debian.Release (parseReleaseName, ReleaseName(..), releaseName', Section, sectionName', SubSection(section))
import qualified Debian.Repo.File as F (File(..), readFile)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types.EnvPath (EnvPath, outsidePath)
import Debian.Repo.Types.Release (makeReleaseInfo, Release)
import Debian.Repo.Types.Repo (compatibilityFile, libraryCompatibilityLevel, Repo(..), RepoKey(..))
import Extra.Files (maybeWriteFile)
import Extra.List (partitionM)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import qualified System.Posix.Files as F (fileMode, getFileStatus, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, setFileMode)
import System.Process.Progress (qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import Text.Regex (matchRegex, mkRegex)

data LocalRepository
    = LocalRepository
      { repoRoot_ :: EnvPath
      , repoLayout_ :: (Maybe Layout)
      , repoReleaseInfoLocal_ :: [Release]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show, Bounded, Enum)

repoRoot :: LocalRepository -> EnvPath
repoRoot = repoRoot_

repoLayout :: LocalRepository -> Maybe Layout
repoLayout = repoLayout_

repoReleaseInfoLocal :: LocalRepository -> [Release]
repoReleaseInfoLocal = repoReleaseInfoLocal_

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir r section source =
    case repoLayout r of
      Just Pool ->
          "pool/" ++ sectionName' section </> prefixDir </> source
              where prefixDir =
                        if isPrefixOf "lib" source
                        then take (min 4 (length source)) source
                        else take (min 1 (length source)) source
      _ -> ""

-- | Return the subdirectory in the pool where a source package would be
-- installed.
poolDir' :: LocalRepository -> ChangesFile -> ChangedFileSpec -> FilePath
poolDir' repo changes file =
    case T.fieldValue "Source" (changeInfo changes) of
      Nothing -> error "No 'Source' field in .changes file"
      Just source -> poolDir repo (section . changedFileSection $ file) (unpack source)

readLocalRepo :: MonadIO m => EnvPath -> Maybe Layout -> m LocalRepository
readLocalRepo root layout =
    do names <- liftIO (getDirectoryContents distDir) >>= return . filter (\ x -> not . elem x $ [".", ".."])
       (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
       linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
       let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
       let distGroups = groupBy fstEq . sort $ aliasPairs
       let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
       releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
       qPutStrLn ("LocalRepository releaseInfo " ++ show root ++ ": " ++ show releaseInfo)
       let repo = LocalRepository { repoRoot_ = root
                                  , repoLayout_ = layout
                                  , repoReleaseInfoLocal_ = releaseInfo }
       return repo
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (ReleaseName, [ReleaseName]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist ++ "/Release"
      distDir = outsidePath root ++ "/dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (F.readFile path) >>= return . parseRelease dist aliases

parseRelease :: ReleaseName -> [ReleaseName] -> F.File Text -> Release
parseRelease name aliases file =
    case F.text file of
      Failure msgs -> error $ "Could not read " ++ show (F.path file) ++ ": " ++ show msgs
      Success t ->
          case T.parseControl (show (F.path file)) t of
            Left msg -> error $ "Failure parsing " ++ show (F.path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (F.path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (F.File {F.path = F.path file, F.text = Success info}) name aliases

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: MonadIO m => EnvPath -> Maybe Layout -> m LocalRepository
prepareLocalRepository root layout =
    do mapM_ (liftIO . initDir)
                 [(".", 0o40755),
                  ("dists", 0o40755),
                  ("incoming", 0o41755),
                  ("removed", 0o40750),
                  ("reject", 0o40750)]
       layout' <- liftIO (computeLayout (outsidePath root)) >>= return . maybe layout Just
                  -- >>= return . maybe (maybe (error "No layout specified for new repository") id layout) id
       mapM_ (liftIO . initDir)
                 (case layout' of
                    Just Pool -> [("pool", 0o40755), ("installed", 0o40755)]
                    Just Flat -> []
                    Nothing -> [])
       readLocalRepo root layout'
    where
      initDir (name, mode) =
          do let path = outsidePath root </> name
             filterM (\ f -> doesDirectoryExist f >>= return . not) [path] >>=
                     mapM_ (\ f -> createDirectoryIfMissing True f)
             actualMode <- F.getFileStatus path >>= return . F.fileMode
             when (mode /= actualMode) (F.setFileMode path mode)
{-      notSymbolicLink root name =
          getSymbolicLinkStatus (root ++ "/dists/" ++ name) >>= return . not . isSymbolicLink
      hasReleaseFile root name =
          doesFileExist (root ++ "/dists/" ++ name ++ "/Release") -}

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
-- repoCD :: EnvPath -> LocalRepository -> LocalRepository
-- repoCD path repo = repo { repoRoot_ = path }

copyLocalRepo :: MonadIO m => EnvPath -> LocalRepository -> m LocalRepository
copyLocalRepo dest repo =
    do qPutStrLn ("Syncing local repository from " ++ src ++ " -> " ++ dst)
       liftIO $ createDirectoryIfMissing True (outsidePath dest)
       result <- liftIO $ rsync [] (outsidePath (repoRoot repo)) (outsidePath dest)
       case result of
         ExitSuccess -> return $ repo {repoRoot_ = dest}
         code -> error $ "*** FAILURE syncing local repository " ++ src ++ " -> " ++ dst ++ ": " ++ show code
    where
      src = outsidePath (repoRoot repo)
      dst = outsidePath dest

-- |Try to determine a repository's layout.
computeLayout :: FilePath -> IO (Maybe Layout)
computeLayout root =
    do
      -- If there are already .dsc files in the root directory
      -- the repository layout is Flat.
      isFlat <- getDirectoryContents root >>= return . (/= []) . catMaybes . map (matchRegex (mkRegex "\\.dsc$"))
      -- If the pool directory already exists the repository layout is
      -- Pool.
      isPool <- doesDirectoryExist (root ++ "/pool")
      case (isFlat, isPool) of
        (True, _) -> return (Just Flat)
        (False, True) -> return (Just Pool)
        _ -> return Nothing

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: MonadIO m => LocalRepository -> m LocalRepository
flushLocalRepository r =
    do liftIO $ removeRecursiveSafely (outsidePath (repoRoot r))
       prepareLocalRepository (repoRoot r) (repoLayout r)

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility r =
    maybeWriteFile path (show libraryCompatibilityLevel ++ "\n")
    where path = outsidePath (repoRoot r) </> compatibilityFile
