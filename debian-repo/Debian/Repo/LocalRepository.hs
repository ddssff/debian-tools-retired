{-# LANGUAGE PackageImports #-}
module Debian.Repo.LocalRepository where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, unpack)
import Debian.Arch (Arch, parseArch)
import qualified Debian.Control.Text as B ( Paragraph, ControlFunctions(parseControl), fieldValue )
import qualified Debian.Control.Text as S ( Control'(Control) )
import Debian.Release (Section(..), ReleaseName, parseReleaseName, releaseName', sectionName', parseSection')
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), insertRepository)
import Debian.Repo.Types (Release(..), EnvPath, outsidePath)
import Debian.Repo.Types.Repo (Repo(repoKey), compatibilityFile, libraryCompatibilityLevel)
import Debian.Repo.Types.Repository (Repository(LocalRepo), Layout(..), LocalRepository(..))
import Control.Applicative.Error ( Failing(Success, Failure) )
import Control.Monad ( filterM, when )
import Data.List ( isPrefixOf, groupBy, partition, sort )
import Data.Maybe ( catMaybes )
import Extra.Files ( maybeWriteFile )
import "Extra" Extra.List ( partitionM )
import System.FilePath ( (</>) )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents )
import qualified System.Posix.Files as F ( fileMode, getFileStatus, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, setFileMode )
import System.Unix.Directory ( removeRecursiveSafely )
import Text.Regex ( matchRegex, mkRegex, splitRegex )
import qualified Tmp.File as F ( File(..), readFile )

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility (LocalRepository root _ _) =
    maybeWriteFile path text
    where text = show libraryCompatibilityLevel ++ "\n"
          path = outsidePath root </> compatibilityFile

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir (LocalRepository _ (Just Pool) _) section source =
    "pool/" ++ sectionName' section </> prefixDir </> source
    where prefixDir =
              if isPrefixOf "lib" source then
                  take (min 4 (length source)) source else
                  take (min 1 (length source)) source
poolDir (LocalRepository _ _ _) _ _ = ""

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: MonadApt m => LocalRepository -> m LocalRepository
flushLocalRepository (LocalRepository path layout _) =
    do liftIO $ removeRecursiveSafely (outsidePath path)
       prepareLocalRepository path layout

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: MonadApt m => EnvPath -> Maybe Layout -> m LocalRepository
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

readLocalRepo :: MonadApt m => EnvPath -> Maybe Layout -> m LocalRepository
readLocalRepo root layout =
    do
      state <- getApt
      names <- liftIO (getDirectoryContents distDir) >>=
               return . filter (\ x -> not . elem x $ [".", ".."])
      (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
      linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
      let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
      let distGroups = groupBy fstEq . sort $ aliasPairs
      let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
      releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
      let repo = LocalRepository { repoRoot = root
                                 , repoLayout = layout
                                 , repoReleaseInfoLocal = releaseInfo }
      putApt (insertRepository (repoKey repo) (LocalRepo repo) state)
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

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (F.readFile path) >>= return . parseRelease dist aliases
{-
    do text <- liftIO (B.readFile path)
       return $ parseRelease path text dist aliases
-}

parseRelease :: ReleaseName -> [ReleaseName] -> F.File Text -> Release
parseRelease name aliases file =
    case F.text file of
      Failure msgs -> error $ "Could not read " ++ show (F.path file) ++ ": " ++ show msgs
      Success text ->
          case B.parseControl (show (F.path file)) text of
            Left msg -> error $ "Failure parsing " ++ show (F.path file) ++ ": " ++ show msg
            Right (S.Control []) -> error $ "Empty release file: " ++ show (F.path file)
            Right (S.Control (info : _)) -> makeReleaseInfo (F.File {F.path = F.path file, F.text = Success info}) name aliases

makeReleaseInfo :: F.File B.Paragraph -> ReleaseName -> [ReleaseName] -> Release
makeReleaseInfo file@(F.File {F.text = Failure msgs}) _name _aliases =
    error $ "Failure reading " ++ show (F.path file) ++ ": " ++ show msgs
makeReleaseInfo file@(F.File {F.text = Success info}) name aliases =
    case (B.fieldValue "Architectures" info, B.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          Release { releaseName = name
                  , releaseAliases = aliases
                  , releaseArchitectures = parseArchitectures archList
                  , releaseComponents = parseComponents compList }
      _ -> error $ "Missing Architectures or Components field in Release file " ++ show (F.path file)

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

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

parseArchitectures :: Text -> [Arch]
parseArchitectures archList =
    map parseArch . splitRegex re . unpack $ archList
    where
      re = mkRegex "[ ,]+"

parseComponents :: Text -> [Section]
parseComponents compList =
    map parseSection' . splitRegex re . unpack  $ compList
    where
      re = mkRegex "[ ,]+"
