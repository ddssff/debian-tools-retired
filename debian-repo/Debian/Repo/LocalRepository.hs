{-# LANGUAGE PackageImports #-}
module Debian.Repo.LocalRepository where

import Control.Monad.Trans (liftIO)
import Debian.Release (Section(..), sectionName')
import Debian.Repo.Monads.Apt (MonadApt)
import Debian.Repo.Types (EnvPath, outsidePath)
import Debian.Repo.Types.Repo (compatibilityFile, libraryCompatibilityLevel)
import Debian.Repo.Types.Repository (Layout(..), LocalRepository(..), readLocalRepo)
import Control.Monad ( filterM, when )
import Data.List (isPrefixOf)
import Data.Maybe ( catMaybes )
import Extra.Files ( maybeWriteFile )
import System.FilePath ( (</>) )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents )
import qualified System.Posix.Files as F ( fileMode, getFileStatus, setFileMode )
import System.Unix.Directory ( removeRecursiveSafely )
import Text.Regex (matchRegex, mkRegex)

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility r =
    maybeWriteFile path text
    where text = show libraryCompatibilityLevel ++ "\n"
          path = outsidePath (repoRoot r) </> compatibilityFile

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

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: MonadApt m => LocalRepository -> m LocalRepository
flushLocalRepository r {-(LocalRepository path layout _)-} =
    do liftIO $ removeRecursiveSafely (outsidePath (repoRoot r))
       prepareLocalRepository (repoRoot r) (repoLayout r)

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
