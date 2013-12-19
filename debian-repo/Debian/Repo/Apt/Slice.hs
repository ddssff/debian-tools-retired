{-# LANGUAGE PackageImports, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Apt.Slice
    ( verifySourcesList
    , repoSources
    , updateCacheSources
    ) where

import Control.Exception (throw)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (toChunks)
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (pack, Text, unpack)
import Debian.Control (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Control.Text (decodeParagraph)
import Debian.Release (parseReleaseName, parseSection', ReleaseName(relName))
import Debian.Repo.Apt (MonadApt, prepareRemoteRepository)
import Debian.Repo.AptCache (AptCache(aptBaseSliceList, aptReleaseName), distDir, SourcesChangedAction(..), sourcesPath)
import Debian.Repo.EnvPath (EnvPath(..), EnvRoot(..))
import Debian.Repo.LocalRepository (prepareLocalRepository)
import Debian.Repo.Repo (repoKey)
import Debian.Repo.Slice (Slice(..), SliceList(..))
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Sources (DebSource(..), SourceType(Deb, DebSrc))
import Debian.URI (dirFromURI, fileFromURI)
import Extra.Files (replaceFile)
import Network.URI (URI(uriScheme, uriPath))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO (hGetLine, stdin)
import System.Process.Progress (ePutStr, ePutStrLn, qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (mkRegex, splitRegex)

-- |Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources :: MonadApt m => Maybe EnvRoot -> URI -> m SliceList
repoSources chroot uri =
    do dirs <- liftIO (uriSubdirs chroot (uri {uriPath = uriPath uri ++ "/dists/"}))
       releaseFiles <- mapM (liftIO . readRelease uri) dirs >>= return . catMaybes
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (Maybe EnvRoot) -> URI -> IO [Text]
uriSubdirs root uri =
    liftIO (dirFromURI uri') >>= either throw (return . map pack)
    where
      uri' = case uriScheme uri of
               "file:" -> uri {uriPath = maybe "" rootPath root ++ (uriPath uri)}
               _ -> uri

readRelease :: URI -> Text -> IO (Maybe (Paragraph' Text))
readRelease uri name =
    do output <- liftIO (fileFromURI uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri </> "dists" </> unpack name </> "Release"}

-- | Make sure all the required local and remote repository objects
-- used by a sources.list file are in our cache.
verifySourcesList :: MonadApt m => Maybe EnvRoot -> [DebSource] -> m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

verifyDebSource :: MonadApt m => Maybe EnvRoot -> DebSource -> m Slice
verifyDebSource chroot line =
    case uriScheme (sourceUri line) of
      "file:" -> prepareLocalRepository (EnvPath chroot' (uriPath (sourceUri line))) Nothing >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
      _ -> prepareRemoteRepository (sourceUri line) >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
    where
      chroot' = fromMaybe (EnvRoot "") chroot

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.
updateCacheSources :: (AptCache c, MonadApt m) => SourcesChangedAction -> c -> m c
updateCacheSources sourcesChangedAction distro =
    -- (\ x -> qPutStrLn "Updating cache sources" >> quieter 2 x) $
    qPutStrLn "Updating cache sources" >>
    do
      let baseSources = aptBaseSliceList distro
      --let distro@(ReleaseCache _ dist _) = releaseFromConfig' top text
      let dir = distDir distro
      distExists <- liftIO $ doesFileExist (sourcesPath distro)
      case distExists of
        True ->
            do
	      fileSources <- liftIO (readFile (sourcesPath distro)) >>= verifySourcesList Nothing . parseSourcesList
	      case (fileSources == baseSources, sourcesChangedAction) of
	        (True, _) -> return ()
	        (False, SourcesChangedError) ->
                    do
                      ePutStrLn ("The sources.list in the existing '" ++ relName (aptReleaseName distro) ++
                                 "' build environment doesn't match the parameters passed to the autobuilder" ++
			         ":\n\n" ++ sourcesPath distro ++ ":\n\n" ++
                                 show (pretty fileSources) ++
			         "\nRun-time parameters:\n\n" ++
                                 show (pretty baseSources) ++ "\n" ++
			         "It is likely that the build environment in\n" ++
                                 dir ++ " is invalid and should be rebuilt.")
                      ePutStr $ "Remove it and continue (or exit)?  [y/n]: "
                      result <- liftIO $ hGetLine stdin
                      case result of
                        ('y' : _) ->
                            do
                              liftIO $ removeRecursiveSafely dir
                              liftIO $ createDirectoryIfMissing True dir
                              liftIO $ replaceFile (sourcesPath distro) (show (pretty baseSources))
                        _ ->
                            error ("Please remove " ++ dir ++ " and restart.")
                (False, RemoveRelease) ->
                    do
                      ePutStrLn $ "Removing suspect environment: " ++ dir
                      liftIO $ removeRecursiveSafely dir
                      liftIO $ createDirectoryIfMissing True dir
                      liftIO $ replaceFile (sourcesPath distro) (show (pretty baseSources))
                (False, UpdateSources) ->
                    do
                      -- The sources.list has changed, but it should be
                      -- safe to update it.
                      ePutStrLn $ "Updating environment with new sources.list: " ++ dir
                      liftIO $ removeFile (sourcesPath distro)
                      liftIO $ replaceFile (sourcesPath distro) (show (pretty baseSources))
        False ->
	    do
              liftIO $ createDirectoryIfMissing True dir
	      liftIO $ replaceFile (sourcesPath distro) (show (pretty baseSources))
      return distro
