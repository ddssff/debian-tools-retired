{-# LANGUAGE DeriveDataTypeable, PackageImports #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |An AptCache represents a local cache of a remote repository.  The
-- cached information is usually downloaded by running "apt-get
-- update", and appears in @\/var\/lib\/apt\/lists@.
module Debian.Repo.Apt.Cache
    ( sliceIndexes
    , updateCacheSources
    ) where

import "mtl" Control.Monad.Trans (MonadIO(..))
import Debian.Arch (Arch(Source))
import Debian.Release (ReleaseName(relName), releaseName')
import Debian.Repo.Apt (MonadApt, prepareRepository)
import Debian.Repo.Apt.Slice (verifySourcesList)
import Debian.Repo.AptImage (AptCache(aptArch, aptBaseSliceList, aptReleaseName))
import Debian.Repo.Cache (distDir, SourcesChangedAction(..), sourcesPath)
import Debian.Repo.PackageIndex (PackageIndex(..))
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (Repo(repoReleaseInfo), repoKey, RepoKey)
import Debian.Repo.Slice (Slice(..))
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Sources (DebSource(..), SourceType(..))
import Extra.Files (replaceFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.IO (hGetLine, stdin)
import System.Process.Progress (ePutStr, ePutStrLn, qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: (MonadApt m, AptCache a) => a -> Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes cache slice =
    prepareRepository (sliceRepoKey slice) >>= \ repo ->
    case (sourceDist (sliceSource slice)) of
      Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
      Right (release, sections) -> return $ map (makeIndex repo release) sections
    where
      makeIndex repo release section =
          (repoKey repo,
           findReleaseInfo repo release,
           PackageIndex { packageIndexComponent = section
                        , packageIndexArch = case (sourceType (sliceSource slice)) of
                                               DebSrc -> Source
                                               Deb -> aptArch cache })
      findReleaseInfo repo release =
          case filter ((==) release . releaseName) (repoReleaseInfo repo) of
            [x] -> x
            [] -> error $ ("sliceIndexes: Invalid release name: " ++ releaseName' release ++
                           "\n  You may need to remove ~/.autobuilder/repoCache." ++
                           "\n  Available: " ++ (show . map releaseName . repoReleaseInfo $ repo)) ++
                           "\n repoKey: " ++ show (repoKey repo) ++
                           "\n repoReleaseInfo: " ++ show (repoReleaseInfo repo) ++
                           "\n slice: " ++ show slice
            xs -> error $ "Internal error 5 - multiple releases named " ++ releaseName' release ++ "\n" ++ show xs

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.
updateCacheSources :: (MonadApt m, AptCache c) => SourcesChangedAction -> c -> m c
updateCacheSources sourcesChangedAction distro =
    -- (\ x -> qPutStrLn "Updating cache sources" >> quieter 2 x) $
    qPutStrLn "Updating cache sources" >>
    do
      let baseSources = aptBaseSliceList distro
      --let distro@(ReleaseCache _ dist _) = releaseFromConfig' top text
      let dir = Debian.Repo.Cache.distDir distro
      distExists <- liftIO $ doesFileExist (Debian.Repo.Cache.sourcesPath distro)
      case distExists of
        True ->
            do
	      fileSources <- liftIO (readFile (Debian.Repo.Cache.sourcesPath distro)) >>= verifySourcesList Nothing . parseSourcesList
	      case (fileSources == baseSources, sourcesChangedAction) of
	        (True, _) -> return ()
	        (False, SourcesChangedError) ->
                    do
                      ePutStrLn ("The sources.list in the existing '" ++ relName (aptReleaseName distro) ++
                                 "' build environment doesn't match the parameters passed to the autobuilder" ++
			         ":\n\n" ++ Debian.Repo.Cache.sourcesPath distro ++ ":\n\n" ++
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
                              liftIO $ replaceFile (Debian.Repo.Cache.sourcesPath distro) (show (pretty baseSources))
                        _ ->
                            error ("Please remove " ++ dir ++ " and restart.")
                (False, RemoveRelease) ->
                    do
                      ePutStrLn $ "Removing suspect environment: " ++ dir
                      liftIO $ removeRecursiveSafely dir
                      liftIO $ createDirectoryIfMissing True dir
                      liftIO $ replaceFile (Debian.Repo.Cache.sourcesPath distro) (show (pretty baseSources))
                (False, UpdateSources) ->
                    do
                      -- The sources.list has changed, but it should be
                      -- safe to update it.
                      ePutStrLn $ "Updating environment with new sources.list: " ++ dir
                      liftIO $ removeFile (Debian.Repo.Cache.sourcesPath distro)
                      liftIO $ replaceFile (Debian.Repo.Cache.sourcesPath distro) (show (pretty baseSources))
        False ->
	    do
              liftIO $ createDirectoryIfMissing True dir
	      liftIO $ replaceFile (Debian.Repo.Cache.sourcesPath distro) (show (pretty baseSources))
      return distro
