{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.Apt.AptImage
    ( prepareAptEnv
    , aptGetSource
    ) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL)
import Data.List (intercalate, sortBy)
import Data.Map as Map (insert, lookup)
import Data.Maybe (listToMaybe)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Relation (PkgName(..), SrcPkgName(..))
import Debian.Release (ReleaseName(..))
import Debian.Repo.Apt (aptImageMap, MonadApt(getApt, putApt))
import Debian.Repo.Apt.Cache (sliceIndexes, updateCacheSources)
import Debian.Repo.Apt.Package (binaryPackagesOfIndex', sourcePackagesOfIndex')
import Debian.Repo.AptImage (AptCache(..), AptImage(..))
import Debian.Repo.Cache (buildArchOfRoot, cacheRootDir, SourcesChangedAction)
import Debian.Repo.EnvPath (EnvRoot(..))
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Slice (binarySlices, NamedSliceList(sliceList, sliceListName), SliceList(slices), sourceSlices)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), entry, findDebianBuildTrees, SourceTree(dir'))
import Debian.Sources (SliceName(sliceName))
import Debian.Version (DebianVersion, prettyDebianVersion)
import Extra.Files (replaceFile, writeFileIfMissing)
import System.Directory (createDirectoryIfMissing)
import System.Process (shell)
import System.Process.Progress (qPutStr, qPutStrLn, quieter, runProcessF)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptEnv :: MonadApt m =>
                 FilePath		-- Put environment in a subdirectory of this
              -> SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptEnv cacheDir sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (sliceName (sliceListName sources))) >> quieter 2 x) $
    getApt >>= return . Map.lookup (sliceListName sources) . getL aptImageMap >>=
    maybe (prepareAptEnv' cacheDir sourcesChangedAction sources) return

prepareAptEnv' :: MonadApt m => FilePath -> SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptEnv' cacheDir sourcesChangedAction sources =
    do let root = rootPath (cacheRootDir cacheDir (ReleaseName (sliceName (sliceListName sources))))
       --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/cache/apt/archives/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/dpkg")
       liftIO $ createDirectoryIfMissing True (root ++ "/etc/apt")
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/status") ""
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/diversions") ""
       -- We need to create the local pool before updating so the
       -- sources.list will be valid.
       let sourceListText = show (pretty (sliceList sources))
       -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
       liftIO $ replaceFile (root ++ "/etc/apt/sources.list") sourceListText
       arch <- liftIO $ buildArchOfRoot
       let os = AptImage { aptGlobalCacheDir = cacheDir
                         , aptImageRoot = EnvRoot root
                         , aptImageArch = arch
                         , aptImageReleaseName = ReleaseName . sliceName . sliceListName $ sources
                         , aptImageSliceList = sliceList sources
                         , aptImageSourcePackages = []
                         , aptImageBinaryPackages = [] }
       os' <- updateCacheSources sourcesChangedAction os >>= updateAptEnv
       getApt >>= putApt . modL aptImageMap (Map.insert (sliceListName sources) os')
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: MonadApt m => AptImage -> m AptImage
updateAptEnv os =
    liftIO (runProcessF (Just (" 1> ", " 2> ")) (shell cmd) L.empty) >>
    getSourcePackages os >>= return . sortBy cmp >>= \ sourcePackages ->
    getBinaryPackages os >>= \ binaryPackages ->
    return $ os { aptImageSourcePackages = sourcePackages
                , aptImageBinaryPackages = binaryPackages }
    where
      cmd = "apt-get" ++ aptOpts os ++ " update"
      -- Flip args to get newest version first
      cmp = flip (compare `on` (packageVersion . sourcePackageID))
{-
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

    putStrLn ("> " ++ cmd) >> system cmd >>= \ code ->
    case code of
      ExitSuccess -> return ()
      ExitFailure n -> error $ cmd ++ " -> ExitFailure " ++ show n
-}

getSourcePackages :: MonadApt m => AptImage -> m [SourcePackage]
getSourcePackages os =
    do qPutStrLn "AptImage.getSourcePackages"
       indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptImageSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages :: MonadApt m => AptImage -> m [BinaryPackage]
getBinaryPackages os =
    do qPutStrLn "AptImage.getBinaryPackages"
       indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptImageSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

-- |Retrieve a source package via apt-get.
aptGetSource :: (AptCache t)
             => FilePath			-- Where to put the source
             -> t				-- Where to apt-get from
             -> SrcPkgName			-- The name of the package
             -> Maybe DebianVersion		-- The desired version, if Nothing get newest
             -> IO DebianBuildTree		-- The resulting source tree
aptGetSource dir os package version =
    do liftIO $ createDirectoryIfMissing True dir
       ready <- findDebianBuildTrees dir
       let newest = listToMaybe . map (packageVersion . sourcePackageID) . filter ((== package) . packageName . sourcePackageID) . aptSourcePackages $ os
       let version' = maybe newest Just version
       case (version', ready) of
         (Nothing, _) ->
             fail $ "No available versions of " ++ unSrcPkgName package ++ " in " ++ rootPath (rootDir os)
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do runAptGet os dir "source" [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed in " ++ dir ++ " (1): trees=" ++ show (map (dir' . tree' . debTree') trees)
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                qPutStr $ "Retrieving APT source for " ++ unSrcPkgName package
                runAptGet os dir "source" [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed (2): trees=" ++ show (map (dir' . tree' . debTree') trees)

-- | Note that apt-get source works for binary or source package names.
runAptGet :: (PkgName n, AptCache t) => t -> FilePath -> String -> [(n, Maybe DebianVersion)] -> IO ()
runAptGet os dir command packages =
    createDirectoryIfMissing True dir >> runProcessF (Just (" 1> ", " 2> ")) (shell cmd) L.empty >> return ()
    where
      cmd = (intercalate " " ("cd" : dir : "&&" : "apt-get" : aptOpts os : command : map formatPackage packages))
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

aptOpts :: AptCache t => t -> String
aptOpts os =
    (" -o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status" ++
     " -o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists" ++
     " -o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives" ++
     " -o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list" ++
     " -o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d")
    where root = rootPath . rootDir $ os
