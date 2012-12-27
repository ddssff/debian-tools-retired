{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |The AptImage object represents a partial OS image which is capable
-- of running apt-get, and thus obtaining repository info and source
-- code packages.
module Debian.Repo.AptImage 
    ( prepareAptEnv
    , updateAptEnv
    , aptGetSource
    ) where

import Debian.Changes ( ChangeLogEntry(logVersion) )
import Debian.Release (ReleaseName(..))
import Debian.Sources (SliceName(sliceName))
import Debian.Repo.Cache ( SourcesChangedAction, cacheRootDir, sliceIndexes, buildArchOfRoot, updateCacheSources )
import Debian.Repo.Package ( sourcePackagesOfIndex', binaryPackagesOfIndex' )
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), lookupAptImage, insertAptImage )
import Debian.Repo.Slice ( sourceSlices, binarySlices )
import Debian.Repo.SourceTree ( DebianBuildTree(debTree'), DebianSourceTree(tree'), SourceTree(dir'), findDebianBuildTrees, entry )
import Debian.Repo.Types ( AptImage(..), AptCache(..), SourcePackage(sourcePackageID), sourcePackageName, BinaryPackage, PackageID(packageVersion),
      NamedSliceList(sliceList, sliceListName), SliceList(slices), EnvRoot(..) )
import Debian.Relation ( PkgName(..), SrcPkgName(..) )
import Debian.Version ( DebianVersion, prettyDebianVersion )
import Control.Monad.Trans ( liftIO )
import qualified Data.ByteString.Lazy as L
import Data.Function ( on )
import Data.List ( intercalate, sortBy )
import Data.Maybe ( listToMaybe )
import Extra.Files ( writeFileIfMissing, replaceFile )
import System.Unix.Directory ( removeRecursiveSafely )
import System.Process (shell)
import System.Process.Progress (runProcessF, quieter, qPutStr, qPutStrLn)
import System.Directory ( createDirectoryIfMissing )
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance Show AptImage where
    show apt = "AptImage " ++ relName (aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = aptGlobalCacheDir
    rootDir = aptImageRoot
    aptArch = aptImageArch
    aptBaseSliceList = aptImageSliceList
    aptSourcePackages = aptImageSourcePackages
    aptBinaryPackages = aptImageBinaryPackages
    aptReleaseName = aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (aptImageReleaseName a) (aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ

prepareAptEnv :: MonadApt m =>
                 FilePath		-- Put environment in a subdirectory of this
              -> SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptEnv cacheDir sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (sliceName (sliceListName sources))) >> quieter 2 x) $
    getApt >>= return . lookupAptImage (sliceListName sources) >>=
    maybe (prepareAptEnv' cacheDir sourcesChangedAction sources) return

-- |Create a skeletal enviroment sufficient to run apt-get.
{-# NOINLINE prepareAptEnv' #-}
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
       getApt >>= putApt . insertAptImage (sliceListName sources) os'
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: MonadApt m => AptImage -> m AptImage
updateAptEnv os =
    liftIO (runProcessF (shell cmd) L.empty) >>
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
    mapM (sourcePackagesOfIndex' os) indexes >>= return . concat
    where
      indexes = concat . map (sliceIndexes os) . slices . sourceSlices . aptImageSliceList $ os

getBinaryPackages :: MonadApt m => AptImage -> m [BinaryPackage]
getBinaryPackages os =
    mapM (binaryPackagesOfIndex' os) indexes >>= return . concat
    where
      indexes = concat . map (sliceIndexes os) . slices . binarySlices . aptImageSliceList $ os

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
       let newest = listToMaybe . map (packageVersion . sourcePackageID) . filter ((== package) . sourcePackageName) . aptSourcePackages $ os
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
{-
    where
      _availableStyle = setStyle (setStart (Just ("Finding available versions of " ++ package ++ " in APT pool")))
      aptGetStyle = setStyle (setStart (Just ("Retrieving APT source for " ++ package)))
-}

-- | Note that apt-get source works for binary or source package names.
runAptGet :: (PkgName n, AptCache t) => t -> FilePath -> String -> [(n, Maybe DebianVersion)] -> IO ()
runAptGet os dir command packages =
    createDirectoryIfMissing True dir >> runProcessF (shell cmd) L.empty >> return ()
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
