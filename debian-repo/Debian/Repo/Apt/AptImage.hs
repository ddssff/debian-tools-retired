{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.Apt.AptImage
    ( prepareAptEnv
    , prepareOSEnv
    , updateOSEnv
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Exception as E (catch)
import Control.Monad.State (get, modify)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL, setL)
import Data.List (intercalate, sortBy)
import Data.List as List (map, partition)
import Data.Map as Map (insert, lookup)
import Data.Maybe (catMaybes)
import qualified Data.Text as T (Text, unpack)
import Debian.Arch (Arch, Arch(..), prettyArch)
import Debian.Control (ControlFunctions(stripWS), formatParagraph)
import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(lookupP), ControlFunctions(parseControlFromHandle), Field, Field'(Field), fieldValue, Paragraph)
import Debian.Relation (BinPkgName(BinPkgName))
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (ReleaseName(..), releaseName', sectionName')
import Debian.Repo.Apt.Slice (updateCacheSources, verifySourcesList)
import Debian.Repo.AptCache (AptCache(aptArch, rootDir), aptGetUpdate, buildArchOfRoot, distDir, SourcesChangedAction(SourcesChangedError), sourcesPath)
import Debian.Repo.AptImage (AptImage, aptImageBinaryPackages, aptImageSources, aptImageSourcePackages, prepareAptEnv'')
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.OSImage (_pbuilderBuild', aptGetInstall, buildEnv', localeGen, neuterEnv, osBaseDistro, osBinaryPackages, OSImage, osFullDistro, osLocalCopy, osLocalMaster, osRoot, osSourcePackages, prepareOSEnv', syncLocalPool, updateLists)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID, PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, BinaryPackage(..), PackageIndex(..), PackageIndex(packageIndexArch, packageIndexComponent), packageIndexPath, SourceControl(..), SourceFileSpec(SourceFileSpec), SourcePackage(..), SourcePackage(sourcePackageID))
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey, repoKeyURI)
import Debian.Repo.Repos (aptImageMap, binaryPackageMap, foldRepository, MonadRepos, sourcePackageMap)
import Debian.Repo.SSH (sshCopy)
import Debian.Repo.Slice (binarySlices, NamedSliceList(sliceListName, sliceList), Slice(sliceRepoKey, sliceSource), SliceList(slices), sourceSlices)
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Repo.Top (MonadTop)
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SourceType(..))
import Debian.URI (URI(uriScheme), uriToString')
import Debian.Version (parseDebianVersion)
import Extra.Files (replaceFile)
import Network.URI (escapeURIString, URI(uriAuthority, uriPath), URIAuth(uriPort, uriRegName, uriUserInfo))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (splitFileName, takeDirectory)
import qualified System.IO as IO (hClose, IOMode(ReadMode), openBinaryFile)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)
import System.Process (shell)
import System.Process.Progress (ePutStrLn, oneResult, qPutStrLn, quieter, readProcessChunks)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptEnv :: (MonadTop m, MonadRepos m) =>
                 SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptEnv sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (relName (sliceListName sources))) >> quieter 2 x) $
    get >>= return . Map.lookup (sliceListName sources) . getL aptImageMap >>=
    maybe (prepareAptEnv' sourcesChangedAction sources) return

prepareAptEnv' :: (MonadTop m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptEnv' sourcesChangedAction sources =
    do os <- prepareAptEnv'' sources
       os' <- updateCacheSources sourcesChangedAction os >>= updateAptEnv
       modify $ modL aptImageMap (Map.insert (sliceListName sources) os')
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: MonadRepos m => AptImage -> m AptImage
updateAptEnv os =
    do liftIO $ aptGetUpdate os
       sourcePackages <- getSourcePackages os >>= return . sortBy cmp
       binaryPackages <- getBinaryPackages os
       return $ setL aptImageSourcePackages sourcePackages $ setL aptImageBinaryPackages binaryPackages $ os
    where
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

getSourcePackages :: MonadRepos m => AptImage -> m [SourcePackage]
getSourcePackages os =
    do qPutStrLn "AptImage.getSourcePackages"
       indexes <- mapM (sliceIndexes os) (slices . sourceSlices . sliceList . getL aptImageSources $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages :: MonadRepos m => AptImage -> m [BinaryPackage]
getBinaryPackages os =
    do qPutStrLn "AptImage.getBinaryPackages"
       indexes <- mapM (sliceIndexes os) (slices . binarySlices . sliceList . getL aptImageSources $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

getSourcePackages' :: MonadRepos m => OSImage -> m [SourcePackage]
getSourcePackages' os =
    do indexes <- mapM (sliceIndexes os) (slices . sourceSlices . osFullDistro $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' os repo rel index) indexes >>= return . concat

getBinaryPackages' :: MonadRepos m => OSImage -> m [BinaryPackage]
getBinaryPackages' os =
    do indexes <- mapM (sliceIndexes os) (slices . binarySlices . osFullDistro $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' os repo rel index) indexes >>= return . concat

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: forall m a. (MonadRepos m, AptCache a) => a -> Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes cache slice =
    foldRepository f f (sliceRepoKey slice)
    where
      f :: Repo r => r -> m [(RepoKey, Release, PackageIndex)]
      f repo =
          case (sourceDist (sliceSource slice)) of
            Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
            Right (release, sections) -> return $ map (makeIndex repo release) sections
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

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, show (pretty l1), show (pretty l2)]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- |Create or update an OS image in which packages can be built.
prepareOSEnv :: (MonadRepos m, MonadTop m) =>
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call (unimplemented)
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ More packages to install, but these may not be available
                                        -- immediately - e.g seereason-keyring.  Ignore exceptions.
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m OSImage
prepareOSEnv root distro repo flush ifSourcesChanged include optional exclude components =
    do os <- prepareOSEnv' root distro repo
       os' <- update os
       arch <- liftIO buildArchOfRoot -- This should be stored in os, but it is a Maybe - why?
       os'' <- recreate arch os os'
       doInclude os''
       doLocales os''
       syncLocalPool os''
    where
      update _ | flush = return (Left Flushed)
      update os = updateOSEnv os
      recreate :: (MonadRepos m, MonadTop m) => Arch -> OSImage -> Either UpdateError OSImage -> m OSImage
      recreate _ _ (Right os) = return os
      recreate _arch _os (Left (Changed name path computed installed))
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       show (pretty computed) ++ "\ninstalled:\n" ++
                       show (pretty installed)
      recreate arch os (Left reason) =
          do dist <- distDir os
             sources <- sourcesPath os
             liftIO $ do ePutStrLn $ "Removing and recreating build environment at " ++ rootPath root ++ ": " ++ show reason
                         -- ePutStrLn ("removeRecursiveSafely " ++ rootPath root)
                         removeRecursiveSafely (rootPath root)
                         -- ePutStrLn ("createDirectoryIfMissing True " ++ show dist)
                         createDirectoryIfMissing True dist
                         -- ePutStrLn ("writeFile " ++ show sources ++ " " ++ show (show . osBaseDistro $ os))
                         replaceFile sources (show . pretty . getL osBaseDistro $ os)
             os' <- buildEnv root distro arch (getL osLocalMaster os) (getL osLocalCopy os) include exclude components
             liftIO $ do doLocales os'
                         neuterEnv os'
             syncLocalPool os'
      doInclude os = liftIO $
          do aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) include)
             aptGetInstall os (map (\ s -> (BinPkgName s, Nothing)) optional) `catchIOError` (\ e -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales os =
          do localeName <- liftIO (try (getEnv "LANG") :: IO (Either SomeException String))
             liftIO $ localeGen (either (const "en_US.UTF-8") id localeName) os

_pbuilderBuild :: (MonadRepos m, MonadTop m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
_pbuilderBuild root distro arch repo copy _extraEssential _omitEssential _extra =
    do os <- _pbuilderBuild' root distro arch repo copy _extraEssential _omitEssential _extra
       updateOSEnv os >>= either (error . show) return

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
buildEnv :: (MonadRepos m, MonadTop m) =>
            EnvRoot
         -> NamedSliceList
         -> Arch
         -> LocalRepository
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
buildEnv root distro arch repo copy include exclude components =
    quieter (-1) $
    do os <- buildEnv' root distro arch repo copy include exclude components
       updateOSEnv os >>= either (error . show) return

-- |Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateOSEnv :: MonadRepos m => OSImage -> m (Either UpdateError OSImage)
updateOSEnv os =
    do liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc")
       liftIO $ readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
       verified <- verifySources
       case verified of
         Left x -> return $ Left x
         Right _ ->
             do liftIO $ prepareDevs (rootPath root)
                os' <- syncLocalPool os
                _ <- liftIO $ updateLists os'
                _ <- liftIO $ sshCopy (rootPath root)
                source' <- getSourcePackages' os'
                binary <- getBinaryPackages' os'
                return . Right $ setL osSourcePackages source' $ setL osBinaryPackages binary $ os'
    where
      verifySources :: MonadRepos m => m (Either UpdateError OSImage)
      verifySources =
          do let computed = remoteOnly (osFullDistro os)
                 sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (sliceListName (getL osBaseDistro os)) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       return $ Left $ Changed (sliceListName (getL osBaseDistro os)) sourcesPath' computed installed'
               _ -> return $ Right os
      root = getL osRoot os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"

-- |Prepare a minimal \/dev directory
{-# WARNING prepareDevs "This function should check all the result codes" #-}
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev devices
  where
    devices :: [(FilePath, String, Int, Int)]
    devices = [(root ++ "/dev/null", "c", 1, 3),
               (root ++ "/dev/zero", "c", 1, 5),
               (root ++ "/dev/full", "c", 1, 7),
               (root ++ "/dev/console", "c", 5, 1),
               (root ++ "/dev/random", "c", 1, 8),
               (root ++ "/dev/urandom", "c", 1, 9)] ++
              (map (\ n -> (root ++ "/dev/loop" ++ show n, "b", 7, n)) [0..7]) ++
              (map (\ n -> (root ++ "/dev/loop/" ++ show n, "b", 7, n)) [0..7])
    prepareDev (path, typ, major, minor) = do
                     createDirectoryIfMissing True (fst (splitFileName path))
                     let cmd = "mknod " ++ path ++ " " ++ typ ++ " " ++ show major ++ " " ++ show minor ++ " 2> /dev/null"
                     exists <- doesFileExist path
                     case exists of
                       False -> readProcessChunks (shell cmd) L.empty >>= return . oneResult
                       True -> return ExitSuccess

-- FIXME: assuming the index is part of the cache
sourcePackagesOfIndex' :: (AptCache a, MonadRepos m) => a -> RepoKey -> Release -> PackageIndex -> m [SourcePackage]
sourcePackagesOfIndex' cache repo release index =
    do -- state <- getApt
       -- let cached = lookupSourcePackages path state <$> getApt
       cached <- (Map.lookup path . getL sourcePackageMap) <$> get
       status <- liftIO $ getFileStatus path `E.catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory (rootPath (rootDir cache)) ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toSourcePackage index) paragraphs
                 modify $ modL sourcePackageMap (Map.insert path (status, packages))
                 -- sourcePackageMap %= Map.insert path (status, packages)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . T.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case (parseSourcesFileList files, parseSourceParagraph package) of
            (Right files', Right para) ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID (T.unpack name) version
                , sourceParagraph = package
                , sourceControl = para
                , sourceDirectory = T.unpack directory
                , sourcePackageFiles = files' }
            (Left messages, _) -> error $ "Invalid file list: " ++ show messages
            (_, Left messages) -> error $ "Error in source paragraph\n package=" ++ show package ++ "\n  index=" ++ show index ++ "\n  messages:\n   " ++ intercalate "\n   " messages
      x -> error $ "Missing info in source package control information in " ++ show index ++ " -> " ++ show x ++ " :\n" ++ T.unpack (formatParagraph package)
    where
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: T.Text -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . List.map parseSourcesFiles . lines . T.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . List.map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . List.map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Either [String] SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    case (B.fieldValue "Package" p,
          B.fieldValue "Maintainer" p) of
      (Just source', Just maintainer') ->
          -- The optional fields can be parsed as pure values
          Right (SourceControl
                  { source = source'
                  , maintainer = maintainer'
                  , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
                  , packageSection = fmap stripWS $ B.fieldValue "Section" p
                  , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
                  , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
                  , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
                  , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
                  , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
                  , standardsVersion = fmap stripWS $ B.fieldValue "Standards-Version" p
                  , homepage = fmap stripWS $ B.fieldValue "Homepage" p })
      _x -> Left ["parseSourceParagraph - One or more required fields (Package, Maintainer, Standards-Version) missing: " ++ show p]

-- FIXME: assuming the index is part of the cache 
binaryPackagesOfIndex' :: (MonadRepos m, AptCache a) => a -> RepoKey -> Release -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfIndex' cache repo release index =
    do cached <- (Map.lookup path . getL binaryPackageMap) <$> get
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toBinaryPackage release index) paragraphs
                 modify $ modL binaryPackageMap (Map.insert path (status, packages))
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

toBinaryPackage :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage 
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)			-- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")	-- Debugging output
       return paragraphs

indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> FilePath
indexPrefix repo release index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distro = releaseName $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user'') Nothing (Just host) port' path' =
          user'' ++ host ++ port' ++ escape path'
      prefix "http:" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix "ftp:" _ _ (Just host) _ path' =
          host ++ escape path'
      prefix "file:" Nothing Nothing Nothing "" path' =
          escape path'
      prefix "ssh:" (Just user'') Nothing (Just host) port' path' =
          user'' ++ host ++ port' ++ escape path'
      prefix "ssh" _ _ (Just host) port' path' =
          host ++ port' ++ escape path'
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s', []) -> [s']
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b
