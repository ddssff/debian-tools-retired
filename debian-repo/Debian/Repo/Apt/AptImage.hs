{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.Apt.AptImage
    ( evalMonadApt
    , execMonadApt
    , runMonadApt
    , withAptImage
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (MonadCatchIO(catch))
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans (liftIO, MonadIO, lift)
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
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (ReleaseName(..), releaseName', sectionName')
import Debian.Repo.Apt.Slice (updateCacheSources)
import Debian.Repo.AptCache (aptGetUpdate, MonadCache(aptArch, rootDir), SourcesChangedAction)
import Debian.Repo.AptImage (AptImage, getApt, modifyApt, aptImageBinaryPackages, aptImageSourcePackages, aptImageSources, MonadApt(putApt, getApt), createAptImage, aptImageRoot, cacheRootDir)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.OSImage (OSImage)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID, PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, BinaryPackage(..), PackageIndex(..), PackageIndex(packageIndexArch, packageIndexComponent), packageIndexPath, SourceControl(..), SourceFileSpec(SourceFileSpec), SourcePackage(..), SourcePackage(sourcePackageID))
import Debian.Repo.Prelude (access, (~=))
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey, repoKeyURI)
import Debian.Repo.Repos (aptImageMap, binaryPackageMap, foldRepository, getRepos, modifyRepos, MonadRepos, sourcePackageMap, findAptImage)
import Debian.Repo.Slice (binarySlices, NamedSliceList(sliceListName, sliceList), Slice(sliceRepoKey, sliceSource), SliceList(slices), sourceSlices)
import Debian.Repo.Top (MonadTop)
import Debian.Sources (DebSource(sourceDist, sourceType), SourceType(Deb, DebSrc))
import Debian.URI (URI(uriScheme), uriToString')
import Debian.Version (parseDebianVersion)
import Network.URI (escapeURIString, URI(uriAuthority, uriPath), URIAuth(uriPort, uriRegName, uriUserInfo))
import System.FilePath (takeDirectory)
import qualified System.IO as IO (hClose, IOMode(ReadMode), openBinaryFile)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)
import System.Process.Progress (qPutStrLn, quieter)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance MonadApt m => MonadApt (StateT OSImage m) where
    getApt = lift getApt
    putApt = lift . putApt

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m a
evalMonadApt task apt = fst <$> runMonadApt task apt

-- | Run MonadOS and update the osImageMap with the modified value
execMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m AptImage
execMonadApt task apt = snd <$> runMonadApt task apt

-- | Run MonadOS and update the osImageMap with the modified value
runMonadApt :: (MonadRepos m, Functor m) => StateT AptImage m a -> AptImage -> m (a, AptImage)
runMonadApt task apt = do
  (a, apt') <- runStateT task apt
  modifyRepos (modL aptImageMap (Map.insert (getL aptImageRoot apt') apt'))
  return (a, apt')

withAptImage :: (MonadRepos m, MonadTop m) => SourcesChangedAction -> NamedSliceList -> StateT AptImage m a -> m a
withAptImage sourcesChangedAction sources action = prepareAptImage sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage :: (MonadTop m, MonadRepos m) =>
                 SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptImage		-- The resulting environment
prepareAptImage sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (relName (sliceListName sources))) >> quieter 2 x) $
    cacheRootDir (sliceListName sources) >>= \ root ->
    getRepos >>= return . Map.lookup root . getL aptImageMap >>=
    maybe (prepareAptImage' sourcesChangedAction sources) return

prepareAptImage' :: (MonadTop m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptImage' sourcesChangedAction sources =
    cacheRootDir (sliceListName sources) >>= \ root ->
    findAptImage root >>=
    maybe (createAptImage sources >>= execMonadApt (updateCacheSources sourcesChangedAction >> updateAptEnv)) return

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
updateAptEnv :: (MonadRepos m, MonadApt m, MonadCache m) => m ()
updateAptEnv =
    do aptGetUpdate
       sourcePackages <- getSourcePackages >>= return . sortBy cmp
       binaryPackages <- getBinaryPackages
       modifyApt (setL aptImageSourcePackages sourcePackages)
       modifyApt (setL aptImageBinaryPackages binaryPackages)
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

getSourcePackages :: (MonadRepos m, MonadApt m, MonadCache m) => m [SourcePackage]
getSourcePackages =
    do qPutStrLn "AptImage.getSourcePackages"
       indexes <- (slices . sourceSlices . sliceList . getL aptImageSources <$> getApt) >>= mapM sliceIndexes >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfIndex' repo rel index) indexes >>= return . concat

getBinaryPackages :: (MonadRepos m, MonadApt m, MonadCache m) => m [BinaryPackage]
getBinaryPackages =
    do qPutStrLn "AptImage.getBinaryPackages"
       indexes <- (slices . binarySlices . sliceList . getL aptImageSources <$> getApt) >>= mapM sliceIndexes >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfIndex' repo rel index) indexes >>= return . concat

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: forall m. (MonadRepos m, MonadCache m) => Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes slice =
    foldRepository f f (sliceRepoKey slice)
    where
      f :: Repo r => r -> m [(RepoKey, Release, PackageIndex)]
      f repo =
          case (sourceDist (sliceSource slice)) of
            Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
            Right (release, sections) -> aptArch >>= \ arch -> return $ map (makeIndex arch repo release) sections
      makeIndex arch repo release section =
          (repoKey repo,
           findReleaseInfo repo release,
           PackageIndex { packageIndexComponent = section
                        , packageIndexArch = case (sourceType (sliceSource slice)) of
                                               DebSrc -> Source
                                               Deb -> arch })
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

-- FIXME: assuming the index is part of the cache
sourcePackagesOfIndex' :: (MonadCache m, MonadRepos m) => RepoKey -> Release -> PackageIndex -> m [SourcePackage]
sourcePackagesOfIndex' repo release index =
    do -- state <- getApt
       -- let cached = lookupSourcePackages path state <$> getApt
       root <- rootPath <$> rootDir
       suff <- indexCacheFile repo release index
       let path = root ++ suff
       cached <- (Map.lookup path . getL sourcePackageMap) <$> getRepos
       status <- liftIO $ getFileStatus path `catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory root ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toSourcePackage index) paragraphs
                 modifyRepos $ modL sourcePackageMap (Map.insert path (status, packages))
                 -- sourcePackageMap %= Map.insert path (status, packages)
                 return packages

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
binaryPackagesOfIndex' :: (MonadRepos m, MonadCache m) => RepoKey -> Release -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfIndex' repo release index =
    do root <- rootPath <$> rootDir
       suff <- indexCacheFile repo release index
       let path = root ++ suff
       cached <- (Map.lookup path . getL binaryPackageMap) <$> getRepos
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toBinaryPackage release index) paragraphs
                 modifyRepos $ modL binaryPackageMap (Map.insert path (status, packages))
                 return packages

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

indexCacheFile :: (MonadCache m) => RepoKey -> Release -> PackageIndex -> m FilePath
indexCacheFile repo release index =
    do arch <- aptArch
       case (arch, packageIndexArch index) of
         (Binary _ _, Source) -> return $ indexPrefix repo release index ++ "_source_Sources"
         (Binary _ _, indexArch@(Binary _ _)) -> return $ indexPrefix repo release index ++ "_binary-" ++ show (prettyArch indexArch) ++ "_Packages"
         (x, _) -> error $ "Invalid build architecture: " ++ show x

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
