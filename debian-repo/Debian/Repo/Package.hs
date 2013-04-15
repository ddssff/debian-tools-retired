{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package
    ( -- * Source and binary packages 
      sourceFilePaths
    , binaryPackageSourceVersion
    , binarySourceVersion
    , sourcePackageBinaryNames
    , sourceBinaryNames
    , toSourcePackage
    , toBinaryPackage
    , binaryPackageSourceID
    , sourcePackageBinaryIDs
    , sourcePackagesOfIndex
    , sourcePackagesOfIndex'
    , binaryPackagesOfIndex
    , binaryPackagesOfIndex'
    , getPackages
    , putPackages
    , releaseSourcePackages
    , releaseBinaryPackages
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Control.Exception as E ( SomeException(..), catch, try, ErrorCall(..) )
import Data.Either ( partitionEithers )
import Debian.Apt.Index ( Compression(..), controlFromIndex )
import Debian.Arch (Arch(..), prettyArch)
import Debian.Control ( Paragraph', ControlFunctions(asString, stripWS), fieldValue, formatParagraph )
import Debian.Repo.PackageIndex ( packageIndexPath, sourceIndexList, binaryIndexList )
import qualified Debian.Control.ByteString as B ( Field'(Field), Paragraph, Field, Control'(Control), ControlFunctions(lookupP), fieldValue )
import Debian.Relation (BinPkgName(..))
import qualified Debian.Relation.ByteString as B ( ParseRelations(..), Relations )
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), lookupSourcePackages, insertSourcePackages, lookupBinaryPackages, insertBinaryPackages, readParagraphs)
import Debian.Release (releaseName', sectionName')
import Debian.Repo.Types ( AptCache(aptArch, rootDir), BinaryPackageLocal, SourceFileSpec(SourceFileSpec, sourceFileName), SourceControl(..), SourcePackage(..),
                           BinaryPackage(..), PackageID(..), makeSourcePackageID, makeBinaryPackageID, binaryPackageName, PackageIndexLocal, PackageIndex(..),
                           Release(releaseInfo, releaseRepo), ReleaseInfo(releaseInfoName), Repo(repoURI), LocalRepository(repoRoot), Repository(LocalRepo),
                           EnvRoot(rootPath), outsidePath )
import Debian.URI ( fileFromURIStrict )
import Debian.Version ( parseDebianVersion, DebianVersion )
import qualified Debian.Version as V ( buildDebianVersion, epoch, revision, version )
import "mtl" Control.Monad.Trans ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8 as L ( ByteString, fromChunks )
import qualified Data.ByteString.Char8 as B ( concat, ByteString, pack, unpack )
import Data.List ( intersperse, intercalate, partition )
import Data.Maybe ( catMaybes, fromMaybe )
import qualified Extra.Files as EF ( writeAndZipFileWithBackup )
import Network.URI ( URI(..), URIAuth(..), escapeURIString, uriToString )
import System.FilePath ((</>), takeDirectory)
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.Posix ( getFileStatus )
import Text.Regex ( matchRegex, mkRegex, splitRegex )

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

sourceFilePaths :: SourcePackage -> [FilePath]
sourceFilePaths package =
    map ((sourceDirectory package) </>) . map sourceFileName . sourcePackageFiles $ package

-- | Return the name and version number of the source package that
-- generated this binary package.  
binaryPackageSourceVersion :: BinaryPackage -> Maybe (String, DebianVersion)
binaryPackageSourceVersion package =
    let binaryName = binaryPackageName package
        binaryVersion = packageVersion . packageID $ package in
    binarySourceVersion' binaryName binaryVersion (packageInfo package)

-- |Return the name and version number of the source package that
-- generated this binary package.
-- see also: 'binaryPackageSourceVersion'
binarySourceVersion :: B.Paragraph -> Maybe ((BinPkgName, DebianVersion), (String, DebianVersion))
binarySourceVersion paragraph =
    let mBinaryName = fmap (BinPkgName . B.unpack) $ fieldValue "Package" paragraph
        mBinaryVersion = fmap (parseDebianVersion . B.unpack) $ fieldValue "Version" paragraph
    in
      case (mBinaryName, mBinaryVersion) of
        (Just binaryName, Just binaryVersion) ->
            fmap ((,) (binaryName, binaryVersion)) $ binarySourceVersion' binaryName binaryVersion paragraph
        _ -> Nothing

binarySourceVersion' :: (ControlFunctions a) => BinPkgName -> DebianVersion -> Paragraph' a -> Maybe (String, DebianVersion)
binarySourceVersion' binaryName binaryVersion paragraph =
    case (B.fieldValue "Source" paragraph) of
      Just source' ->
          case matchRegex re (asString source') of
            Just [name, _, ""] -> Just (name, binaryVersion)
            Just [name, _, version] -> Just (name, copyEpoch binaryVersion (parseDebianVersion version))
            _ -> error "internal error"
      Nothing ->
          Just (asString (unBinPkgName binaryName), binaryVersion)
    where
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"
      -- In the Packages file the version number in the Source: field has
      -- the epoch number stripped off.  I don't know why - I should search
      -- the Debian policy manual for this.  This puts it back on.
      copyEpoch src dst = V.buildDebianVersion (V.epoch src) (V.version dst) (V.revision dst)

sourcePackageBinaryNames :: SourcePackage -> [BinPkgName]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)

sourceBinaryNames :: B.Paragraph -> [BinPkgName]
sourceBinaryNames paragraph = 
    case B.fieldValue "Binary" paragraph of
      Just names -> map BinPkgName (splitRegex (mkRegex "[ ,\t\n]+") (B.unpack names))
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (B.unpack . formatParagraph $ paragraph))

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . B.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case parseSourcesFileList files of
            Right files' ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID index (B.unpack name) version
                , sourceParagraph = package
                , sourceControl = fromMaybe (error ("Failure parsing Source package control information: " ++ asString (formatParagraph package))) (parseSourceParagraph package)
                , sourceDirectory = B.unpack directory
                , sourcePackageFiles = files' }
            Left messages -> error $ "Invalid file list: " ++ show messages
      _ -> error $ "Missing info in source package control information:\n" ++ B.unpack (formatParagraph package)
    where      
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: B.ByteString -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . map parseSourcesFiles . lines . B.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Maybe SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    B.fieldValue "Source" p >>= \ source' ->
    B.fieldValue "Maintainer" p >>= \ maintainer' ->
    B.fieldValue "Standards-Version" p >>= \ standardsVersion' ->
    B.fieldValue "Homepage" p >>= \ homepage' ->
    -- The optional fields can be parsed as pure values
    return (SourceControl
            { source = source'
            , maintainer = maintainer'
            , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
            , packageSection = fmap stripWS $ B.fieldValue "Section" p
            , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
            , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
            , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
            , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
            , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
            , standardsVersion = standardsVersion'
            , homepage = homepage' })

toBinaryPackage :: PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage 
          { packageID =
                makeBinaryPackageID index (B.unpack name) (parseDebianVersion (B.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID :: BinaryPackage -> PackageID BinPkgName
binaryPackageSourceID package =
    case maybe Nothing (matchRegex re . B.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> makeBinaryPackageID sourceIndex name (packageVersion pid)
      Just [name, _, version] -> makeBinaryPackageID sourceIndex name (parseDebianVersion version)
      _ -> error "Missing Source attribute in binary package info"
    where
      sourceIndex = PackageIndex release component Source
      (PackageIndex release component _) = packageIndex pid
      pid = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryIDs :: Arch -> SourcePackage -> [PackageID BinPkgName]
sourcePackageBinaryIDs Source _ = error "invalid argument"
sourcePackageBinaryIDs arch package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> map (binaryID (parseDebianVersion (B.unpack version))) $ splitRegex (mkRegex "[ ,]+") (B.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (B.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = makeBinaryPackageID binaryIndex name version
      sourceIndex = packageIndex (sourcePackageID package)
      binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

-- | Get the contents of a package index
getPackages :: PackageIndex -> IO (Either SomeException [BinaryPackage])
getPackages index =
    liftIO ({- hPutStrLn stderr ("getPackages: index = " ++ show index) >> -}
            fileFromURIStrict uri' >>= return . either (Left . SomeException) (Right . toLazy) >>= {- showStream >>= -} readControl)
    where
      readControl :: Either SomeException L.ByteString -> IO (Either SomeException [BinaryPackage])
      readControl (Left e) = return (Left e)
      readControl (Right s) =
          try (case controlFromIndex Uncompressed (show uri') s of
                 Left e -> return $ Left (SomeException (ErrorCall (show uri' ++ ": " ++ show e)))
                 Right (B.Control control) -> return $ Right $ map (toBinaryPackage index) control) >>=
          return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id
      uri' = uri {uriPath = uriPath uri </> packageIndexPath index}
      uri = repoURI repo
      repo = releaseRepo release
      release = packageIndexRelease index
      toLazy s = L.fromChunks [s]
      --showStream :: Either Exception L.ByteString -> IO (Either Exception L.ByteString)
      --showStream x@(Left e) = hPutStrLn stderr (show uri' ++ " - exception: " ++ show e) >> return x
      --showStream x@(Right s) = hPutStrLn stderr (show uri' ++ " - stream length: " ++ show (L.length s)) >> return x

-- | Get the contents of a package index
binaryPackagesOfIndex :: PackageIndex -> IO (Either SomeException [BinaryPackage])
binaryPackagesOfIndex index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> getPackages index -- >>= return . either Left (Right . map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex :: PackageIndex -> IO (Either SomeException [SourcePackage])
sourcePackagesOfIndex index =
    case packageIndexArch index of
      Source -> getPackages index >>= return . either Left (Right . map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

-- FIXME: assuming the index is part of the cache 
sourcePackagesOfIndex' :: (AptCache a, MonadApt m) => a -> PackageIndex -> m [SourcePackage]
sourcePackagesOfIndex' cache index =
    do state <- getApt
       let cached = lookupSourcePackages path state
       status <- liftIO $ getFileStatus path `E.catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory (rootPath (rootDir cache)) ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = map (toSourcePackage index) paragraphs 
                 putApt (insertSourcePackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache index

indexCacheFile :: (AptCache a) => a -> PackageIndex -> FilePath
indexCacheFile apt index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: PackageIndex -> FilePath
indexPrefix index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      release = packageIndexRelease index
      section = packageIndexComponent index
      repo = releaseRepo release
      uri = repoURI repo
      distro = releaseInfoName . releaseInfo $ release
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
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoURI. releaseRepo . packageIndexRelease $ index))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s, []) -> [s]
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b

-- FIXME: assuming the index is part of the cache 
binaryPackagesOfIndex' :: (MonadApt m, AptCache a) => a -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfIndex' cache index =
    do state <- getApt
       let cached = lookupBinaryPackages path state
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = map (toBinaryPackage index) paragraphs
                 putApt (insertBinaryPackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache index

-- | Return a list of all source packages.
releaseSourcePackages :: Release -> IO (Either ErrorCall [SourcePackage])
releaseSourcePackages release =
    mapM sourcePackagesOfIndex (sourceIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Right (concat ok)
                  (bad, _) -> Left . ErrorCall $ intercalate ", " (map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages :: Release -> IO (Either ErrorCall [BinaryPackage])
releaseBinaryPackages release =
    mapM binaryPackagesOfIndex (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Right (concat ok)
                  (bad, _) -> Left . ErrorCall $ intercalate ", " (map show bad)

-- | Write a set of packages into a package index.
putPackages :: PackageIndexLocal ->  [BinaryPackageLocal] -> IO ()
putPackages index packages =
    case releaseRepo release of
      LocalRepo repo ->  EF.writeAndZipFileWithBackup (outsidePath (repoRoot repo) </> packageIndexPath index) text >>= either (fail . intercalate "\n") return
      x -> fail $ "Package.putPackages: Expected local repository, found " ++ show x
    where
      release = packageIndexRelease index
      --repo = releaseRepo release
      text = L.fromChunks [B.concat (intersperse (B.pack "\n") . map formatParagraph . map packageInfo $ packages)]
