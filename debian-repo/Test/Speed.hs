{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Exception (SomeException)
import Control.Monad.Trans (liftIO)
import Data.Either (partitionEithers)
import Data.List as List (intercalate, map)
import Data.Set as Set (Set, unions, toList, fromList, size)
import Debian.Arch (Arch(..), prettyArch)
import Debian.Repo (runAptIO, prepareLocalRepository, findReleases)
import Debian.Repo.PackageIndex ( packageIndexPath, sourceIndexList, binaryIndexList )
import Debian.Repo.Types (Layout(Pool), rootEnvPath, Release', PackageIndex(packageIndexArch),
                          BinaryPackage(..), SourcePackage)
import System.IO (hPutStrLn, stderr)

import Control.Exception as E ( SomeException(..), catch, try, ErrorCall(..) )
import Data.Either ( partitionEithers )
import Data.List as List (map)
import Data.Set as Set (Set, unions, fromList, map)
import qualified Data.Text as T (Text, unpack, concat, pack)
import Data.Text.Encoding (encodeUtf8)
import Debian.Apt.Index ( Compression(..), controlFromIndex )
import Debian.Arch (Arch(..), prettyArch)
import Debian.Control ( Paragraph', ControlFunctions(asString, stripWS))
import Debian.Control (formatParagraph)
import Debian.Repo.PackageIndex ( packageIndexPath, sourceIndexList, binaryIndexList )
import qualified Debian.Control.Text as B ( Field'(Field), Paragraph, Field, Control'(Control), ControlFunctions(lookupP), fieldValue )
import Debian.Relation (BinPkgName(..))
import qualified Debian.Relation.Text as B ( ParseRelations(..), Relations )
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), lookupSourcePackages, insertSourcePackages, lookupBinaryPackages, insertBinaryPackages, readParagraphs)
import Debian.Release (releaseName', sectionName')
import Debian.Repo.Types ( AptCache(aptArch, rootDir), BinaryPackageLocal, SourceFileSpec(SourceFileSpec, sourceFileName), SourceControl(..), SourcePackage(..),
                           BinaryPackage(..), PackageID(..), makeSourcePackageID, makeBinaryPackageID, binaryPackageName, PackageIndexLocal, PackageIndex(..),
                           Release', Release(releaseName), Repo(repoURI), LocalRepository(repoRoot), Repository(LocalRepo),
                           EnvRoot(rootPath), outsidePath )
import Debian.URI ( fileFromURIStrict )
import Debian.Version ( parseDebianVersion, DebianVersion )
import qualified Debian.Version as V ( buildDebianVersion, epoch, revision, version )
import "mtl" Control.Monad.Trans ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8 as L ( ByteString, fromChunks )
import Data.List ( intersperse, intercalate, partition )
import Data.Maybe ( catMaybes )
import qualified Extra.Files as EF ( writeAndZipFileWithBackup )
import Network.URI ( URI(..), URIAuth(..), escapeURIString, uriToString )
import System.FilePath ((</>), takeDirectory)
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.Posix ( getFileStatus )
import Text.Regex ( matchRegex, mkRegex, splitRegex )

deriving instance Show BinaryPackage

-- | How long to parse the files in a repository?
root = rootEnvPath "/srv/deb/ubuntu"

main = runAptIO $
    do repo <- prepareLocalRepository root (Just Pool)
       releases <- findReleases repo
       sources <- mapM (liftIO . releaseSourcePackages) releases >>= return . Set.unions
       binaries <- mapM (liftIO . releaseBinaryPackages) releases >>= return . Set.unions
       -- requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
       -- return $ mergeReleases (existingReleases ++ requiredReleases)
       liftIO (hPutStrLn stderr ("rels:\n " ++ intercalate "\n " (List.map (show . snd) releases) ++ "\n\n" ++
                                 "source packages:\n " ++ {-intercalate "\n " (List.map show (toList sources))-} show (size sources) ++ "\n\n" ++
                                 "binary packages:\n " ++ {-intercalate "\n " (List.map show (toList binaries))-} show (size binaries) ++ "\n"))

-- | Return a list of all source packages.
releaseSourcePackages :: Release' -> IO (Set SourcePackage)
releaseSourcePackages release =
    mapM (sourcePackagesOfIndex release) (sourceIndexList release) >>= return . test
    where
      test :: [Either SomeException [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages :: Release' -> IO (Set BinaryPackage)
releaseBinaryPackages release =
    mapM (binaryPackagesOfIndex release) (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Get the contents of a package index
sourcePackagesOfIndex :: Release' -> PackageIndex -> IO (Either SomeException [SourcePackage])
sourcePackagesOfIndex release index =
    case packageIndexArch index of
      Source -> getPackages release index >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

-- | Get the contents of a package index
binaryPackagesOfIndex :: Release' -> PackageIndex -> IO (Either SomeException [BinaryPackage])
binaryPackagesOfIndex release index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> getPackages release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
getPackages :: Release' -> PackageIndex -> IO (Either SomeException [BinaryPackage])
getPackages (repo, release) index =
    fileFromURIStrict uri' >>= return . either (Left . SomeException) Right >>= {- showStream >>= -} readControl
    where
      readControl :: Either SomeException L.ByteString -> IO (Either SomeException [BinaryPackage])
      readControl (Left e) = return (Left e)
      readControl (Right s) =
          try (case controlFromIndex Uncompressed (show uri') s of
                 Left e -> return $ Left (SomeException (ErrorCall (show uri' ++ ": " ++ show e)))
                 Right (B.Control control) -> return (Right $ List.map (toBinaryPackage (repo, release) index) control)) >>=
          return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id
      uri' = uri {uriPath = uriPath uri </> packageIndexPath (repo, release) index}
      uri = repoURI repo
      toLazy s = L.fromChunks [s]
      --showStream :: Either Exception L.ByteString -> IO (Either Exception L.ByteString)
      --showStream x@(Left e) = hPutStrLn stderr (show uri' ++ " - exception: " ++ show e) >> return x
      --showStream x@(Right s) = hPutStrLn stderr (show uri' ++ " - stream length: " ++ show (L.length s)) >> return x

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

toBinaryPackage :: Release' -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage 
          { packageID = makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
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
