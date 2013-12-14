{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.DeepSeq (force)
import Control.Exception (SomeException)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
-- import qualified Data.ByteString.Char8 as B
import Data.Either (partitionEithers)
import Data.List as List (intercalate, map)
import Data.Set as Set (Set, unions, fromList, size)
import Debian.Arch (Arch(..))
import Debian.Repo (runAptIO, findReleases)
import Debian.Repo.PackageIndex ( packageIndexPath, sourceIndexList, binaryIndexList )
import Debian.Repo.Types (rootEnvPath, Release, PackageIndex(packageIndexArch),
                          BinaryPackage(..), SourcePackage)
import Debian.Repo.Types.EnvPath (EnvPath)
import Debian.Repo.Types.LocalRepository (Layout(Pool), prepareLocalRepository)
import System.IO (hPutStrLn, stderr)

import Control.Exception as E ( SomeException(..), try, ErrorCall(..) )
import qualified Data.Text as T (Text, unpack)
import Debian.Apt.Index ( Compression(..), controlFromIndex )
import Debian.Control (ControlFunctions(stripWS))
import Debian.Control (formatParagraph)
import qualified Debian.Control.Text as B ( Field'(Field), Paragraph, Field, Control'(Control), ControlFunctions(lookupP), fieldValue )
import qualified Debian.Relation.Text as B ( ParseRelations(..), Relations )
import Debian.Repo.Types (SourceFileSpec(SourceFileSpec), SourceControl(..), SourcePackage(..), makeSourcePackageID, makeBinaryPackageID)
import Debian.Repo.Types.Repo (repoURI)
import Debian.Repo.Types.Repository (Repository, fromLocalRepository)
import Debian.Version (parseDebianVersion)
-- import qualified Data.ByteString.Lazy.Char8 as L ( ByteString, fromChunks )
import Data.List (partition)
import Data.Maybe ( catMaybes )
import GHC.IO.Exception (IOErrorType(UserError), IOException)
import Network.URI (URI(..), URIAuth(..), uriToString)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Error (mkIOError)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import System.Process.Progress (quieter, qPutStrLn)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

deriving instance Show BinaryPackage

root :: EnvPath
root = rootEnvPath "/srv/deb/ubuntu"

-- | How long does it take to parse the files in a repository?
main :: IO ()
main = runAptIO $ quieter (- 3) $
    do repo <- prepareLocalRepository root (Just Pool)
       releases <- findReleases repo
       sources <- mapM (liftIO . releaseSourcePackages . (fromLocalRepository repo,)) releases >>= return . Set.unions
       binaries <- mapM (liftIO . releaseBinaryPackages . (fromLocalRepository repo,)) releases >>= return . Set.unions
       -- requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
       -- return $ mergeReleases (existingReleases ++ requiredReleases)
       liftIO (hPutStrLn stderr ("rels:\n " ++ intercalate "\n " (List.map show releases) ++ "\n\n" ++
                                 "source packages:\n " ++ {-intercalate "\n " (List.map show (toList sources))-} show (size sources) ++ "\n\n" ++
                                 "binary packages:\n " ++ {-intercalate "\n " (List.map show (toList binaries))-} show (size binaries) ++ "\n"))

-- | Return a list of all source packages.
releaseSourcePackages :: (Repository, Release) -> IO (Set SourcePackage)
releaseSourcePackages (repo, release) =
    mapM (sourcePackagesOfIndex (repo, release)) (sourceIndexList release) >>= return . test
    where
      test :: [Either SomeException [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages :: (Repository, Release) -> IO (Set BinaryPackage)
releaseBinaryPackages (repo, release) =
    mapM (binaryPackagesOfIndex (repo, release)) (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Get the contents of a package index
sourcePackagesOfIndex :: (Repository, Release) -> PackageIndex -> IO (Either SomeException [SourcePackage])
sourcePackagesOfIndex (repo, release) index =
    case packageIndexArch index of
      Source -> getPackages (repo, release) index >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

-- | Get the contents of a package index
binaryPackagesOfIndex :: (Repository, Release) -> PackageIndex -> IO (Either SomeException [BinaryPackage])
binaryPackagesOfIndex (repo, release) index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> getPackages (repo, release) index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
getPackages :: (Repository, Release) -> PackageIndex -> IO (Either SomeException [BinaryPackage])
getPackages (repo, release) index =
    qPutStrLn ("fileFromURIStrict " ++ show uri') >>
    fileFromURIStrict uri' >>= return . either (Left . SomeException) Right >>= {- showStream >>= -} readControl
    where
      readControl :: Either SomeException L.ByteString -> IO (Either SomeException [BinaryPackage])
      readControl (Left e) = return (Left e)
      readControl (Right s) =
          try (case controlFromIndex Uncompressed (show uri') (force s) of
                 Left e -> return $ Left (SomeException (ErrorCall (show uri' ++ ": " ++ show e)))
                 Right (B.Control control) -> return (Right $ List.map (toBinaryPackage (repo, release) index) control)) >>=
          return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id
      uri' = uri {uriPath = uriPath uri </> packageIndexPath release index}
      uri = repoURI repo

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

toBinaryPackage :: (Repository, Release) -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage (repo, release) index p =
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

fileFromURIStrict :: URI -> IO (Either IOException L.ByteString)
fileFromURIStrict uri =
    case uriScheme uri of
      "file:" -> try (L.readFile (uriPath uri))
      _ ->
          let (cmd, args) = case (uriScheme uri, uriAuthority uri) of
                              ("ssh:", Just auth) -> ("ssh", [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri])
                              _ -> ("curl", ["-s", "-g", uriToString' uri]) in
          do result <- try (readProcessWithExitCode cmd args L.empty)
             case result of
               Left e -> return (Left e)
               Right (ExitSuccess, out, _) -> return (Right out)
               Right (ExitFailure _, _, err) -> return (Left (mkIOError UserError (L.unpack err) Nothing Nothing))
