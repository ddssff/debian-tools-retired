{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository
    , LocalRepository(repoRoot, repoLayout, repoReleaseInfoLocal)
    , Layout(..)
    , MonadRepoCache(..)
    , loadRepoCache
    , saveRepoCache
    , fromLocalRepository
    , readLocalRepo
    , prepareLocalRepository
    , prepareRepository'
    , prepareRepository
    , setRepositoryCompatibility
    , flushLocalRepository
    , poolDir'
    , SliceList(..)
    , NamedSliceList(..)
    -- , sliceReleaseNames
    , repoReleaseNames
    ) where

import Control.Applicative.Error (Failing(Success, Failure), maybeRead)
import Control.Exception (ErrorCall(..), SomeException, toException, try, evaluate)
import Control.Monad (filterM, when, unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, throw)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (groupBy, partition, sort, isPrefixOf, intercalate)
import Data.Map as Map (Map, insertWith, lookup, insert, fromList, toList, union)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangesFile(changeInfo), ChangedFileSpec(changedFileSection))
import qualified Debian.Control.Text as T (ControlFunctions(parseControl), Control'(Control), fieldValue, Paragraph, Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Release (ReleaseName(..), releaseName', Section, sectionName', parseReleaseName, SubSection(section))
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.Types.Release (Release(releaseName), makeReleaseInfo)
import Debian.Repo.Types.Repo (Repo(..), RepoKey(..), compatibilityFile, libraryCompatibilityLevel)
import Debian.Sources ( SliceName(..), DebSource(..), SourceType(..) )
import Debian.URI (URI'(..), uriToString', URI(uriScheme, uriPath), dirFromURI, fileFromURI)
import Debian.UTF8 as Deb (decode)
import Extra.Files (maybeWriteFile)
import Extra.List (partitionM)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified System.Posix.Files as F (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink,
                                          fileMode, getFileStatus, setFileMode, removeLink)
import System.Process.Progress (quieter, qPutStr, qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import qualified Text.Format as F (Pretty(..))
import qualified Tmp.File as F ( File(..), readFile )
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat, text)
import Text.Regex (matchRegex, mkRegex)
import qualified Tmp.File as F (Source(RemotePath))

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
--data Repository = forall a. (Repo a) => Repository a
data Repository
    = LocalRepo LocalRepository
    | VerifiedRepo URI' [Release]
    | UnverifiedRepo URI'
    deriving (Show, Read)

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

-- | URI has a bogus show function, which we are using here.
instance F.Pretty URI' where
    pretty = text . show . unURI

instance F.Pretty Repository where
    pretty (LocalRepo r) = text $ outsidePath (repoRoot r)
    pretty (VerifiedRepo s _) = F.pretty s
    pretty (UnverifiedRepo s) = F.pretty s

instance Repo Repository where
    repoKey (LocalRepo (LocalRepository path _ _)) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoKey (VerifiedRepo uri _) = Remote uri
    repoKey (UnverifiedRepo uri) = Remote uri
    repoReleaseInfo (LocalRepo (LocalRepository _ _ info)) = info
    repoReleaseInfo (VerifiedRepo _ info) = info
    repoReleaseInfo (UnverifiedRepo _uri) = error "No release info for unverified repository"

data LocalRepository
    = LocalRepository
      { repoRoot :: EnvPath
      , repoLayout :: (Maybe Layout)
      , repoReleaseInfoLocal :: [Release]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show)

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

class MonadIO m => MonadRepoCache m where
    getRepoCache :: m (Map RepoKey Repository)
    putRepoCache :: Map RepoKey Repository -> m ()

fromLocalRepository :: LocalRepository -> Repository
fromLocalRepository = LocalRepo

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: (MonadRepoCache m, MonadTop m) => m ()
loadRepoCache =
    do repoCache <- sub "repoCache"
       qPutStrLn "Loading repo cache..."
       uris <- liftIO $ try (readFile repoCache) >>=
                        try . evaluate . either (\ (_ :: SomeException) -> []) read >>=
                        return . either (\ (_ :: SomeException) -> []) id
{-
       uris <- liftIO (try (readFile repoCache)) >>=
               return . evaluate . either (\ (_ :: SomeException) -> []) read >>=
               return . either (\ (_ :: SomeException) -> []) id :: IO [(URI', Repository)]
-}
       qPutStrLn $ "Loaded " ++ show (length uris) ++ " entries from the repo cache."
       putRepoCache (fromList (map fixURI uris))
    where
      -- fixURI (Remote s, x) = (fromJust (parseURI s), x)
      fixURI :: (URI', a) -> (RepoKey, a)
      fixURI (s, x) = (Remote s, x)

-- | Write the repo cache map into a file.
saveRepoCache :: (MonadIO m, MonadTop m, MonadRepoCache m) => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- getRepoCache
             repoCache <- liftIO $ loadCache path
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live repoCache
             liftIO (F.removeLink path `IO.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map RepoKey Repository)
            loadCache path =
                readFile path `IO.catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

readLocalRepo :: MonadRepoCache m => EnvPath -> Maybe Layout -> m LocalRepository
readLocalRepo root layout =
    do
      state <- getRepoCache
      names <- liftIO (getDirectoryContents distDir) >>=
               return . filter (\ x -> not . elem x $ [".", ".."])
      (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
      linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
      let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
      let distGroups = groupBy fstEq . sort $ aliasPairs
      let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
      releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
      let repo = LocalRepository { repoRoot = root
                                 , repoLayout = layout
                                 , repoReleaseInfoLocal = releaseInfo }
      putRepoCache (insertWith (\ _ x -> x) (repoKey repo) (LocalRepo repo) state)
      return repo
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (ReleaseName, [ReleaseName]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist ++ "/Release"
      distDir = outsidePath root ++ "/dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (F.readFile path) >>= return . parseRelease dist aliases
{-
    do text <- liftIO (B.readFile path)
       return $ parseRelease path text dist aliases
-}

parseRelease :: ReleaseName -> [ReleaseName] -> F.File Text -> Release
parseRelease name aliases file =
    case F.text file of
      Failure msgs -> error $ "Could not read " ++ show (F.path file) ++ ": " ++ show msgs
      Success text ->
          case T.parseControl (show (F.path file)) text of
            Left msg -> error $ "Failure parsing " ++ show (F.path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (F.path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (F.File {F.path = F.path file, F.text = Success info}) name aliases

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: MonadRepoCache m => EnvPath -> Maybe Layout -> m LocalRepository
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

-- | Return the subdirectory in the pool where a source package would be
-- installed.
poolDir' :: LocalRepository -> ChangesFile -> ChangedFileSpec -> FilePath
poolDir' repo changes file =
    case T.fieldValue "Source" (changeInfo changes) of
      Nothing -> error "No 'Source' field in .changes file"
      Just source -> poolDir repo (section . changedFileSection $ file) (unpack source)

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: MonadRepoCache m => LocalRepository -> m LocalRepository
flushLocalRepository r {-(LocalRepository path layout _)-} =
    do liftIO $ removeRecursiveSafely (outsidePath (repoRoot r))
       prepareLocalRepository (repoRoot r) (repoLayout r)

{-
prepareRepository' :: MonadApt m => Maybe EnvRoot -> URI -> m Repository
prepareRepository' chroot uri =
    case uriScheme uri of
      "file:" ->
          let dir = EnvPath (maybe (EnvRoot "") id chroot) (uriPath uri) in
          prepareLocalRepository dir Nothing >>= return . LocalRepo
      _ ->
          prepareRepository uri
-}

prepareRepository' :: MonadRepoCache m => RepoKey -> m Repository
prepareRepository' key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= return . LocalRepo
      Remote (URI' uri) -> prepareRepository uri

-- |This is a remote repository which we have queried to find out the
-- names, sections, and supported architectures of its releases.
--data VerifiedRepo = VerifiedRepo URI [ReleaseInfo]

{- instance Show VerifiedRepo where
    show (VerifiedRepo uri _) = "Verified Repository " ++ show uri -- ++ " " ++ show dists
instance Ord VerifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq VerifiedRepo where
    a == b = compare a b == EQ -}

-- |This is a repository whose structure we haven't examined 
-- to determine what release it contains.
--data UnverifiedRepo = UnverifiedRepo URI

{- instance Show UnverifiedRepo where
    show (UnverifiedRepo uri) = "Unverified Repository " ++ show uri -- ++ " (unverified)"
instance Ord UnverifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq UnverifiedRepo where
    a == b = compare a b == EQ -}

-- | Prepare a repository, which may be remote or local depending on
-- the URI.
prepareRepository :: MonadRepoCache m => URI -> m Repository
prepareRepository uri =
    do state <- getRepoCache
       repo <- maybe newRepo return (Map.lookup (Remote (URI' uri)) state)
       putRepoCache (Map.insert (Remote (URI' uri)) repo state)
       return repo
    where
      newRepo =
             case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               -- FIXME: We only want to verifyRepository on demand.
               _ -> verifyRepository (UnverifiedRepo (URI' uri))
               -- _ -> return . Repository . UnverifiedRepo $ uri

{-# NOINLINE verifyRepository #-}
verifyRepository :: MonadIO m => Repository -> m Repository
verifyRepository (UnverifiedRepo uri) =
    do --tio (vHPutStrBl IO.stderr 0 $ "Verifying repository " ++ show uri ++ "...")
       -- Use unsafeInterleaveIO to avoid querying the repository
       -- until the value is actually needed.
       -- qPutStrLn $ "verifyRepository " ++ uri
       releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . unURI $ uri
       {- tio (vHPutStrLn IO.stderr 0 $ "\n" {- -> VerifiedRepo " ++ show uri ++ " " ++ show releaseInfo -} ) -}
       return $ VerifiedRepo uri releaseInfo
verifyRepository x = return x

-- Nice code to do caching, but I figured out how to fix the old code.

--instance Read URI where
--    readsPrec _ s = [(fromJust (parseURI s), "")]
--
---- |Get the list of releases of a remote repository.
--getReleaseInfo :: FilePath
--               -> Bool     -- ^ If False don't look at existing cache
--               -> URI
--               -> IO [ReleaseInfo]
--getReleaseInfo top tryCache uri =
--    readCache >>= \ cache ->
--    return (lookup uri cache) >>=
--    maybe (updateCache cache) return
--    where
--      cachePath = top ++ "/repoCache"
--      readCache :: IO [(URI, [ReleaseInfo])]
--      readCache = if tryCache
--                  then try (readFile cachePath >>= return . read) >>= return . either (\ (_ :: SomeException) -> []) id
--                  else return []
--      updateCache :: [(URI, [ReleaseInfo])] -> IO [ReleaseInfo]
--      updateCache pairs = getReleaseInfoRemote uri >>= \ info ->
--                          writeCache ((uri, info) : pairs) >> return info
--      writeCache :: [(URI, [ReleaseInfo])] -> IO ()
--      writeCache pairs = writeFile (show pairs) cachePath

-- |Get the list of releases of a remote repository.
getReleaseInfoRemote :: URI -> IO [Release]
getReleaseInfoRemote uri =
    qPutStr ("(verifying " ++ uriToString' uri ++ ".") >>
    quieter 2 (dirFromURI distsURI) >>=
    quieter 2 . either (error . show) verify >>=
    return . catMaybes >>= 
    (\ result -> qPutStr ")\n" >> return result)
    where
      distsURI = uri {uriPath = uriPath uri </> "dists/"}
      verify names =
          do let dists = map parseReleaseName names
             (releaseFiles :: [F.File (T.Paragraph' Text)]) <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap T.unpack (T.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe Text -> (F.File T.Paragraph) -> ReleaseName -> Maybe Release
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (T.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo info (parseReleaseName (T.unpack dist)) []
      getSuite :: F.File (T.Paragraph' Text) -> Maybe Text
      getSuite (F.File {F.text = Success releaseFile}) = T.fieldValue (releaseNameField releaseFile) releaseFile
      getSuite (F.File {F.text = Failure msgs}) = fail (intercalate "\n" msgs)
      getReleaseFile :: ReleaseName -> IO (F.File (T.Paragraph' Text))
      -- getReleaseFile :: ReleaseName -> IO (T.Paragraph' T.ByteString)
      getReleaseFile distName =
          do qPutStr "."
             release <- fileFromURI releaseURI
             let control = either Left (either (Left . toException . ErrorCall . show) Right . T.parseControl (show releaseURI) . Deb.decode) release
             case control of
               Right (T.Control [info :: T.Paragraph' Text]) -> return $ F.File {F.path = F.RemotePath releaseURI, F.text = Success info}
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [(Repository, DebSource)]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = vcat . map (pretty . snd) . slices

deriving instance Show SourceType
deriving instance Show DebSource

repoReleaseNames :: Repository -> [ReleaseName]
repoReleaseNames (VerifiedRepo _ rels) = map releaseName rels
repoReleaseNames _ = []
