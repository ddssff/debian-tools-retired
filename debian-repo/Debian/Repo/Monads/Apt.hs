{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Monads.Apt
    ( MonadApt(..)
    , AptIOT
    , AptIO
    -- * AptIO Monad
    , runAptIO
    , runAptT
    -- * State
    , AptState
    , initState
    , setRepoMap
    , getRepoMap
    , lookupRepository
    , insertRepository
    , lookupAptImage
    , insertAptImage
    , lookupSourcePackages
    , insertSourcePackages
    , lookupBinaryPackages
    , insertBinaryPackages
    , readParagraphs
    , findRelease
    , putRelease
    , countTasks
    , prepareRemoteRepository
    , prepareRepository
    ) where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Exception (ErrorCall(ErrorCall), Exception(toException))
import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (get, MonadIO(..), MonadTrans(..), put, StateT(runStateT))
import Data.List (intercalate)
import Data.Map as Map (empty, insert, lookup, Map)
import Data.Maybe (catMaybes)
import Data.Text as T (Text, unpack)
import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(parseControlFromHandle), Paragraph)
import qualified Debian.Control.Text as T (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph, Paragraph')
import Debian.Release (parseReleaseName, ReleaseName, ReleaseName(relName), releaseName')
import qualified Debian.Repo.File as F (File(..), Source(RemotePath))
import Debian.Repo.Types (AptImage, BinaryPackage, Release, SourcePackage)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot))
import Debian.Repo.Types.LocalRepository (prepareLocalRepository)
import Debian.Repo.Types.Release (makeReleaseInfo)
import Debian.Repo.Types.RemoteRepository (RemoteRepository, RemoteRepository(RemoteRepository))
import Debian.Repo.Types.Repo (Repo(repoKey), RepoKey(..))
import Debian.Repo.Types.Repository (Repository, Repository(LocalRepo, RemoteRepo))
import Debian.Sources (SliceName)
import Debian.URI (dirFromURI, fileFromURI, fromURI', toURI', URI(uriPath, uriScheme), URI', uriToString')
import Debian.UTF8 as Deb (decode)
import System.FilePath ((</>))
import qualified System.IO as IO (hClose, IOMode(ReadMode), openBinaryFile)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)
import System.Process.Progress (ePutStrLn, qPutStr, quieter)
import Text.Printf (printf)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- | A new monad to support the IO requirements of the autobuilder.
-- This uses the RWS monad.  The reader monad is used to store a flag
-- indicating whether this is a dry run, and the style information
-- associated with each output handle, including indentation, prefixing,
-- and replacing the output with one dot per n output characters.
-- The state monad stores information used to implement the current
-- output style and includes state information about whether the console
-- is at the beginning of a line, per-handle state information, and a
-- cache of the repositories that have been verified.
type AptIOT = StateT AptState
type AptIO = AptIOT IO

-- | This represents the state of the IO system.
data AptState
    = AptState
      { repoMap :: Map.Map URI' RemoteRepository		-- ^ Map to look up known (remote) Repository objects
      , releaseMap :: Map.Map (RepoKey, ReleaseName) Release -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map SliceName AptImage	-- ^ Map to look up prepared AptImage objects
      , sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      , binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
      }

-- |Perform an AptIO monad task in the IO monad.
runAptIO :: AptIO a -> IO a
runAptIO action = (runStateT action) initState >>= \ (a, _) -> return a

runAptT :: Monad m => AptIOT m a -> m a
runAptT action = (runStateT action) initState >>= return . fst

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: AptState
initState = AptState
            { repoMap = Map.empty
            , releaseMap = Map.empty
            , aptImageMap = Map.empty
            , sourcePackageMap = Map.empty
            , binaryPackageMap = Map.empty
            }

setRepoMap :: Map.Map URI' RemoteRepository -> AptState -> AptState
setRepoMap m state = state {repoMap = m}

getRepoMap :: AptState -> Map.Map URI' RemoteRepository
getRepoMap state = repoMap state

lookupRepository :: URI' -> AptState -> Maybe RemoteRepository
lookupRepository uri state = Map.lookup uri (repoMap state)

insertRepository :: URI' -> RemoteRepository -> AptState -> AptState
insertRepository uri repo state = state {repoMap = Map.insert uri repo (repoMap state)}

lookupAptImage :: SliceName -> AptState -> Maybe AptImage
lookupAptImage name state = Map.lookup  name (aptImageMap state)

insertAptImage :: SliceName -> AptImage -> AptState -> AptState
insertAptImage name image state = state {aptImageMap = Map.insert name image (aptImageMap state)}

lookupSourcePackages :: FilePath -> AptState -> Maybe (FileStatus, [SourcePackage])
lookupSourcePackages key state = Map.lookup key (sourcePackageMap state)

insertSourcePackages :: FilePath -> (FileStatus, [SourcePackage]) -> AptState -> AptState
insertSourcePackages key packages state = state {sourcePackageMap = Map.insert key packages (sourcePackageMap state)}

lookupBinaryPackages :: FilePath -> AptState -> Maybe (FileStatus, [BinaryPackage])
lookupBinaryPackages key state = Map.lookup key (binaryPackageMap state)

insertBinaryPackages :: FilePath -> (FileStatus, [BinaryPackage]) -> AptState -> AptState
insertBinaryPackages key packages state =
    state {binaryPackageMap = Map.insert key packages (binaryPackageMap state)}

readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)			-- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")	-- Debugging output
       return paragraphs

findRelease :: Repository -> ReleaseName -> AptState -> Maybe Release
findRelease repo dist state =
    Map.lookup (repoKey repo, dist) (releaseMap state)

putRelease :: Repository -> ReleaseName -> Release -> AptState -> AptState
putRelease repo dist release state =
    state {releaseMap = Map.insert (repoKey repo, dist) release (releaseMap state)}

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task

class (MonadIO m, Functor m, MonadCatchIO m) => MonadApt m where
    getApt :: m AptState
    putApt :: AptState -> m ()
    getRepoCache :: m (Map URI' RemoteRepository)
    putRepoCache :: Map URI' RemoteRepository -> m ()

instance (MonadIO m, Functor m, MonadCatchIO m) => MonadApt (AptIOT m) where
    getApt = get
    putApt = put
    getRepoCache = getApt >>= return . repoMap
    putRepoCache mp = getApt >>= \ a -> putApt (a {repoMap = mp})

instance MonadApt m => MonadApt (ReaderT s m) where
    getApt = lift getApt
    putApt = lift . putApt
    getRepoCache = getApt >>= return . repoMap
    putRepoCache mp = getApt >>= \ a -> putApt (a {repoMap = mp})

prepareRemoteRepository :: MonadApt m => URI -> m RemoteRepository
prepareRemoteRepository uri =
    do (state :: Map URI' RemoteRepository) <- getRepoCache
       -- FIXME: We only want to verifyRepository on demand.
       repo <- maybe (verifyRemoteRepository (toURI' uri)) return (Map.lookup (toURI' uri) state)
       putRepoCache (Map.insert (toURI' uri) repo state)
       return repo

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
{-# NOINLINE verifyRemoteRepository #-}
verifyRemoteRepository :: MonadApt m => URI' -> m RemoteRepository
verifyRemoteRepository uri =
    do state <- getRepoCache
       case Map.lookup uri state of
         Just repo -> return repo
         Nothing ->
             do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
                let repo = RemoteRepository uri releaseInfo
                modifyRepoCache (Map.insert uri repo)
                return repo

modifyRepoCache :: MonadApt m => (Map URI' RemoteRepository -> Map URI' RemoteRepository) -> m ()
modifyRepoCache f = do
    s <- getRepoCache
    putRepoCache (f s)

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

prepareRepository :: MonadApt m => RepoKey -> m Repository
prepareRepository key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= return . LocalRepo
      Remote uri' ->
          let uri = fromURI' uri' in
          case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               _ -> prepareRemoteRepository uri >>= return . RemoteRepo
