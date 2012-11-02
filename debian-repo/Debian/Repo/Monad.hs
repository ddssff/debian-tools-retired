{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.Monad
    ( AptIOT
    , AptIO
    -- * AptIO Monad
    , io
    , tio
    , runAptIO
    , tryAB
    , tryJustAB
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
    ) where

import Control.Exception (Exception, tryJust)
import qualified Debian.Control.ByteString as B
    ( Paragraph,
      Control'(Control),
      ControlFunctions(parseControlFromHandle) )
import Debian.Release (ReleaseName)
import Debian.Sources (SliceName)
import Debian.Repo.Types
    ( AptImage,
      SourcePackage,
      BinaryPackage,
      Release,
      Repo(repoURI),
      Repository )
import Control.Exception ( try )
import Control.Monad.State
    ( MonadTrans(..),
      MonadState(get),
      MonadIO(..),
      StateT(runStateT),
      mapStateT )
import qualified Data.Map as Map
    ( insert, Map, empty, findWithDefault )
import Debian.URI ( URI )
import qualified System.IO as IO
    ( IOMode(ReadMode), hClose, openBinaryFile )
import System.Posix.Files
    ( FileStatus, deviceID, fileID, modificationTime )
import System.Process.Progress (ePutStrLn)
import Text.Printf ( printf )

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
      { repoMap :: Map.Map URI (Maybe Repository)		-- ^ Map to look up known Repository objects
      , releaseMap :: Map.Map (URI, ReleaseName) (Maybe Release) -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map SliceName (Maybe AptImage)	-- ^ Map to look up prepared AptImage objects
      , sourcePackageMap :: Map.Map FilePath (Maybe (FileStatus, [SourcePackage]))
      , binaryPackageMap :: Map.Map FilePath (Maybe (FileStatus, [BinaryPackage]))
      }

-- |mark an action that should be run in the regular IO monad
io :: IO a -> AptIO a
io = liftIO

-- |mark an action that should be run in the terminal IO monad
tio :: IO a -> AptIO a
tio = lift

-- |Perform an AptIO monad task in the IO monad.
runAptIO :: AptIO a -> IO a
runAptIO action = (runStateT action) initState >>= \ (a, _) -> return a

-- |Implementation of try for the AptIO monad.  If the task throws
-- an exception the initial state will be restored.
tryAB :: Exception e => AptIO a -> AptIO (Either e a)
tryAB task =
    do state <- get
       mapStateT (try' state) task
    where
      try' state task =
          do result <- try task
             case result of
               Left e -> return (Left e, state)
               Right (a, state') -> return (Right a, state')

-- |Implementation of try for the AptIO monad.  If the task throws
-- an exception the initial state will be restored.
tryJustAB :: Exception e => (e -> Maybe b) -> AptIO a -> AptIO (Either b a)
tryJustAB f task =
    do state <- get
       mapStateT (tryJust' state) task
    where
      tryJust' state task =
          do result <- tryJust f task
             case result of
               Left b -> return (Left b, state)
               Right (a, state') -> return (Right a, state')

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

setRepoMap :: Map.Map URI (Maybe Repository) -> AptState -> AptState
setRepoMap m state = state {repoMap = m}

getRepoMap :: AptState -> Map.Map URI (Maybe Repository)
getRepoMap state = repoMap state

lookupRepository :: URI -> AptState -> Maybe Repository
lookupRepository uri state = Map.findWithDefault Nothing uri (repoMap state)

insertRepository :: URI -> Repository -> AptState -> AptState
insertRepository uri repo state = state {repoMap = Map.insert uri (Just repo) (repoMap state)}

lookupAptImage :: SliceName -> AptState -> Maybe AptImage
lookupAptImage name state = Map.findWithDefault Nothing  name (aptImageMap state)

insertAptImage :: SliceName -> AptImage -> AptState -> AptState
insertAptImage name image state = state {aptImageMap = Map.insert name (Just image) (aptImageMap state)}

lookupSourcePackages :: FilePath -> AptState -> Maybe (FileStatus, [SourcePackage])
lookupSourcePackages key state = Map.findWithDefault Nothing key (sourcePackageMap state)

insertSourcePackages :: FilePath -> (FileStatus, [SourcePackage]) -> AptState -> AptState
insertSourcePackages key packages state = state {sourcePackageMap = Map.insert key (Just packages) (sourcePackageMap state)}

lookupBinaryPackages :: FilePath -> AptState -> Maybe (FileStatus, [BinaryPackage])
lookupBinaryPackages key state = Map.findWithDefault Nothing key (binaryPackageMap state)

insertBinaryPackages :: FilePath -> (FileStatus, [BinaryPackage]) -> AptState -> AptState
insertBinaryPackages key packages state =
    state {binaryPackageMap = Map.insert key (Just packages) (binaryPackageMap state)}

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
    Map.findWithDefault Nothing (repoURI repo, dist) (releaseMap state)

putRelease :: Repository -> ReleaseName -> Release -> AptState -> AptState
putRelease repo dist release state =
    state {releaseMap = Map.insert (repoURI repo, dist) (Just release) (releaseMap state)}

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task
