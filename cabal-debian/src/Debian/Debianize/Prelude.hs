-- | Functions and instances used by but not related to cabal-debian.
-- These could conceivably be moved into more general libraries.
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Prelude
    ( curry3
    , DebMap
    , buildDebVersionMap
    , (!)
    , trim
    , strictReadF
    , replaceFile
    , modifyFile
    , diffFile
    , removeIfExists
    , dpkgFileMap
    , debOfFile
    , cond
    , readFile'
    , readFileMaybe
    , showDeps
    , showDeps'
    , withCurrentDirectory
    , getDirectoryContents'
    , setMapMaybe
    , zipMaps
    , foldEmpty
    , maybeL
    , indent
    , maybeRead
    , read'
    , modifyM
    , intToVerbosity'
    , (~=)
    , (~?=)
    , (%=)
    , (+=)
    , (++=)
    , (+++=)
    , fromEmpty
    , fromSingleton
    , Pretty(pretty)
    ) where

import Control.Applicative ((<$>))
import Control.Exception as E (catch, try, bracket, IOException)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (MonadState, StateT, get, put)
import Data.Char (isSpace)
import qualified Data.Lens.Lazy as Lens ((~=), (%=))
import Data.List as List (isSuffixOf, intercalate, map, lines)
import Data.Lens.Lazy (Lens, modL)
import Data.Map as Map (Map, foldWithKey, empty, fromList, findWithDefault, insert, map, lookup, insertWith)
import Data.Maybe (catMaybes, mapMaybe, listToMaybe, fromMaybe, fromJust)
import Data.Monoid (Monoid, (<>), mappend)
import Data.Set as Set (Set, toList)
import qualified Data.Set as Set
import Data.Text as Text (Text, unpack, lines)
import Data.Text.IO (hGetContents)
import Debian.Control (parseControl, lookupP, Field'(Field), unControl, stripWS)
import Debian.Orphans ()
import Debian.Version (DebianVersion, prettyDebianVersion)
import Debian.Version.String (parseDebianVersion)
import qualified Debian.Relation as D
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Verbosity (Verbosity, intToVerbosity)
import Prelude hiding (map, lookup)
import System.Directory (doesFileExist, doesDirectoryExist, removeFile, renameFile, removeDirectory, getDirectoryContents, getCurrentDirectory, setCurrentDirectory)
import System.Exit(ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), dropExtension)
import System.IO (IOMode (ReadMode), withFile, openFile, hSetBinaryMode)
import System.IO.Error (isDoesNotExistError, catchIOError)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

type DebMap = Map D.BinPkgName (Maybe DebianVersion)

-- | Read and parse the status file for installed debian packages: @/var/lib/dpkg/status@
buildDebVersionMap :: IO DebMap
buildDebVersionMap =
    readFile "/var/lib/dpkg/status" >>=
    return . either (const []) unControl . parseControl "/var/lib/dpkg/status" >>=
    mapM (\ p -> case (lookupP "Package" p, lookupP "Version" p) of
                   (Just (Field (_, name)), Just (Field (_, version))) ->
                       return (Just (D.BinPkgName (stripWS name), Just (parseDebianVersion (stripWS version))))
                   _ -> return Nothing) >>=
    return . Map.fromList . catMaybes

(!) :: DebMap -> D.BinPkgName -> DebianVersion
m ! k = maybe (error ("No version number for " ++ (show . pretty $ k) ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)

trim :: String -> String
trim = dropWhile isSpace

strictReadF :: (Text -> r) -> FilePath -> IO r
strictReadF f path = withFile path ReadMode (\h -> hGetContents h >>= (\x -> return $! f x))
-- strictRead = strictReadF id

-- | Write a file which we might still be reading from in
-- order to compute the text argument.
replaceFile :: FilePath -> String -> IO ()
replaceFile path s =
    do removeFile back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       renameFile path back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       writeFile path s
    where
      back = path ++ "~"

-- | Compute the new file contents from the old.  If f returns Nothing
-- do not write.
modifyFile :: FilePath -> (String -> IO (Maybe String)) -> IO ()
modifyFile path f =
    do removeFile back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       try (renameFile path back) >>=
           either (\ (e :: IOException) -> if not (isDoesNotExistError e)
                                           then ioError e
                                           else f "" >>= maybe (return ()) (writeFile path))
                  (\ () -> readFile back >>= f >>= maybe (return ()) (writeFile path))
    where
      back = path ++ "~"

diffFile :: FilePath -> Text -> IO (Maybe String)
diffFile path s =
    readProcessWithExitCode cmd args (unpack s) >>= \ (code, out, _err) ->
    case code of
      ExitSuccess -> return Nothing
      ExitFailure 1 -> return (Just out)
      _ -> error (showCommandForUser cmd args {- ++ " < " ++ show s -} ++ " -> " ++ show code)
    where
      cmd = "diff"
      args = ["-ruw", path, "-"]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists x = doesFileExist x >>= (`when` (removeFile x))

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists x = doesDirectoryExist x >>= (`when` (removeDirectory x))

removeIfExists :: FilePath -> IO ()
removeIfExists x = removeFileIfExists x >> removeDirectoryIfExists x

-- |Create a map from pathname to the names of the packages that contains that pathname using the
-- contents of the debian package info directory @/var/lib/dpkg/info".
dpkgFileMap :: IO (Map FilePath (Set D.BinPkgName))
dpkgFileMap =
    do
      let fp = "/var/lib/dpkg/info"
      names <- getDirectoryContents fp >>= return . filter (isSuffixOf ".list")
      let paths = List.map (fp </>) names
      -- Read strictly to make sure we consume all the files and don't
      -- hold tons of open file descriptors.
      files <- mapM (strictReadF Text.lines) paths
      return $ Map.fromList $ zip (List.map dropExtension names) (List.map (Set.fromList . List.map (D.BinPkgName . unpack)) $ files)

-- |Given a path, return the name of the package that owns it.
debOfFile :: FilePath -> ReaderT (Map FilePath (Set D.BinPkgName)) IO (Maybe D.BinPkgName)
debOfFile path =
    do mp <- ask
       return $ testPath (lookup path mp)
    where
      -- testPath :: Maybe (Set FilePath) -> Maybe FilePath
      testPath Nothing = Nothing
      testPath (Just s) =
          case Set.size s of
            1 -> Just (Set.findMin s)
            _ -> Nothing

cond :: t -> t -> Bool -> t
cond ifF _ifT False = ifF
cond _ifF ifT True = ifT

readFile' :: FilePath -> IO Text
readFile' path =
    do file <- openFile path ReadMode
       hSetBinaryMode file True
       hGetContents file

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe path = (Just <$> readFile' path) `catchIOError` (\ _ -> return Nothing)

-- Would like to call pretty instead of D.prettyRelations, but the
-- Pretty instance for [a] doesn't work for us.
showDeps :: [[D.Relation]] -> String
showDeps = show . D.prettyRelations

-- The extra space after prefix' is here for historical reasons(?)
showDeps' :: [a] -> [[D.Relation]] -> String
showDeps' prefix xss =
    intercalate  ("\n" ++ prefix' ++ " ") . Prelude.lines . show . D.prettyRelations $ xss
    where prefix' = List.map (\ _ -> ' ') prefix

-- | From Darcs.Utils - set the working directory and run an IO operation.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory name m =
    E.bracket
        (do cwd <- getCurrentDirectory
            setCurrentDirectory name
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd {- `catchall` return () -})
        (const m)

{-
catchall :: IO a -> IO a -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)

-- catchNonSignal is a drop-in replacement for Control.Exception.catch, which allows
-- us to catch anything but a signal.  Useful for situations where we want
-- don't want to inhibit ctrl-C.

catchNonSignal :: IO a -> (E.SomeException -> IO a) -> IO a
catchNonSignal comp handler = catch comp handler'
    where handler' se =
           case fromException se :: Maybe SignalException of
             Nothing -> handler se
             Just _ -> E.throw se

newtype SignalException = SignalException Signal deriving (Show, Typeable)

instance Exception SignalException where
   toException e = SomeException e
   fromException (SomeException e) = cast e
-}

-- | Get directory contents minus dot files.
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir =
    getDirectoryContents dir >>= return . filter (not . dotFile)
    where
      dotFile "." = True
      dotFile ".." = True
      dotFile _ = False

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = Set.fromList . mapMaybe p . toList

zipMaps :: Ord k => (k -> Maybe a -> Maybe b -> Maybe c) -> Map k a -> Map k b -> Map k c
zipMaps f m n =
    foldWithKey h (foldWithKey g empty m) n
    where
      g k a r = case f k (Just a) (lookup k n) of
                  Just c -> Map.insert k c r              -- Both m and n have entries for k
                  Nothing -> r                            -- Only m has an entry for k
      h k b r = case lookup k m of
                  Nothing -> case f k Nothing (Just b) of
                               Just c -> Map.insert k c r -- Only n has an entry for k
                               Nothing -> r
                  Just _ -> r

foldEmpty :: r -> ([a] -> r) -> [a] -> r
foldEmpty r _ [] = r
foldEmpty _ f l = f l

-- | If the current value of getL x is Nothing, replace it with f.
maybeL :: Lens a (Maybe b) -> Maybe b -> a -> a
maybeL lens mb x = modL lens (maybe mb Just) x

indent :: [Char] -> String -> String
indent prefix s = unlines (List.map (prefix ++) (List.lines s))

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

read' :: Read a => (String -> a) -> String -> a
read' f s = fromMaybe (f s) (maybeRead s)

-- modifyM :: (Monad m, MonadTrans t, MonadState a (t m)) => (a -> m a) -> t m ()
-- modifyM f = get >>= lift . f >>= put

-- modifyM :: (Monad m, MonadTrans t, MonadState a (t m)) => (a -> m a) -> t m ()
modifyM :: MonadState a m => (a -> m a) -> m ()
modifyM f = get >>= f >>= put

-- read' :: Read a => String -> a
-- read' s = trace ("read " ++ show s) (read s)

-- | Version of 'Distribution.Verbosity.intToVerbosity' that first
-- clamps its argument to the acceptable range (0-3).
intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))

-- | Set a lens value.  (This is a version of Data.Lens.Lazy.~= that returns () instead of b.)
(~=) :: Monad m => Lens a b -> b -> StateT a m ()
lens ~= x = lens Lens.~= x >> return ()

-- | Set @b@ if it currently isNothing and the argument isJust, that is
--  1. Nothing happens if the argument isNothing
--  2. Nothing happens if the current value isJust
(~?=) :: Monad m => Lens a (Maybe b) -> Maybe b -> StateT a m ()
lens ~?= (Just x) = lens Lens.%= maybe (Just x) Just >> return ()
_ ~?= _ = return ()

-- | Modify a value.  (This is a version of Data.Lens.Lazy.%= that returns () instead of a.)
(%=) :: Monad m => Lens a b -> (b -> b) -> StateT a m ()
lens %= f = lens Lens.%= f >> return ()

-- | Insert an element into a @(Set b)@
(+=) :: (Monad m, Ord b) => Lens a (Set b) -> b -> StateT a m ()
lens += x = lens %= Set.insert x

-- | Insert an element into a @(Map b c)@
(++=) :: (Monad m, Ord b) => Lens a (Map b c) -> (b, c) -> StateT a m ()
lens ++= (k, a) = lens %= Map.insert k a

-- | Insert an element into a @(Map b (Set c))@
(+++=) :: (Monad m, Ord b, Monoid c) => Lens a (Map b c) -> (b, c) -> StateT a m ()
lens +++= (k, a) = lens %= Map.insertWith mappend k a

fromEmpty :: Set a -> Set a -> Set a
fromEmpty d s | Set.null s = d
fromEmpty _ s = s

fromSingleton :: a -> ([a] -> a) -> Set a -> a
fromSingleton e multiple s =
    case toList s of
      [x] -> x
      [] -> e
      xs -> multiple xs

instance Pretty PackageIdentifier where
    pretty p = pretty (pkgName p) <> text "-" <> pretty (pkgVersion p)

instance Pretty PackageName where
    pretty (PackageName s) = text s
