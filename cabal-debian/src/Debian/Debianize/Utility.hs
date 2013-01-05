{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Utility
    ( DebMap
    , buildDebVersionMap
    , (!)
    , trim
    , strictReadF
    , replaceFile
    , modifyFile
    , diffFile
    , removeIfExists
    , dpkgFileMap
    , cond
    , debOfFile
    , readFile'
    , showDeps
    , showDeps'
    , withCurrentDirectory
    ) where

import Control.Exception as E (catch, try, bracket, IOException)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask)
import Data.Char (isSpace)
import Data.List (isSuffixOf, intercalate, intersperse)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import qualified Data.Set as Set
import Data.Text as Text (Text, unpack, lines)
import Data.Text.IO (hGetContents)
import Debian.Control (parseControl, lookupP, Field'(Field), unControl, stripWS)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Debian.Version.String (parseDebianVersion)
import qualified Debian.Relation as D
import System.Directory (doesFileExist, doesDirectoryExist, removeFile, renameFile, removeDirectory, getDirectoryContents, getCurrentDirectory, setCurrentDirectory)
import System.Exit(ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), dropExtension)
import System.IO (IOMode (ReadMode), withFile, openFile, hSetBinaryMode)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (pretty, text)

type DebMap = Map.Map D.BinPkgName (Maybe DebianVersion)

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
replaceFile path text =
    do removeFile back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       renameFile path back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       writeFile path text
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
diffFile path text =
    readProcessWithExitCode cmd args (unpack text) >>= \ (code, out, _err) ->
    case code of
      ExitSuccess -> return Nothing
      ExitFailure 1 -> return (Just out)
      _ -> error (showCommandForUser cmd args {- ++ " < " ++ show text -} ++ " -> " ++ show code)
    where
      cmd = "diff"
      args = ["-ruw", path, "-"]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists x = doesFileExist x >>= (`when` (removeFile x))

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists x = doesDirectoryExist x >>= (`when` (removeDirectory x))

removeIfExists :: FilePath -> IO ()
removeIfExists x = removeFileIfExists x >> removeDirectoryIfExists x

-- |Create a map from pathname to the names of the packages that contains that pathname.
-- We need to make sure we consume all the files, so 
dpkgFileMap :: IO (Map.Map FilePath (Set.Set D.BinPkgName))
dpkgFileMap =
    do
      let fp = "/var/lib/dpkg/info"
      names <- getDirectoryContents fp >>= return . filter (isSuffixOf ".list")
      let paths = map (fp </>) names
      files <- mapM (strictReadF Text.lines) paths
      return $ Map.fromList $ zip (map dropExtension names) (map (Set.fromList . map (D.BinPkgName . unpack)) $ files)

-- |Given a path, return the name of the package that owns it.
debOfFile :: FilePath -> ReaderT (Map.Map FilePath (Set.Set D.BinPkgName)) IO (Maybe D.BinPkgName)
debOfFile path =
    do mp <- ask
       return $ testPath (Map.lookup path mp)
    where
      -- testPath :: Maybe (Set.Set FilePath) -> Maybe FilePath
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

showDeps :: [[D.Relation]] -> String
showDeps = show . mconcat . intersperse (text ", ") . map D.prettyOrRelation

-- | Format dependencies on multiple lines, indenting enough to line
-- up under the given label.
showDeps' :: [a] -> [[D.Relation]] -> String
showDeps' prefix xss =
    intercalate  ("\n" ++ prefix' ++ " ") . Prelude.lines . show . D.prettyRelations $ xss
    where prefix' = map (\ _ -> ' ') prefix

-- | From Darcs.Utils
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
