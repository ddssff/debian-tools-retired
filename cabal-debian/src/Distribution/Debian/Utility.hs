{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Distribution.Debian.Utility
    ( DebMap
    , buildDebVersionMap
    , (!)
    , trim
    , unPackageName
    , strictReadF
    , replaceFile
    , modifyFile
    , diffFile
    , removeIfExists
    , dpkgFileMap
    , cond
    , debOfFile
    , readFile'
    , filterMissing
    , showDeps
    , showDeps'
    ) where

import Control.Exception (catch, try, IOException)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask)
import Data.Char (isSpace)
import Data.List (isSuffixOf, intercalate)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Debian.Control (parseControl, lookupP, Field'(Field), unControl, stripWS)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Debian.Version.String (parseDebianVersion)
import qualified Debian.Relation as D
import Distribution.Package (PackageName(..))
import Prelude hiding (catch)
import System.Directory (doesFileExist, doesDirectoryExist, removeFile, renameFile, removeDirectory, getDirectoryContents)
import System.Exit(ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), dropExtension)
import System.IO (IOMode (ReadMode), hGetContents, withFile, openFile, hSetBinaryMode, hGetContents)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode, showCommandForUser)

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
m ! k = maybe (error ("No version number for " ++ (show . D.prettyPkgName $ k) ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)

trim :: String -> String
trim = dropWhile isSpace

unPackageName :: PackageName -> String
unPackageName (PackageName s) = s

strictReadF :: (String -> r) -> FilePath -> IO r
strictReadF f path = withFile path ReadMode (\h -> hGetContents h >>= (\x -> return $! f x))
-- strictRead = strictReadF id

-- | Write a file which we might still be reading from in
-- order to compute the text argument.
replaceFile :: FilePath -> String -> IO ()
replaceFile path text =
    do removeFile back `catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       renameFile path back `catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       writeFile path text
    where
      back = path ++ "~"

-- | Compute the new file contents from the old.  If f returns Nothing
-- do not write.
modifyFile :: FilePath -> (String -> IO (Maybe String)) -> IO ()
modifyFile path f =
    do removeFile back `catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       try (renameFile path back) >>=
           either (\ (e :: IOException) -> if not (isDoesNotExistError e)
                                           then ioError e
                                           else f "" >>= maybe (return ()) (writeFile path))
                  (\ () -> readFile back >>= f >>= maybe (return ()) (writeFile path))
    where
      back = path ++ "~"

diffFile :: FilePath -> String -> IO (Maybe String)
diffFile path text =
    readProcessWithExitCode cmd args text >>= \ (code, out, _err) ->
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
      files <- mapM (strictReadF lines) paths
      return $ Map.fromList $ zip (map dropExtension names) (map (Set.fromList . map D.BinPkgName) $ files)

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

readFile' :: FilePath -> IO String
readFile' path
  = do
    file <- openFile path ReadMode
    hSetBinaryMode file True
    hGetContents file

filterMissing :: [D.BinPkgName] -> [[D.Relation]] -> [[D.Relation]]
filterMissing missing rels =
    filter (/= []) (map (filter (\ (D.Rel name _ _) -> not (elem name missing))) rels)

showDeps :: [[D.Relation]] -> String
showDeps xss = intercalate ", " (map (intercalate " | " . map (show . D.prettyRelation)) xss)

{-
showDeps' :: [a] -> [[D.Relation]] -> String
showDeps' prefix xss =
    intercalate (",\n " ++ prefix') (map (intercalate " | " . map (show . D.prettyRelation)) xss)
    where prefix' = map (\ _ -> ' ') prefix
-}

showDeps' :: [a] -> [[D.Relation]] -> String
showDeps' prefix xss =
    intercalate ("\n" ++ prefix' ++ " , ") (map (intercalate " | " . map (show . D.prettyRelation)) xss)
    where prefix' = map (\ _ -> ' ') prefix
