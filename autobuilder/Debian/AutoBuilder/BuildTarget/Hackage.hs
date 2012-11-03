{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults -fno-warn-missing-signatures -Werror #-}
module Debian.AutoBuilder.BuildTarget.Hackage
    ( prepare
    , documentation
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
import Control.Exception (SomeException, try, throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, isSuffixOf, intercalate, nub, sort)
import Data.Maybe (catMaybes)
import Data.Version (Version, showVersion, parseVersion)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo hiding (getVersion)
import System.Exit
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (showCommandForUser, CmdSpec(..))
import System.Process.Read (readProcessWithExitCode)
import System.Process.Progress (runProcess, collectOutputs)
import System.Unix.Directory (removeRecursiveSafely)
import Text.XML.HaXml (htmlprint)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Posn
import Text.ParserCombinators.ReadP (readP_to_S)

documentation :: [String]
documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

prepare :: MonadApt e m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package name = liftIO $
    do (version' :: Version) <- maybe (getVersion (P.hackageServer (P.params cache)) name) (return . readVersion) versionString
       when (P.flushSource (P.params cache)) (removeRecursiveSafely (tarball cache name version'))
       download cache name version'
       tree <- findSourceTree (unpacked cache name version')
       return $ T.Download { T.package = package
                           , T.getTop = topdir tree
                           , T.logText =  "Built from hackage, revision: " ++ show (P.spec package)
                           , T.mVersion = Just version'
                           , T.origTarball = Just (tarball cache name version')
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id }
    where
      versionString = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                                  P.CabalPin s -> Just s
                                                                  _ -> Nothing) (P.flags package)))) of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory:
-- > download \"/home/dsf/.autobuilder/hackage\" -> \"/home/dsf/.autobuilder/hackage/happstack-server-6.1.4.tar.gz\"
-- After the download it tries to untar the file, and then it saves the compressed tarball.
download :: P.CacheRec -> String -> Version -> IO ()
download cache name version =
    removeRecursiveSafely (unpacked cache name version) >>
    downloadCached (P.hackageServer (P.params cache)) cache name version >>=
    unpack cache

{-
-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory:
-- > download \"/home/dsf/.autobuilder/hackage\" -> \"/home/dsf/.autobuilder/hackage/happstack-server-6.1.4.tar.gz\"
-- After the download it tries to untar the file, and then it saves the compressed tarball.
downloadAndDebianize :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
downloadAndDebianize cache flags name version =
    download  cache flags name version >>
    debianize cache flags (unpacked (P.topDir cache) name version)
-}

-- |Scan the flag list for Patch flag, and apply the patches
{-
patch :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
patch cache flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked cache name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("patch " ++ show (unpacked cache name version) ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()
-}

readVersion :: String -> Version
readVersion s =
    case filter (null . snd) $ readP_to_S parseVersion s of
      [(v, _)] -> v
      _ -> error $ "Failure reading cabal vesion: " ++ show s

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory.  After the download it validates the
-- tarball text and saves the compressed tarball.
downloadCached :: String -> P.CacheRec -> String -> Version -> IO B.ByteString
downloadCached server cache name version =
    do exists <- doesFileExist (tarball cache name version)
       case exists of
         True -> let path = tarball cache name version in
                 try (B.readFile path >>=
                      return . validate >>=
                      maybe (download' server cache name version) return) >>=
                 either (\ (e :: SomeException) ->
                             let msg = "Failure reading " ++ path ++ ": " ++ show e in
                             hPutStrLn stderr msg >>
                             hPutStrLn stderr ("Removing " ++ path) >>
                             removeFile path >>
                             download' server cache name version)
                        return
         False -> download' server cache name version

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> String -> IO Version
getVersion server name =
    do result@(code, out, _) <- readProcessWithExitCode cmd args B.empty
       case code of
         ExitSuccess -> return $ readVersion $ findVersion name $ htmlParse (showCommandForUser cmd args) (B.unpack out)
         _ -> error ("Could not get version for " ++ name ++ "\n " ++ cmd ++ " -> " ++ show result)
    where
      cmd = "curl"
      args = ["-s", url]
      url = packageURL server name

-- |Unpack and save the files of a tarball.
unpack :: P.CacheRec -> B.ByteString -> IO ()
unpack cache text = Tar.unpack (tmpDir cache) (Tar.read (Z.decompress text))

-- |Validate the text of a tarball file.
validate :: B.ByteString -> Maybe B.ByteString
validate text =
    let entries = Tar.read (Z.decompress text) in
    case Tar.foldEntries (\ _ -> either throw (Right . (+ 1))) (Right 0) Left entries of
      Left _ -> Nothing
      Right _ -> Just text

findVersion :: String -> Document Posn -> String
findVersion package (Document _ _ (Elem _name _attrs content) _) =
    case doContentList content of
      [s] -> s
      _ss -> error ("Could not find version number of " ++ package ++ " in " ++ show (map (htmlprint . (: [])) content))
    where
      doContentList [CElem (Elem (N "head") _ _) _, CElem (Elem (N "body") _ content) _] = doContentList content
      doContentList [CElem (Elem (N "div") _ _) _, CElem (Elem (N "div") _ content) _, CElem (Elem (N "div") _ _) _] = doContentList content
      doContentList (CElem (Elem (N "h1") _ _) _ : etc) = doContentList (drop (length etc - 2) etc)
      doContentList [CElem (Elem (N "h2") _ _) _, CElem (Elem (N "ul") _ content) _] = doContentList content
      doContentList (CElem (Elem (N "li") _ content) _ :  _) = doContentList content
      doContentList (CElem (Elem (N "a") _ content) _ : _) = doContentList content
      doContentList [CString _ c _] = [parseTarballName c]
      doContentList xs = error (show (map ((: []) . htmlprint . (: [])) xs))
      parseTarballName s =
          let prefix = package ++ "-"
              suffix = ".tar.gz" in
          if isPrefixOf prefix s && isSuffixOf suffix s
          then let s' = drop (length prefix) s in
               take (length s' - length suffix) s'
          else error $ "findVersion - not a tarball: " ++ show s

-- |Download and save the tarball, return its contents.
download' :: String -> P.CacheRec -> String -> Version -> IO B.ByteString
download' server cache name version =
    do (res, out, err, _) <- runProcess id (ShellCommand (downloadCommand server name version)) B.empty >>= return . collectOutputs
       -- (res, out, err) <- runProcessWith
       case res of
         (ExitSuccess : _) ->
             do createDirectoryIfMissing True (tmpDir cache)
                B.writeFile (tarball cache name version) out
                return out
         _ ->
             let msg = downloadCommand server name version ++ " ->\n" ++ show (err, res) in
             hPutStrLn stderr msg >>
             error msg

-- |Hackage paths
packageURL server name = "http://" ++ server ++ "/package/" ++ name

versionURL server name version = "http://" ++ server ++ "/packages/archive/" ++ name ++ "/" ++ showVersion version ++ "/" ++ name ++ "-" ++ showVersion version ++ ".tar.gz"

downloadCommand :: String -> String -> Version -> String
downloadCommand server name version = "curl -s '" ++ versionURL server name version ++ "'" {- ++ " > '" ++ destPath top name version ++ "'" -}

unpacked :: P.CacheRec -> String -> Version -> FilePath
unpacked cache name version = tmpDir cache </> name ++ "-" ++ showVersion version

tarball :: P.CacheRec -> String -> Version -> FilePath
tarball cache name version  = tmpDir cache </> name ++ "-" ++ showVersion version ++ ".tar.gz"

tmpDir :: P.CacheRec -> FilePath
tmpDir cache = P.topDir cache </> "hackage"
