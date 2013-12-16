{-# LANGUAGE DeriveDataTypeable, PackageImports #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |An AptCache represents a local cache of a remote repository.  The
-- cached information is usually downloaded by running "apt-get
-- update", and appears in @\/var\/lib\/apt\/lists@.
module Debian.Repo.Cache
    ( SourcesChangedAction(..)
    , aptSourcePackagesSorted
    , cacheDistDir
    , distDir
    , aptDir
    , cacheRootDir
    , cacheSourcesPath
    , sourcesPath
    , sourceDir
    , aptCacheFiles
    , aptCacheFilesOfSlice
    , archFiles
    , buildArchOfEnv
    , buildArchOfRoot
    , sourcePackages
    , binaryPackages
    ) where

import Control.DeepSeq (force, NFData)
import Data.Data (Data)
import Data.List (intercalate, sortBy)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..), prettyArch)
import Debian.Relation (BinPkgName, SrcPkgName(..))
import Debian.Release (ReleaseName(relName), releaseName', sectionName')
import Debian.Repo.AptImage (AptCache(aptArch, aptBinaryPackages, aptReleaseName, aptSourcePackages, globalCacheDir))
import Debian.Repo.EnvPath (EnvRoot(EnvRoot))
import Debian.Repo.PackageID (PackageID(packageVersion, packageName))
import Debian.Repo.PackageIndex (BinaryPackage(packageID), SourcePackage(sourcePackageID))
import Debian.Sources (DebSource(..), SourceType(..))
import Network.URI (escapeURIString, URI(uriAuthority, uriPath, uriScheme), URIAuth(uriPort, uriRegName, uriUserInfo))
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import System.Unix.Chroot (useEnv)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance NFData ExitCode

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

-- | A directory which will hold all the cached files for this
-- NamedSliceList.
cacheDistDir :: FilePath -> ReleaseName -> FilePath
cacheDistDir cacheDir release = cacheDir ++ "/dists/" ++ relName release

cacheRootDir :: FilePath -> ReleaseName -> EnvRoot
cacheRootDir cacheDir release = EnvRoot (cacheDistDir cacheDir release ++ "/aptEnv")

distDir :: AptCache c => c -> FilePath
distDir cache = cacheDistDir (globalCacheDir cache) (aptReleaseName cache)

aptDir :: AptCache c => c -> SrcPkgName -> FilePath
aptDir cache package = distDir cache ++ "/apt/" ++ unSrcPkgName package

-- | The path where a text of the SliceList is stored.
cacheSourcesPath :: FilePath -> ReleaseName -> FilePath
cacheSourcesPath cacheDir release = cacheDistDir cacheDir release </> "sources"

sourcesPath :: AptCache c => c -> FilePath
sourcesPath cache = cacheSourcesPath (globalCacheDir cache) (aptReleaseName cache)

-- Additional functions which can only be used on already constructed
-- instances of AptCache.

-- | A directory holding all files downloaded by apt-get source for a
-- certain package
sourceDir :: AptCache t => t -> String -> FilePath
sourceDir c package = distDir c ++ "/apt/" ++ package

-- |Return all the named source packages sorted by version
aptSourcePackagesSorted :: AptCache t => t -> [SrcPkgName] -> [SourcePackage]
aptSourcePackagesSorted os names =
    sortBy cmp . filterNames names . aptSourcePackages $ os
    where
      filterNames names' packages =
          filter (flip elem names' . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

-- |Return the paths in the local cache of the index files of a slice list.
aptCacheFiles :: AptCache a => a -> [DebSource] -> [FilePath]
aptCacheFiles apt sources = concat . map (aptCacheFilesOfSlice apt) $ sources

-- |Return the paths in the local cache of the index files of a single slice.
aptCacheFilesOfSlice :: AptCache a => a -> DebSource -> [FilePath]
aptCacheFilesOfSlice apt slice = archFiles (aptArch apt) slice

-- |Return the list of files that apt-get update would write into
-- \/var\/lib\/apt\/lists when it processed the given list of DebSource.
archFiles :: Arch -> DebSource -> [FilePath]
archFiles arch deb =
    case (arch, deb) of
      (Binary _ _, DebSource DebSrc _ _) ->
          map (++ "_source_Sources") (archFiles' deb)
      (Binary _os _cpu, DebSource Deb _ _) ->
          map (++ ("_binary-" ++ show (prettyArch arch) ++ "_Packages")) (archFiles' deb)
      (x, _) -> error $ "Invalid build architecture: " ++ show x

archFiles' :: DebSource -> [FilePath]
archFiles' deb =
    let uri = sourceUri deb
        distro = sourceDist deb in
    let scheme = uriScheme uri
        auth = uriAuthority uri
        path = uriPath uri in
    let userpass = maybe "" uriUserInfo auth
        reg = maybeOfString $ maybe "" uriRegName auth
        port = maybe "" uriPort auth in
    let (user, pass) = break (== ':') userpass in
    let user' = maybeOfString user
        pass' = maybeOfString pass in
    let uriText = prefix scheme user' pass' reg port path in
    -- what about dist?
    either (\ exact -> [(escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText ++ escape exact))])
           (\ (dist, sections) ->
                map (\ section ->
                         (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
                          releaseName' dist ++ "_" ++ sectionName' section))
                    sections)
           distro
    where
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
      prefix _ _ _ _ _ _ = error ("invalid DebSource: " ++ show (pretty deb))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)

buildArchOfEnv :: EnvRoot -> IO Arch
buildArchOfEnv (EnvRoot root)  =
    do setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
       a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (ArchOS os) (ArchCPU cpu)
         _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)
{-
  (err, _) <- useEnv root forceList (readProcessChunks (shell cmd) L.empty) >>= return . collectOutputs
       case code of
         (ExitSuccess : _) ->
             case words (UTF8.toString (B.concat (L.toChunks out))) of
               [] -> error $ "Invalid output from " ++ cmd
               (arch : _) -> return (Binary arch)
         _ -> error $ "Failure: " ++ cmd ++ " -> " ++ show code ++ "\n\nstdout:\n\n" ++ show out ++ "\n\nstderr:\n\n" ++ show err
    where
      cmd = "export LOGNAME=root; dpkg-architecture -qDEB_BUILD_ARCH"
-}

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x
{-
    do (code, out, err, _) <- runProcess (shell cmd) L.empty >>= return . collectOutputs
       case code of
         (ExitSuccess : _) ->
             case words (UTF8.toString (B.concat (L.toChunks out))) of
               [] -> error $ "Invalid output from " ++ cmd
               (arch : _) -> return (Binary arch)
         _ -> error $ "Failure: " ++ cmd ++ " -> " ++ show code ++ "\n\nstdout:\n\n" ++ show out ++ "\n\nstderr:\n\n" ++ show err
    where
      cmd = "dpkg-architecture -qDEB_BUILD_ARCH"
-}

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b

wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
wordsBy p s =
    case (break p s) of
      (s', []) -> [s']
      (h, t) -> h : wordsBy p (drop 1 t)

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

-- | Return a sorted list of available source packages, newest version first.
sourcePackages :: AptCache a => a -> [SrcPkgName] -> [SourcePackage]
sourcePackages os names =
    sortBy cmp . filterNames . aptSourcePackages $ os
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          filter (flip elem names . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

binaryPackages :: AptCache a => a -> [BinPkgName] -> [BinaryPackage]
binaryPackages os names =
    sortBy cmp . filterNames . aptBinaryPackages $ os
    where
      filterNames :: [BinaryPackage] -> [BinaryPackage]
      filterNames packages =
          filter (flip elem names . packageName . packageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . packageID $ p1
            v2 = packageVersion . packageID $ p2
