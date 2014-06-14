{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.GHC
    ( withCompilerVersion
    , newestAvailable
    , compilerIdFromDebianVersion
    , compilerFlavorOption
    , newestAvailableCompilerId
    -- , ghcNewestAvailableVersion'
    -- , ghcNewestAvailableVersion
    -- , compilerIdFromDebianVersion
    ) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (toLower, toUpper, isSpace)
import Data.Function.Memoize (deriveMemoizable, memoize)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion, Version(Version))
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerId(CompilerId), CompilerFlavor(..))
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Unix.Chroot (useEnv)
import Text.Read (readMaybe)

$(deriveMemoizable ''CompilerFlavor)
$(deriveMemoizable ''BinPkgName)

withCompilerVersion :: CompilerFlavor -> FilePath -> (DebianVersion -> a) -> a
withCompilerVersion hc root f = f (newestAvailableCompiler hc root)

-- | Memoized version of newestAvailable'
newestAvailable :: BinPkgName -> FilePath -> Maybe DebianVersion
newestAvailable pkg root =
    memoize f (pkg, root)
    where
      f :: (BinPkgName, FilePath) -> Maybe DebianVersion
      f (pkg', root') = unsafePerformIO (newestAvailable' pkg' root')

-- | Look up the newest version of a deb available in the given changeroot.
newestAvailable' :: BinPkgName -> FilePath -> IO (Maybe DebianVersion)
newestAvailable' (BinPkgName name) root = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "newestAvailable: no such environment: " ++ show root)
  versions <- try $ chroot root $
                (readProcess "apt-cache" ["showpkg", name] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "newestAvailable failed in " ++ show root ++ ": " ++ show e
    Right (_ : versionLine : _) -> return . Just . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> return Nothing
    where
      chroot "/" = id
      chroot _ = useEnv root (return . force)

newestAvailableCompiler :: CompilerFlavor -> FilePath -> DebianVersion
newestAvailableCompiler hc root =
    case debName hc of
      Nothing -> error $ "newestAvailableCompiler - Unsupported CompilerFlavor: " ++ show hc
      Just pkg -> fromMaybe (error $ "newestAvailableCompiler - No versions of " ++ show hc ++ " available in " ++ show root) (newestAvailable pkg root)

newestAvailableCompilerId :: CompilerFlavor -> FilePath -> CompilerId
newestAvailableCompilerId hc root = compilerIdFromDebianVersion hc (newestAvailableCompiler hc root)

{-
-- | The IO portion of ghcVersion.  For there to be no version of ghc
-- available is an exceptional condition, it has been standard in
-- Debian and Ubuntu for a long time.
ghcNewestAvailableVersion :: CompilerFlavor -> FilePath -> IO DebianVersion
ghcNewestAvailableVersion hc root = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "ghcVersion: no such environment: " ++ show root)
  versions <- try $ chroot $
                (readProcess "apt-cache" ["showpkg", map toLower (show hc)] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "ghcNewestAvailableVersion failed in " ++ show root ++ ": " ++ show e
    Right (_ : versionLine : _) -> return . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> error $ "No version of ghc available in " ++ show root
    where
      chroot = case root of
                 "/" -> id
                 _ -> useEnv root (return . force)

-- | Memoize the CompilerId built for the newest available version of
-- the compiler package so we don't keep running apt-cache showpkg
-- over and over.
ghcNewestAvailableVersion' :: CompilerFlavor -> FilePath -> CompilerId
ghcNewestAvailableVersion' hc root =
    memoize f (hc, root)
    where
      f :: (CompilerFlavor, FilePath) -> CompilerId
      f (hc', root) = unsafePerformIO (g hc' root)
      g hc root = do
        ver <- ghcNewestAvailableVersion hc root
        let cid = compilerIdFromDebianVersion ver
        -- hPutStrLn stderr ("GHC Debian version: " ++ show ver ++ ", Compiler ID: " ++ show cid)
        return cid
-}

compilerIdFromDebianVersion :: CompilerFlavor -> DebianVersion -> CompilerId
compilerIdFromDebianVersion hc debVersion =
    let (Version ds ts) = greatestLowerBound debVersion (map (\ d -> Version [d] []) [0..]) in
    CompilerId hc (greatestLowerBound debVersion (map (\ d -> Version (ds ++ [d]) ts) [0..]))
#if MIN_VERSION_Cabal(1,21,0)
               Nothing
#endif
    where
      greatestLowerBound :: DebianVersion -> [Version] -> Version
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion (showVersion v) < b) xs

-- | General function to build a command line option that reads most
-- of the possible values for CompilerFlavor.
compilerFlavorOption :: forall a. (CompilerFlavor -> a -> a) -> OptDescr (a -> a)
compilerFlavorOption f =
    Option [] ["hc", "compiler-flavor"] (ReqArg readHC "COMPILER") "Build packages using this Haskell compiler"
    where
      -- Most of the constructors in CompilerFlavor are arity zero and
      -- all caps, though two are capitalized - Hugs and Helium.  This
      -- won't read those, and it won't read HaskellSuite String or
      -- OtherCompiler String
      readHC :: String -> a -> a
      readHC s = maybe (error $ "Invalid CompilerFlavor: " ++ show s) f (readMaybe (map toUpper s))

debName :: CompilerFlavor -> Maybe BinPkgName
debName hc =
    case map toLower (show hc) of
      s | any isSpace s -> Nothing
      s -> Just (BinPkgName s)
