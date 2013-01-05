{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Cabal.PackageDescription
    ( withSimplePackageDescription
    , inputCopyright
    , inputMaintainer
    , dataDirectory
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe
import Data.Text (Text, pack)
import Debian.Debianize.Utility (readFile', withCurrentDirectory)
import Data.Version (Version, showVersion)
import Debian.Policy (getDebianMaintainer, haskellMaintainer, parseMaintainer)
import Distribution.License (License(..))
import Distribution.Package (Package(packageId), PackageIdentifier(pkgName, pkgVersion), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (PackageDescription(package, licenseFile, license, package, maintainer), FlagName)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, intToVerbosity)
import System.Cmd (system)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath((</>))
import System.IO.Error (catchIOError)
import System.Posix.Files (setFileCreationMask)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))

withSimplePackageDescription :: FilePath -> Int -> Maybe Version -> [(FlagName, Bool)] -> (PackageDescription -> Compiler -> IO a) -> IO a
withSimplePackageDescription top vb compilerVersion cabalFlagAssignments action =
    do (pkgDesc, compiler) <- getSimplePackageDescription top vb compilerVersion cabalFlagAssignments
       action pkgDesc compiler

getSimplePackageDescription :: FilePath -> Int -> Maybe Version -> [(FlagName, Bool)] -> IO (PackageDescription, Compiler)
getSimplePackageDescription top vb compilerVersion cabalFlagAssignments =
    withCurrentDirectory top $ do
      descPath <- defaultPackageDesc (intToVerbosity' vb)
      genPkgDesc <- readPackageDescription (intToVerbosity' vb) descPath
      (compiler', _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration (intToVerbosity' vb)
      let compiler = case compilerVersion of
                       (Just ver) -> compiler' {compilerId = CompilerId GHC ver}
                       _ -> compiler'
      pkgDesc <- case finalizePackageDescription cabalFlagAssignments (const True) (Platform buildArch buildOS) (compilerId compiler) [] genPkgDesc of
                   Left e -> error $ "finalize failed: " ++ show e
                   Right (pd, _) -> return pd
      liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf (intToVerbosity' vb) pkgDesc
      return (pkgDesc, compiler)

-- | Run the package's configuration script.
autoreconf :: Verbosity -> PackageDescription -> IO ()
autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

-- | This is the directory where the files listed in the Data-Files
-- section of the .cabal file need to be installed.
dataDirectory :: PackageDescription -> FilePath
dataDirectory pkgDesc =
    "usr/share" </> (pkgname ++ "-" ++ (showVersion . pkgVersion . package $ pkgDesc))
    where
      PackageName pkgname = pkgName . package $ pkgDesc

inputCopyright :: PackageDescription -> IO Text
inputCopyright pkgDesc = readFile' (licenseFile pkgDesc) `catchIOError` (\ _ -> return . pack . showLicense . license $ pkgDesc)

-- | Convert from license to RPM-friendly (now Debian-friendly?)
-- description.  The strings are taken from TagsCheck.py in the
-- rpmlint distribution.
showLicense :: License -> String
showLicense (Apache _) = "Apache"
showLicense (GPL _) = "GPL"
showLicense (LGPL _) = "LGPL"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD-like"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Non-distributable"
showLicense MIT = "MIT"
showLicense (UnknownLicense _) = "Unknown"

inputMaintainer :: Maybe NameAddr -> PackageDescription -> IO (Maybe NameAddr)
inputMaintainer maint pkgDesc =
    do m3 <- getDebianMaintainer
       return $ case (maint, parse cabalMaintainer, m3) of
                  (Just x, _, _) -> Just x
                  (_, Just x, _) -> Just x
                  (_, _, Just x) -> Just x
                  _ -> Just haskellMaintainer
    where
      parse :: Maybe String -> Maybe NameAddr
      parse Nothing = Nothing
      parse (Just s) = either (const Nothing) Just (parseMaintainer s)
      -- parse (Just s) = try (return (parseMaintainer s)) >>= return . either (\ (_ :: SomeException) -> Nothing) Just
      cabalMaintainer =
          case Cabal.maintainer pkgDesc of
            "" -> Nothing
            x -> Just $ takeWhile (\ c -> c /= ',' && c /= '\n') x
