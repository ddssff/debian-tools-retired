{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Cabal
    ( inputCabalization
    , inputCopyright
    , inputMaintainer
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Lens.Lazy (getL, setL)
import Data.Maybe
import Data.Set (toList)
import Data.Text (Text, pack)
import Debian.Debianize.Atoms as Debian (Atoms, packageDescription, compiler, maintainer, verbosity, compilerVersion, cabalFlagAssignments)
import Debian.Debianize.Utility (readFile', withCurrentDirectory)
import Debian.Policy (getDebianMaintainer, haskellMaintainer, parseMaintainer)
import Distribution.License (License(..))
import Distribution.Package (Package(packageId))
import Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, license, maintainer))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, intToVerbosity)
import System.Cmd (system)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.IO.Error (catchIOError)
import System.Posix.Files (setFileCreationMask)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputCabalization :: FilePath -> Atoms -> IO Atoms
inputCabalization top atoms =
    withCurrentDirectory top $ do
      descPath <- defaultPackageDesc vb
      genPkgDesc <- readPackageDescription vb descPath
      (compiler', _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration vb
      let compiler'' = case getL compilerVersion atoms of
                         (Just ver) -> compiler' {compilerId = CompilerId GHC ver}
                         _ -> compiler'
      case finalizePackageDescription (toList (getL cabalFlagAssignments atoms)) (const True) (Platform buildArch buildOS) (compilerId compiler'') [] genPkgDesc of
        Left e -> error $ "Failed to load cabal package description: " ++ show e
        Right (pkgDesc, _) -> do
          liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
          return $ setL compiler (Just compiler'') $
                   setL packageDescription (Just pkgDesc) $ atoms
    where
      vb = intToVerbosity' (getL verbosity atoms)

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

-- | Try to read the license file specified in the cabal package,
-- otherwise return a text representation of the License field.
inputCopyright :: PackageDescription -> IO Text
inputCopyright pkgDesc = readFile' (licenseFile pkgDesc) `catchIOError` handle
    where handle _e =
              do -- here <- getCurrentDirectory
                 -- hPutStrLn stderr ("Error reading " ++ licenseFile pkgDesc ++ " from " ++ here ++ ": " ++ show _e)
                 return . pack . showLicense . license $ pkgDesc

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

-- | Try to compute the debian maintainer from the maintainer field of the
-- cabal package, or from the value returned by getDebianMaintainer.
inputMaintainer :: Atoms -> IO (Maybe NameAddr)
inputMaintainer atoms =
    debianPackageMaintainer >>= maybe cabalPackageMaintainer (return . Just) >>=
                                maybe getDebianMaintainer (return . Just) >>=
                                maybe lastResortMaintainer (return . Just)
    where
      debianPackageMaintainer :: IO (Maybe NameAddr)
      debianPackageMaintainer = return (getL Debian.maintainer atoms)
      cabalPackageMaintainer :: IO (Maybe NameAddr)
      cabalPackageMaintainer = return $ case fmap Cabal.maintainer (getL packageDescription atoms) of
                                          Nothing -> Nothing
                                          Just "" -> Nothing
                                          Just x -> either (const Nothing) Just (parseMaintainer (takeWhile (\ c -> c /= ',' && c /= '\n') x))
      lastResortMaintainer :: IO (Maybe NameAddr)
      lastResortMaintainer = return (Just haskellMaintainer)

intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))
