{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Cabal
    ( getSimplePackageDescription
    , inputCopyright
    , inputMaintainer
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe
import Data.Set (Set, toList)
import Data.Text (Text, pack)
import Data.Version (Version)
import Debian.Debianize.Atoms (setCompiler, setPackageDescription)
import Debian.Debianize.Utility (readFile', withCurrentDirectory)
import Debian.Debianize.Types.Atoms (HasAtoms, DebAtomKey(Source), DebAtom(DHMaintainer), lookupAtom)
import Debian.Policy (getDebianMaintainer, haskellMaintainer, parseMaintainer)
import Distribution.License (License(..))
import Distribution.Package (Package(packageId))
import Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, license, maintainer), FlagName)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, intToVerbosity)
import System.Cmd (system)
import System.Directory (doesFileExist {-, getCurrentDirectory-})
import System.Exit (ExitCode(..))
-- import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Files (setFileCreationMask)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))

{-
withSimplePackageDescription :: HasAtoms atoms => Int -> Maybe Version -> Set (FlagName, Bool) -> FilePath -> atoms -> (atoms -> IO a) -> IO a
withSimplePackageDescription verbosity compilerVersion cabalFlagAssignments top atoms action =
    do (pkgDesc, compiler) <- getSimplePackageDescription verbosity compilerVersion cabalFlagAssignments top
       let atoms' = setCompiler compiler . setPackageDescription pkgDesc $ atoms
       action atoms'
-}

getSimplePackageDescription :: HasAtoms atoms => Int -> Maybe Version -> Set (FlagName, Bool) -> FilePath -> atoms -> IO atoms
getSimplePackageDescription verbosity compilerVersion cabalFlagAssignments top atoms =
    withCurrentDirectory top $ do
      descPath <- defaultPackageDesc vb
      genPkgDesc <- readPackageDescription vb descPath
      (compiler', _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration vb
      let compiler = case compilerVersion of
                       (Just ver) -> compiler' {compilerId = CompilerId GHC ver}
                       _ -> compiler'
      pkgDesc <- case finalizePackageDescription (toList cabalFlagAssignments) (const True) (Platform buildArch buildOS) (compilerId compiler) [] genPkgDesc of
                   Left e -> error $ "finalize failed: " ++ show e
                   Right (pd, _) -> return pd
      liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
      return $ setCompiler compiler $ setPackageDescription pkgDesc $ atoms
    where
      vb = intToVerbosity' verbosity

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
inputMaintainer :: HasAtoms atoms => PackageDescription -> atoms -> IO (Maybe NameAddr)
inputMaintainer pkgDesc atoms =
    return (lookupAtom Source from atoms) >>=
    return . maybe (parse cabalMaintainer) Just >>=
    maybe getDebianMaintainer (return . Just) >>=
    return . maybe (Just haskellMaintainer) Just
    where
      from (DHMaintainer x) = Just x
      from _ = Nothing
      parse :: Maybe String -> Maybe NameAddr
      parse Nothing = Nothing
      parse (Just s) = either (const Nothing) Just (parseMaintainer s)
      -- parse (Just s) = try (return (parseMaintainer s)) >>= return . either (\ (_ :: SomeException) -> Nothing) Just
      cabalMaintainer =
          case Cabal.maintainer pkgDesc of
            "" -> Nothing
            x -> Just $ takeWhile (\ c -> c /= ',' && c /= '\n') x
