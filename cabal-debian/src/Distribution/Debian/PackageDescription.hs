{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Debian.PackageDescription
    ( withSimplePackageDescription
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List
import Data.Maybe
import Distribution.Debian.Config (Flags(..))
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Package (Package(..))
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, intToVerbosity)
import Prelude hiding (catch)
import System.Cmd (system)
import System.Directory
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)

intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))

withSimplePackageDescription :: Flags -> (PackageDescription -> Compiler -> IO ()) -> IO ()
withSimplePackageDescription flags action = do
  descPath <- liftIO $ defaultPackageDesc (intToVerbosity' (verbosity flags))
  genPkgDesc <- liftIO $ readPackageDescription (intToVerbosity' (verbosity flags)) descPath
  when (compilerFlavor flags /= GHC) (error "Only the GHC compiler is supported.")
  (compiler', _) <- liftIO $ configCompiler (Just (compilerFlavor flags)) Nothing Nothing defaultProgramConfiguration (intToVerbosity' (verbosity flags))
  let compiler = case (compilerVersion flags, compilerFlavor flags) of
                   (Just v, ghc) -> compiler' {compilerId = CompilerId ghc v}
                   _ -> compiler'
  pkgDesc <- case finalizePackageDescription (configurationsFlags flags) (const True) (Platform buildArch buildOS) (compilerId compiler) [] genPkgDesc of
               Left e -> error $ "finalize failed: " ++ show e
               Right (pd, _) -> return pd
  --lbi <- localBuildInfo pkgDesc flags
  liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do autoreconf (intToVerbosity' (verbosity flags)) pkgDesc
                                                                               action pkgDesc compiler

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
