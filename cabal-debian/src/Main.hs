{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (bracket)
import Control.Monad (when)
import Data.List
import Data.Maybe
import Distribution.Debian.Config (Flags(..), DebAction(..))
import Distribution.Debian.Debianize (debianize)
import Distribution.Debian.Options (getFlags)
import Distribution.Debian.SubstVars (substvars)
import Distribution.Debian.Utility (buildDebVersionMap)
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Package (Package(..))
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (catch)
import System.Cmd (system)
import System.Directory
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)

main :: IO ()
main = do
  flags <- getFlags
  descPath <- defaultPackageDesc (verbosity flags)
  genPkgDesc <- readPackageDescription (verbosity flags) descPath
  case compilerFlavor flags of
    GHC -> do
         (compiler, pkgDesc) <- simplePackageDescription genPkgDesc flags
         let verbose = verbosity flags
         --lbi <- localBuildInfo pkgDesc flags
         debVersions <- buildDebVersionMap
         bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
               autoreconf verbose pkgDesc
               case debAction flags of
                 SubstVar name ->
                     do substvars flags pkgDesc compiler debVersions name
                 Debianize ->
                     debianize flags pkgDesc compiler (debOutputDir flags)
                 Usage ->
                     error "Unexpected debAction: usage"
    _ -> error "Only the GHC compiler is supported."

simplePackageDescription :: GenericPackageDescription -> Flags -> IO (Compiler, PackageDescription)
simplePackageDescription genPkgDesc flags = do
    (compiler', _) <- {- fchroot (buildRoot flags) -} (configCompiler (Just (compilerFlavor flags)) Nothing Nothing
                                                               defaultProgramConfiguration
                                                               (verbosity flags))
    let compiler = case (compilerVersion flags, compilerFlavor flags) of
                     (Just v, ghc) -> compiler' {compilerId = CompilerId ghc v}
                     _ -> compiler'
    --installed <- installedPackages
    case finalizePackageDescription (configurationsFlags flags)
          (const True) (Platform buildArch buildOS) (compilerId compiler)
          {- (Nothing :: Maybe PackageIndex) -}
          [] genPkgDesc of
      Left e -> die $ "finalize failed: " ++ show e
      Right (pd, _) -> return (compiler, pd)

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
