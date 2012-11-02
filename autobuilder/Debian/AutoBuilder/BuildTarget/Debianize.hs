{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -Werror #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isSuffixOf)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Relation (PkgName(unPkgName), BinPkgName(unBinPkgName))
import Debian.Repo hiding (getVersion, pkgName, pkgVersion)
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..) {-, PackageName(..)-})
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getDirectoryContents)
import System.Exit
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(cwd), showCommandForUser)
import System.Unix.Directory (removeRecursiveSafely)
import System.Process (CmdSpec(RawCommand))
import System.Process.Read (readModifiedProcessWithExitCode)
import System.Process.Progress (qPutStrLn)
--import System.Unix.QIO (qPutStrLn)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: P.CacheRec -> P.Packages -> T.Download -> AptIOT IO T.Download
prepare cache package' cabal = liftIO $
    getDirectoryContents (T.getTop cabal) >>= return . filter (isSuffixOf ".cabal") >>= \ cabfiles ->
    case cabfiles of
      [cabfile] ->
          do desc <- readPackageDescription normal (T.getTop cabal </> cabfile)
             -- let (PackageName name) = pkgName . package . packageDescription $ desc
             let version = pkgVersion . package . packageDescription $ desc
             removeRecursiveSafely (T.getTop cabal </> "debian")
             -- tree <- findSourceTree (T.getTop cabal)
             debianize cache (P.flags package') (T.getTop cabal)
             return $ T.Download { T.package = package'
                                 , T.getTop = T.getTop cabal
                                 , T.logText =  "Built from hackage, revision: " ++ show (P.spec package')
                                 , T.mVersion = Just version
                                 , T.origTarball = T.origTarball cabal
                                 , T.cleanTarget = \ top -> T.cleanTarget cabal top
                                 , T.buildWrapper = id }
      _ -> error $ "Download at " ++ T.getTop cabal ++ " missing or multiple cabal files"

-- | Run cabal-debian on the given directory, creating a debian subdirectory.
debianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache pflags dir =
    do qPutStrLn ("debianizing " ++ dir)
       let pflags' = if any isMaintainerFlag pflags then pflags else P.Maintainer "Unknown Maintainer <unknown@debian.org>" : pflags
           args = (["--debianize"] ++
                   maybe [] (\ x -> ["--ghc-version", x]) ver ++
                   -- concatMap cflag cflags ++
                   concatMap pflag pflags')
       (code, out, err) <- run "cabal-debian" args (\ p -> p {cwd = Just dir}) B.empty
       case code of
         ExitFailure _ -> error (showCommandForUser "cabal-debian" args ++ "(in " ++ show dir ++ ") -> " ++ show code ++
                                 "\nStdout:\n" ++ indent " 1> " out ++ "\nStderr:\n" ++ indent " 2> " err)
         ExitSuccess -> return ()
    where
      indent pre s = unlines $ map (pre ++) $ lines $ B.unpack $ s
      pflag (P.Maintainer s) = ["--maintainer", s]
      pflag (P.ExtraDep s) = ["--build-dep", s]
      pflag (P.ExtraDevDep s) = ["--dev-dep", s]
      pflag (P.MapDep c d) = ["--map-dep", c ++ "=" ++ unPkgName (unBinPkgName d)]
      pflag (P.DebVersion s) = ["--deb-version", s]
      pflag (P.Revision s) = ["--revision", s]
      pflag (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
      pflag P.NoDoc = ["--disable-haddock"]
      pflag _ = []

      ver = P.ghcVersion (P.params cache)
      isMaintainerFlag (P.Maintainer _) = True
      isMaintainerFlag _ = False

      run cmd args cwd input =
          hPutStrLn stderr ("-> " ++ showCommandForUser cmd args ++ " (in " ++ show dir ++ ")") >>
          readModifiedProcessWithExitCode cwd (RawCommand cmd args) input
