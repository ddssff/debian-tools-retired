{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket, catch)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isSuffixOf)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Relation (PkgName(unPkgName), BinPkgName(unBinPkgName))
import Debian.Repo hiding (getVersion, pkgName, pkgVersion)
import qualified Distribution.Debian as Cabal
import qualified Distribution.Debian.Options as Cabal
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..) {-, PackageName(..)-})
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory, removeFile, doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeFileName)
import System.Process (CreateProcess(env))
--import System.Unix.Directory (removeRecursiveSafely)
import System.Process (CmdSpec(RawCommand))
import System.Process.Progress (qPutStrLn, runProcessVF, runProcess)
--import System.Unix.QIO (qPutStrLn)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: MonadDeb m => P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare cache package' cabal =
    do dir <- sub ("debianize" </> takeFileName (T.getTop cabal))
       liftIO $ createDirectoryIfMissing True dir
       _ <- runProcessVF id (RawCommand "rsync" ["-aHxSpDt", "--delete", T.getTop cabal ++ "/", dir]) B.empty
       cabfiles <- liftIO $ getDirectoryContents dir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
             do desc <- liftIO $ readPackageDescription normal (dir </> cabfile)
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . packageDescription $ desc
                -- We want to see the original changelog, so don't remove this
                -- removeRecursiveSafely (dir </> "debian")
                liftIO $ debianize cache (P.flags package') dir
                return $ T.Download { T.package = package'
                                    , T.getTop = dir
                                    , T.logText =  "Built from hackage, revision: " ++ show (P.spec package')
                                    , T.mVersion = Just version
                                    , T.origTarball = T.origTarball cabal
                                    , T.cleanTarget = \ top -> T.cleanTarget cabal top
                                    , T.buildWrapper = id }
         _ -> error $ "Download at " ++ dir ++ ": missing or multiple cabal files"

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new action = bracket (getCurrentDirectory >>= \ old -> setCurrentDirectory new >> return old) setCurrentDirectory (\ _ -> action)

-- | Run cabal-debian on the given directory, creating a debian subdirectory.
debianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache pflags dir =
    withCurrentDirectory dir $ qPutStrLn ("debianizing " ++ dir) >> runSetupConfigure >>= callDebianize
    where
      -- Try running Setup configure --builddir=debian to see if there
      -- is code in the Setup file to create a debianization.
      runSetupConfigure :: IO Bool
      runSetupConfigure =
          do removeFile "debian/compat" `catch` (\ (_ :: IOError) -> return ())
             -- Stash the flags from the package's autobuilder
             -- configuration in CABALDEBIAN, they may get picked up
             -- by the code in Setup.hs
             oldEnv <- filter (not . (== "CABALDEBIAN") . fst) <$> getEnvironment
             let newEnv = ("CABALDEBIAN", show (collectPackageFlags cache pflags)) : oldEnv
             _ <- runProcess (\ p -> p {env = Just newEnv}) (RawCommand "runhaskell" ["Setup", "configure", "--builddir=debian"]) B.empty
             doesFileExist "debian/compat" `catch` (\ (_ :: IOError) -> return False)
      -- Running Setup configure didn't produce a debianization, call
      -- the debianize function instead.
      callDebianize True = Cabal.debianize (compilePackageFlags cache pflags Cabal.defaultFlags)
      callDebianize False = return ()

compilePackageFlags :: P.CacheRec -> [P.PackageFlag] -> Cabal.Flags -> Cabal.Flags
compilePackageFlags cache pflags cflags =
    Cabal.compileArgs (collectPackageFlags cache pflags) cflags

collectPackageFlags :: P.CacheRec -> [P.PackageFlag] -> [String]
collectPackageFlags cache pflags =
    maybe [] (\ x -> ["--ghc-version", x]) ver ++ concatMap pflag pflags
    where
      pflag (P.Maintainer s) = ["--maintainer", s]
      pflag (P.ExtraDep s) = ["--build-dep", s]
      pflag (P.ExtraDevDep s) = ["--dev-dep", s]
      pflag (P.MapDep c d) = ["--map-dep", c ++ "=" ++ unPkgName (unBinPkgName d)]
      pflag (P.DebVersion s) = ["--deb-version", s]
      pflag (P.Revision s) = ["--revision", s]
      pflag (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
      pflag P.NoDoc = ["--disable-haddock"]
      pflag (P.CabalDebian ss) = ss
      pflag _ = []

      ver = P.ghcVersion (P.params cache)
