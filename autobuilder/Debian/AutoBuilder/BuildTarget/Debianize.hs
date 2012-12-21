{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Monad (when)
import Control.Monad.CatchIO (MonadCatchIO, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Debian.AutoBuilder.Monads.Deb (MonadDeb)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Relation (BinPkgName(unBinPkgName))
import Debian.Repo (sub)
import Debian.Repo.Sync (rsync)
import qualified Distribution.Debian as Cabal
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), takeFileName)
import System.Process.Progress (verbosity)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: MonadDeb m => P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare cache package' target =
    do dir <- sub ("debianize" </> takeFileName (T.getTop target))
       liftIO $ createDirectoryIfMissing True dir
       _ <- rsync [] (T.getTop target) dir
       cabfiles <- liftIO $ getDirectoryContents dir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
             do desc <- liftIO $ readPackageDescription normal (dir </> cabfile)
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . packageDescription $ desc
                -- We want to see the original changelog, so don't remove this
                -- removeRecursiveSafely (dir </> "debian")
                liftIO $ autobuilderCabal cache (P.flags package') dir Cabal.defaultFlags
                return $ T.Download { T.package = package'
                                    , T.getTop = dir
                                    , T.logText =  "Built from hackage, revision: " ++ show (P.spec package')
                                    , T.mVersion = Just version
                                    , T.origTarball = T.origTarball target
                                    , T.cleanTarget = \ top -> T.cleanTarget target top
                                    , T.buildWrapper = id }
         _ -> error $ "Download at " ++ dir ++ ": missing or multiple cabal files"

withCurrentDirectory :: MonadCatchIO m => FilePath -> m a -> m a
withCurrentDirectory new action = bracket (liftIO getCurrentDirectory >>= \ old -> liftIO (setCurrentDirectory new) >> return old) (liftIO . setCurrentDirectory) (\ _ -> action)

{-
-- | Run cabal-debian on the given directory, creating or updating the
-- debian subdirectory.  If the script in debian/Debianize.hs fails this
-- will throw an exception.
autobuilderDebianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
autobuilderDebianize cache pflags currentDirectory =
    withCurrentDirectory currentDirectory $
    do args <- collectPackageFlags cache pflags
       done <- Cabal.runDebianize args
       when (not done) (Cabal.callDebianize args)
-}

-- | Convert a set of package flags into the corresponding
-- cabal-debian command line options.  (Is this really in the IO monad
-- for a good reason?)
collectPackageFlags :: P.CacheRec -> [P.PackageFlag] -> IO [String]
collectPackageFlags cache pflags =
    do v <- verbosity
       return $ maybe [] (\ x -> ["--ghc-version", x]) ver ++
                ["--verbose=" ++ show v] ++
                concatMap pflag pflags
    where
      pflag (P.Maintainer s) = ["--maintainer", s]
      pflag (P.ExtraDep s) = ["--build-dep", s]
      pflag (P.ExtraDevDep s) = ["--dev-dep", s]
      pflag (P.MapDep c d) = ["--map-dep", c ++ "=" ++ unBinPkgName d]
      pflag (P.DebVersion s) = ["--deb-version", s]
      pflag (P.Revision s) = ["--revision", s]
      pflag (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
      pflag P.NoDoc = ["--disable-haddock"]
      pflag (P.CabalDebian ss) = ss
      pflag _ = []

      ver = P.ghcVersion (P.params cache)

autobuilderCabal :: P.CacheRec -> [P.PackageFlag] -> FilePath -> Cabal.Flags -> IO ()
autobuilderCabal cache pflags currentDirectory flags =
    withCurrentDirectory currentDirectory $
    do args <- collectPackageFlags cache pflags
       done <- Cabal.runDebianize args
       when (not done) (Cabal.debianize (foldr applyPackageFlag (Cabal.Config {Cabal.modifyAtoms = id, Cabal.flags = flags}) pflags))

-- | Apply a set of package flags to a cabal-debian configuration record.
applyPackageFlag :: P.PackageFlag -> Cabal.Config -> Cabal.Config
applyPackageFlag x config@(Cabal.Config {Cabal.flags = fs, Cabal.modifyAtoms = fn}) =
    case x of
      P.Maintainer s -> config {Cabal.flags = fs {Cabal.debMaintainer = Just s}}
      P.ExtraDep s -> config {Cabal.flags = fs {Cabal.buildDeps = s : Cabal.buildDeps fs}}
      P.ExtraDevDep s -> config {Cabal.flags = fs {Cabal.extraDevDeps = s : Cabal.extraDevDeps fs}}
      P.NoDoc -> config {Cabal.flags = fs {Cabal.haddock = False}}
      P.MapDep c d -> config {Cabal.flags = fs {Cabal.extraLibMap = Map.insertWith (++) c [d] (Cabal.extraLibMap fs)}}
      P.DebVersion s -> config {Cabal.flags = fs {Cabal.debVersion = Just s}}
      P.Revision s -> config {Cabal.flags = fs {Cabal.revision = s}}
      P.Epoch name d -> config {Cabal.flags = fs {Cabal.epochMap = if d >= 1 && d <= 9
                                                                   then Map.insert (PackageName name) d (Cabal.epochMap fs)
                                                                   else Cabal.epochMap fs}}
      P.CabalDebian ss -> config {Cabal.flags = Cabal.compileArgs ss fs}
      -- Compose a modifyAtoms argument with the current value
      P.ModifyAtoms fn' -> config {Cabal.modifyAtoms = fn' . fn}

      -- Flags that do not affect cabal-debian
      P.RelaxDep _ -> config
      P.UDeb _ -> config
      P.OmitLTDeps -> config
      P.AptPin _ -> config
      P.CabalPin _ -> config
      P.DarcsTag _ -> config
      P.GitBranch _ -> config
