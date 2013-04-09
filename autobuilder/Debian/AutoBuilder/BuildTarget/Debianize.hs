{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Monad (when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (isSuffixOf)
import Debian.AutoBuilder.Monads.Deb (MonadDeb)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Debianize (Atoms, compileArgs, Top(Top))
import qualified Debian.Debianize as Cabal
import Debian.Relation (BinPkgName(unBinPkgName))
import Debian.Repo (sub)
import Debian.Repo.Sync (rsync)
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (withArgs)
import System.FilePath ((</>), takeFileName)
import System.Process.Progress (verbosity)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: MonadDeb m => Atoms -> P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare defaultAtoms cache package' target =
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
                liftIO $ autobuilderCabal cache (P.flags package') dir defaultAtoms
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
                concatMap asCabalFlags pflags
    where
      ver = P.ghcVersion (P.params cache)

autobuilderCabal :: P.CacheRec -> [P.PackageFlag] -> FilePath -> Atoms -> IO ()
autobuilderCabal cache pflags debianizeDirectory defaultAtoms =
    withCurrentDirectory debianizeDirectory $
    do -- This will be false if the package has no debian/Debianize.hs script
       done <- collectPackageFlags cache pflags >>= Cabal.runDebianize
       when (not done) (withArgs [] (Cabal.debianization (Top ".") (return . applyPackageFlags pflags) defaultAtoms >>= Cabal.writeDebianization (Top ".")))

applyPackageFlags :: [P.PackageFlag] -> Atoms -> Atoms
applyPackageFlags flags atoms = foldr applyPackageFlag atoms flags

applyPackageFlag :: P.PackageFlag -> Atoms -> Atoms
applyPackageFlag x@(P.Maintainer _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.ExtraDep _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.ExtraDevDep _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.MapDep _ _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.DebVersion _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.Revision _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@(P.Epoch _ _) atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag x@P.NoDoc atoms = compileArgs (asCabalFlags x) atoms
applyPackageFlag (P.CabalDebian ss) atoms = compileArgs ss atoms
applyPackageFlag (P.ModifyAtoms f) atoms = f atoms
applyPackageFlag (P.RelaxDep _) x = x
applyPackageFlag (P.UDeb _) x = x
applyPackageFlag P.OmitLTDeps x = x -- I think this exists
applyPackageFlag (P.AptPin _) x = x
applyPackageFlag (P.CabalPin _) x = x
applyPackageFlag (P.DarcsTag _) x = x
applyPackageFlag (P.GitBranch _) x = x

asCabalFlags :: P.PackageFlag -> [String]
asCabalFlags (P.Maintainer s) = ["--maintainer", s]
asCabalFlags (P.ExtraDep s) = ["--build-dep", s]
asCabalFlags (P.ExtraDevDep s) = ["--dev-dep", s]
asCabalFlags (P.MapDep c d) = ["--map-dep", c ++ "=" ++ unBinPkgName d]
asCabalFlags (P.DebVersion s) = ["--deb-version", s]
asCabalFlags (P.Revision s) = ["--revision", s]
asCabalFlags (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
asCabalFlags P.NoDoc = ["--disable-haddock"]
asCabalFlags (P.CabalDebian ss) = ss
asCabalFlags (P.RelaxDep _) = []
asCabalFlags (P.UDeb _) = []
asCabalFlags P.OmitLTDeps = [] -- I think this exists
asCabalFlags (P.AptPin _) = []
asCabalFlags (P.CabalPin _) = []
asCabalFlags (P.ModifyAtoms _) = []
asCabalFlags (P.DarcsTag _) = []
asCabalFlags (P.GitBranch _) = []

-- | Apply a set of package flags to a cabal-debian configuration record.
{-
applyPackageFlag :: P.PackageFlag -> Cabal.Atoms -> Cabal.Atoms
applyPackageFlag x atoms = Cabal.compileArgs atoms (asCabalFlags x)
-}
