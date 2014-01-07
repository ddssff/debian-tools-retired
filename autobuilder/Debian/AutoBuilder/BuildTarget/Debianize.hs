{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Monad (when)
import Control.Monad.State (modify)
import Control.Monad.Catch (MonadCatch, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (isSuffixOf)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Debianize as Cabal hiding (verbosity, withCurrentDirectory)
import Debian.Relation (prettyRelations)
import Debian.Repo.Prelude (rsync)
import Debian.Repo.State (MonadRepos)
import Debian.Repo.Top (MonadTop, sub)
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
prepare :: (MonadRepos m, MonadTop m) => DebT IO () -> P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare defaultAtoms cache package' target =
    do dir <- sub ("debianize" </> takeFileName (T.getTop target))
       liftIO $ createDirectoryIfMissing True dir
       _ <- rsync [] (T.getTop target) dir
       cabfiles <- liftIO $ getDirectoryContents dir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
             do desc <- liftIO $ readPackageDescription normal (dir </> cabfile)
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . Distribution.PackageDescription.packageDescription $ desc
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

withCurrentDirectory :: (MonadCatch m, MonadIO m) => FilePath -> m a -> m a
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

autobuilderCabal :: P.CacheRec -> [P.PackageFlag] -> FilePath -> DebT IO () -> IO ()
autobuilderCabal cache pflags debianizeDirectory defaultAtoms =
    withCurrentDirectory debianizeDirectory $
    do -- This will be false if the package has no debian/Debianize.hs script
       done <- collectPackageFlags cache pflags >>= runDebianizeScript
       when (not done) (withArgs [] (Cabal.evalDebT (debianization (Top ".") defaultAtoms (applyPackageFlags pflags) >>
                                                     writeDebianization (Top "."))
                                                    newAtoms))

applyPackageFlags :: [P.PackageFlag] -> DebT IO ()
applyPackageFlags flags = mapM_ applyPackageFlag flags

applyPackageFlag :: P.PackageFlag -> DebT IO ()
applyPackageFlag x@(P.Maintainer _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.BuildDep _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.DevelDep _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.MapDep _ _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.DebVersion _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.Revision _) = compileArgs (asCabalFlags x)
applyPackageFlag x@(P.Epoch _ _) = compileArgs (asCabalFlags x)
applyPackageFlag x@P.NoDoc = compileArgs (asCabalFlags x)
applyPackageFlag (P.CabalDebian ss) = compileArgs ss
applyPackageFlag (P.ModifyAtoms f) = modify f
applyPackageFlag (P.RelaxDep _) = return ()
applyPackageFlag (P.UDeb _) = return ()
applyPackageFlag P.OmitLTDeps = return () -- I think this exists
applyPackageFlag (P.AptPin _) = return ()
applyPackageFlag (P.CabalPin _) = return ()
applyPackageFlag (P.DarcsTag _) = return ()
applyPackageFlag (P.GitBranch _) = return ()

asCabalFlags :: P.PackageFlag -> [String]
asCabalFlags (P.Maintainer s) = ["--maintainer", s]
asCabalFlags (P.BuildDep s) = ["--build-dep", s]
asCabalFlags (P.DevelDep s) = ["--build-dep", s, "--dev-dep", s]
asCabalFlags (P.MapDep c d) = ["--map-dep", c ++ "=" ++ show (prettyRelations d)]
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
