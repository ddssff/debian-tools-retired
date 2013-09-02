{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- | Remove packages from a release.
module Debian.Repo.Delete
    ( deleteTrumped
    , deleteGarbage
    , deleteSourcePackages
    ) where

import Control.Exception (SomeException)
import Control.Monad (filterM, when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L (fromChunks)
import Data.Either (partitionEithers)
import Data.List as List (intercalate, sortBy, groupBy, isSuffixOf, partition, map)
import Data.Monoid (mconcat)
import Data.Set as Set (fromList, member, toList, difference, empty, null, partition, map, union, fold, toAscList)
import Data.Text as T (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Debian.Arch (Arch(..))
import Debian.Control (formatControl)
import qualified Debian.Control.Text as B (Control'(Control))
import Debian.Relation (BinPkgName)
import Debian.Repo.Insert (findLive)
import Debian.Repo.Monads.Apt (MonadApt)
import qualified Debian.Repo.Package as DRP (binaryPackageSourceID, getPackages)
import Debian.Repo.PackageIndex ( packageIndexPath, packageIndexList, sourceIndexList )
import Debian.Repo.Release (signRelease)
import Debian.Repo.Types ( BinaryPackageLocal, binaryPackageName, PackageIDLocal,
                           BinaryPackage(packageID, packageInfo), PackageID, PackageIndexLocal, PackageIndex(..),
                           PackageVersion(pkgVersion), Release,
                           EnvPath, outsidePath, Release(..))
import Debian.Repo.Types.Repo (repoKey)
import Debian.Repo.Types.Repository (Layout(..), LocalRepository, repoLayout, repoRoot, fromLocalRepository)
import Debian.Version.Text ()
import Extra.GPGSign ( PGPKey )
import Extra.Files ( writeAndZipFileWithBackup )
import System.FilePath ( splitFileName, (</>) )
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, renameFile)
import System.Process.Progress (qPutStr, qPutStrLn, ePutStrLn)
import qualified Text.Format as F ( Pretty(..) )

-- |Delete any packages from a dist which are trumped by newer
-- packages.  These packages are not technically garbage because they
-- can still be installed by explicitly giving their version number to
-- apt, but it is not really a good idea to use them.
deleteTrumped :: Bool -> Maybe PGPKey -> LocalRepository -> [Release] -> IO [Release]
deleteTrumped _ _ _ [] = error "deleteTrumped called with empty release list"
deleteTrumped dry keyname repo releases =
    mapM (findTrumped repo) releases >>=
    return . partitionEithers >>=
    \ (bad, good) ->
        case bad of
          [] -> return (concat good) >>=
                ifEmpty (qPutStr "deleteTrumped: nothing to delete") >>=
                deleteSourcePackages dry keyname repo . (List.map (\ (r, i, p) -> (r, i, packageID p)))
          _ -> error $ "Error reading package lists"
    where
      ifEmpty :: IO () -> [a] -> IO [a]
      ifEmpty action [] = do action; return []
      ifEmpty _ x = return x

-- | Return a list of packages in a release which are trumped by some
-- newer version.
findTrumped :: LocalRepository -> Release -> IO (Either String [(Release, PackageIndex, BinaryPackage)])
findTrumped repo release =
    do
      mapM doIndex (sourceIndexList release) >>= return . merge
    where
      doIndex index = DRP.getPackages (repoKey repo) release index >>= return . either Left (Right . (List.map (\ b -> (release, index, b))))
      merge :: [Either SomeException [(Release, PackageIndex, BinaryPackage)]] -> Either String [(Release, PackageIndex, BinaryPackage)]
      merge packages =
          case partitionEithers packages of
            ([], packages') -> Right . concat . List.map tail . List.map newestFirst . groupByName . concat $ packages'
            (bad, _) -> Left $ "Error(s) reading source indexes: " ++ intercalate ", " (List.map show bad)
{-
      --ePutStr ("findTrumped " ++ show release)
      packages <- mapM DRP.getPackages (sourceIndexList release)
      case partitionEithers packages of
        ([], packages') ->
            do let groups = List.map newestFirst . groupByName . concat $ packages'
               mapM_ (qPutStrLn) (catMaybes . List.map formatGroup $ groups)
               return . Right . concat . List.map tail $ groups
        (bad, _) -> return (Left $ "Error reading source indexes: " ++ intercalate ", " (List.map show bad))
-}

      groupByName :: [(Release, PackageIndex, BinaryPackage)] -> [[(Release, PackageIndex, BinaryPackage)]]
      groupByName = groupBy equalNames . sortBy compareNames
      equalNames (_, _, a') (_, _, b') = binaryPackageName a' == binaryPackageName b'
      compareNames (_, _, a') (_, _, b') = compare (binaryPackageName a') (binaryPackageName b')
      newestFirst = sortBy (flip compareVersions)
      compareVersions (_, _, a') (_, _, b') = compare (pkgVersion a') (pkgVersion b')
{-
      formatGroup :: [(Release, PackageIndex, BinaryPackage)] -> Maybe String
      formatGroup [] = Nothing
      formatGroup [_] = Nothing
      formatGroup ((release, index, newest) : other) =
          Just ("Trumped by " ++ show (F.pretty newest) ++ " in " ++ show (F.pretty (repo, release, index)) ++ ":\n " ++
                intercalate "\n " (List.map (\ (_, _, x) -> show (F.pretty x)) other))
-}

-- | Collect files that no longer appear in any package index and move
-- them to the removed directory.  The .changes files are treated
-- specially: they don't appear in any index files, but the package
-- they belong to can be constructed from their name.
deleteGarbage :: MonadApt m => LocalRepository -> m ()
deleteGarbage repo =
    case repoLayout repo of
      Just layout ->
          do
            qPutStrLn ("deleteGarbage in " ++ outsidePath root ++ " (layout=" ++ show layout ++ ")")
            allFiles1 <- liftIO $ poolFiles root layout
            allFiles2 <- liftIO $ changesFileList root layout
            let allFiles = allFiles1 ++ allFiles2
            -- ePutStr ("allFiles:\n  " ++ intercalate "\n  " (sort allFiles) ++ "\n")
            liveFiles <- findLive repo
            -- ePutStr ("liveFiles:\n  " ++ intercalate "\n  " (sort liveFiles) ++ "\n")
            let deadFiles = Set.difference (Set.map T.pack (Set.fromList allFiles)) liveFiles
            qPutStrLn ("Removing:\n  " ++ intercalate "\n  " (Set.toAscList (Set.map T.unpack deadFiles)) ++ "\n")
            mapM_ (liftIO . moveToRemoved root . T.unpack) (Set.toList deadFiles)
      _ -> error "Cannot remove files from an empty repository"
    where
      root = repoRoot repo
      poolFiles root Flat = getDirectoryContents (outsidePath root) >>=
                            filterM (doesFileExist . ((outsidePath root ++ "/") ++))
      poolFiles root Pool = 
          getSubPaths (outsidePath root ++ "/pool") >>=
          mapM getSubPaths >>= return . concat >>=
          mapM getSubPaths >>= return . concat >>=
          mapM getSubPaths >>= return . concat
      changesFileList root Pool = getDirectoryPaths (outsidePath root ++ "/installed")
      -- In this case we already got the .changes files from the top directory
      changesFileList root Flat = getDirectoryPaths (outsidePath root) >>= return . filter (isSuffixOf ".changes")
      getSubPaths path = 
          do
            isDir <- doesDirectoryExist path
            case isDir of
              False -> return [path]
              True -> getDirectoryPaths path
      getDirectoryPaths dir = getDirectoryContents dir >>= return . filter filterDots >>= return . List.map ((dir ++ "/") ++)
      filterDots "." = False
      filterDots ".." = False
      filterDots _ = True
      -- upload files only appear when we dupload from a flat repository to another.
      moveToRemoved root file =
          renameFile file (outsidePath root ++ "/removed/" ++ snd (splitFileName file))

-- | Delete specific source packages and their associated binary packages.
deleteSourcePackages :: Bool -> Maybe PGPKey -> LocalRepository -> [(Release, PackageIndex, PackageIDLocal BinPkgName)] -> IO [Release]
deleteSourcePackages _ _ _ [] = return []
deleteSourcePackages dry keyname repo packages =
    if Set.null invalid
    then qPutStrLn (unlines ("Removing packages:" : List.map (show . F.pretty . (\ (_, _, x) -> x)) packages)) >>
         mapM doIndex (Set.toList allIndexes)
    else error $ "deleteSourcePackages: not a source index: " ++ show (F.pretty invalid)
    where
      doIndex (release, index) = getEntries release index >>= put release index . List.partition (victim release index)
      put :: Release -> PackageIndex -> ([BinaryPackage], [BinaryPackage]) -> IO Release
      put release index ([], _) =
          qPutStrLn ("Nothing to remove from " ++ show index) >>
          return release
      put release index (junk, keep) =
          qPutStrLn ("Removing packages from " ++ show (F.pretty (fromLocalRepository repo, release, index)) ++ ":\n  " ++ intercalate "\n  " (List.map (show . F.pretty . packageID) junk)) >>
          putIndex' keyname release index keep
      allIndexes = Set.fold Set.union Set.empty (Set.map (\ (r, _) -> Set.fromList (List.map (r,) (packageIndexList r))) indexes) -- concatMap allIndexes (Set.toList indexes)
      (indexes, invalid) = Set.partition (\ (_, i) -> packageIndexArch i == Source) (Set.fromList (List.map (\ (r, i, _) -> (r, i)) packages))
      -- (source, invalid) = Set.partition (\ (r, i, b) -> packageIndexArch i == Source) (Set.fromList packages)
      -- (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (List.map fst packages))
      -- allIndexes (release, sourceIndex) = packageIndexList release

      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: Release -> PackageIndex -> BinaryPackage -> Bool
      victim release index binaryPackage = Set.member (sourceIdent (release, index, binaryPackage)) (Set.fromList packages)
      sourceIdent :: (Release, PackageIndex, BinaryPackage) -> (Release, PackageIndex, PackageID BinPkgName)
      sourceIdent (release, index, entry) =
          case packageIndexArch index of
            Source -> (release, index, packageID entry)
            _ -> (release, (index {packageIndexArch = Source}), DRP.binaryPackageSourceID index entry)
      getEntries :: Release -> PackageIndex -> IO [BinaryPackage]
      getEntries release index = DRP.getPackages (repoKey repo) release index >>= return . either (error . show) id
      putIndex' :: Maybe PGPKey -> Release -> PackageIndexLocal -> [BinaryPackageLocal] -> IO Release
      putIndex' keyname release index entries =
          do let root = repoRoot repo
             case dry of
               True -> ePutStrLn ("dry run: not changing " ++ show index)
               False -> putIndex root release index entries >> signRelease keyname repo release
             return release
      putIndex :: EnvPath -> Release -> PackageIndexLocal -> [BinaryPackageLocal] -> IO (Either [String] ())
      putIndex root release index packages =
                let text = formatControl (B.Control (List.map packageInfo packages)) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath release index) (L.fromChunks [encodeUtf8 (mconcat text)])
