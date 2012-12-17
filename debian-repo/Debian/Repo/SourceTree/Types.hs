{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.SourceTree.Types
    ( SourceTree(..)
    , DebianSourceTree(..)
    , DebianBuildTree(..)
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Trans ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8 as L ( empty )
import Data.List ( nubBy, sortBy, intercalate )
import Debian.Changes ( ChangeLogEntry(..), parseEntries )
import Debian.Control.String ( Field'(Comment), Paragraph'(..), Control'(Control), ControlFunctions(parseControl), Control )
import Debian.Repo.SourceTree.Classes (SourceTreeC(..), DebianSourceTreeC(..), DebianBuildTreeC(..), findDebianBuildTrees)
import Debian.Repo.Sync (rsync)
import qualified Debian.Version as V ( version )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import System.FilePath ((</>))
import System.Process (proc, readProcessWithExitCode)
import System.Process.Progress (runProcess)

-- |Any directory containing source code.
data SourceTree =
    SourceTree {dir' :: FilePath} deriving Show

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
data DebianSourceTree =
    DebianSourceTree {tree' :: SourceTree,
                      control' :: Control,
                      entry' :: ChangeLogEntry}

-- |A Debian source tree plus a parent directory, which is where the
-- binary and source deb packages appear after a build.  Note that
-- topdir' </> subdir' == dir' . tree' . debTree'
data DebianBuildTree =
    DebianBuildTree {topdir' :: FilePath,
                     subdir' :: String,
                     debTree' :: DebianSourceTree}

instance SourceTreeC SourceTree where
    topdir = dir'
    copySourceTree tree dest = createDirectoryIfMissing True dest >> rsync [] (topdir tree) dest >> return (SourceTree dest)
    findSourceTree path =
        doesDirectoryExist path >>= \ exists ->
        case exists of
          False -> fail $ "No such directory: " ++ path
          True -> return $ SourceTree path

instance SourceTreeC DebianSourceTree where
    topdir = dir' . tree'
    copySourceTree tree dest = DebianSourceTree <$> copySourceTree (tree' tree) dest <*> pure (control' tree) <*> pure (entry' tree)
    findSourceTree path0 =
      findSourceTree path0 >>= \ tree ->
      readFile controlPath >>= return . parseControl controlPath >>= either (fail . show) (return . removeCommentParagraphs) >>= \ control ->
      -- We only read part of the changelog, so be careful that the file
      -- descriptor gets closed.
      withFile changelogPath ReadMode
        (\ handle ->
          hGetContents handle >>= \ log ->
          case parseEntries log of
            (Right entry : _) ->
              -- ePutStrLn ("findDebianSourceTree " ++ show path0 ++ " -> " ++ topdir tree) >>
              return (DebianSourceTree tree control entry)
            (Left msgs : _) -> error $ "Bad changelog entry in " ++ show changelogPath ++ ": " ++ intercalate ", " msgs
            [] -> return $ error $ "Empty changelog file: " ++ show changelogPath)
      where
        controlPath = path0 ++ "/debian/control"
        changelogPath = path0 ++ "/debian/changelog"
        removeCommentParagraphs :: Control' a -> Control' a
        removeCommentParagraphs (Control paragraphs) =
            Control (filter (not . isCommentParagraph) paragraphs)
            where
              isCommentParagraph (Paragraph fields) = all isCommentField fields
              isCommentField (Comment _) = True
              isCommentField _ = False

instance DebianSourceTreeC DebianSourceTree where
    debdir = dir' . tree'
    control = control'
    entry = entry'

instance SourceTreeC DebianBuildTree where
    topdir = topdir'
    copySourceTree build dest =
        copySource >>= copyTarball >>= return . moveBuild
        where
          copySource = rsync [] (topdir' build) dest
          -- copySource = DebianBuildTree <$> pure dest <*> pure (subdir' tree) <*> copySourceTree (debTree' tree) (dest </> subdir' tree)
          copyTarball (ExitFailure n) = error $ "Failed to copy source tree: " ++ topdir' build ++ " -> " ++ dest
          copyTarball ExitSuccess =
              do exists <- liftIO $ doesFileExist origPath
                 case exists of
                   False -> return (ExitSuccess, "", "")
                   True -> liftIO $ readProcessWithExitCode "cp" ["-p", origPath, dest ++ "/"] ""
          moveBuild (ExitFailure 1, _, _) = error $ "Failed to copy Tarball: " ++ origPath ++ " -> " ++ dest ++ "/"
          moveBuild (ExitSuccess, _, _) = build {topdir' = dest, debTree' = moveSource (debTree' build)}
          moveSource source = source {tree' = SourceTree {dir' = dest </> subdir build}}
          origPath = topdir build </> orig
          orig = name ++ "_" ++ version ++ ".orig.tar.gz"
          name = logPackage . entry $ build
          version = V.version . logVersion . entry $ build
    findSourceTree path =
        do trees <- findDebianBuildTrees path
           case nubBy eqNames trees of
             [_] -> return . head . sortBy cmpVers $ trees
             [] -> error $ "No source trees found in subdirectorys of " ++ path
             names -> error $ "Mutiple source package names found in " ++ path ++ ": " ++ show (map (logPackage . entry) names)
        where
          eqNames tree1 tree2 = (logPackage . entry $ tree1) == (logPackage . entry $ tree2)
          cmpVers tree1 tree2 = compare (logVersion . entry $ tree1) (logVersion . entry $ tree2)

instance DebianSourceTreeC DebianBuildTree where
    debdir t = topdir' t </> subdir' t
    control = control' . debTree'
    entry = entry' . debTree'

instance DebianBuildTreeC DebianBuildTree where
    subdir = subdir'
    findBuildTree path subdir = findSourceTree (path </> subdir) >>= return . DebianBuildTree path subdir
