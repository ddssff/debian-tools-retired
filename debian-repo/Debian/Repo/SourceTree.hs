{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}
module Debian.Repo.SourceTree
    ( addLogEntry
    , buildDebs
    , DebianBuildTreeC (subdir, findBuildTree)
    , DebianSourceTreeC (entry, control, debdir)
    , SourceTreeC (copySourceTree, findSourceTree, topdir)
    , explainSourcePackageStatus
    , findChanges
    , findDebianBuildTrees
    , findDebianSourceTrees
    , findOneDebianBuildTree
    , findOrigTarball
    , origTarballPath
    , SourcePackageStatus (Indep, All, None)
    , DebianBuildTree (debTree', topdir')
    , DebianSourceTree(tree', control')
    , SourceTree(dir')
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception (evaluate, SomeException, try)
import Control.Monad (foldM)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as L (empty)
import Data.List (intercalate, nubBy, sortBy)
import Data.Text (Text)
import Data.Text.IO as T (readFile)
import Data.Time (NominalDiffTime)
import Debian.Changes (ChangeLogEntry(..), ChangesFile(..), parseEntries)
import Debian.Control.Text (Control, Control'(Control), ControlFunctions(parseControl), Field'(Comment), Paragraph'(..))
import Debian.Relation (BinPkgName)
import Debian.Repo.Changes (findChangesFiles)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.OSImage (MonadOS, osRoot)
import Debian.Repo.Prelude (access)
import Debian.Repo.Sync (rsync)
import qualified Debian.Version as V (version)
import "Extra" Extra.Files (getSubDirectories, replaceFile)
import "Extra" Extra.List (dropPrefix)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv, getEnvironment)
import System.Exit (ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (hGetContents, IOMode(ReadMode), withFile)
import System.Process (CmdSpec(..), CreateProcess(cwd, env, cmdspec), proc, readProcessWithExitCode, showCommandForUser)
import System.Process.Progress (keepResult, noisier, runProcessF, timeTask)
import System.Unix.Chroot (useEnv)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- |Any directory containing source code.
class SourceTreeC t where
    topdir :: t -> FilePath		-- ^The top directory of the source tree
    copySourceTree :: t -> FilePath -> IO t
    findSourceTree :: FilePath -> IO t

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
class SourceTreeC t => DebianSourceTreeC t where
    debdir :: t -> FilePath		-- ^The directory containing the debian subdirectory
    control :: t -> Control		-- ^The contents of debian\/control
    entry :: t -> ChangeLogEntry	-- ^The latest entry from debian\/changelog

-- |A debian source tree plus a parent directory, which is where the
-- binary and source deb packages appear after a build.
class DebianSourceTreeC t => DebianBuildTreeC t where
    subdir :: t -> String		-- ^The basename of debdir
    findBuildTree :: FilePath -> String -> IO t

-- |Find the .changes file which is generated by a successful run of
-- dpkg-buildpackage.
findChanges :: DebianBuildTreeC t => t -> IO ChangesFile
findChanges tree =
    do let dir = topdir tree
       result <- findChangesFiles dir
       case result of
         [cf] -> return cf
         [] -> fail ("Couldn't find .changes file in " ++ dir)
         lst -> fail ("Multiple .changes files in " ++ dir ++ ": " ++ intercalate ", " (map (show . pretty) lst))

-- |Rewrite the changelog with an added entry.
addLogEntry :: DebianSourceTreeC t => ChangeLogEntry -> t -> IO ()
addLogEntry entry'' debtree =
-- readFile changelogPath >>= replaceFile changelogPath . ((show (pretty entry'')) ++)
  withFile changelogPath ReadMode (\ handle -> hGetContents handle >>= replaceFile changelogPath . ((show (pretty entry'') ++ "\n\n") ++))
    where
      changelogPath = (debdir debtree) ++ "/debian/changelog"

-- |There are three possible results of a build: an upload consisting
-- of only the architecture independent debs (Indep), one including
-- both indep and binary debs (All), or with a failed build (None).
data SourcePackageStatus = All | Indep [BinPkgName] | None deriving (Show, Eq)

explainSourcePackageStatus :: SourcePackageStatus -> String
explainSourcePackageStatus All = "All architecture dependent files for the current build architecture are present."
explainSourcePackageStatus (Indep missing) = "Some or all architecture-dependent files for the current build architecture are missing: " ++ show missing
explainSourcePackageStatus None = "This version of the package is not present."

-- | Run dpkg-buildpackage in a build tree.
buildDebs :: (DebianBuildTreeC t, MonadOS m, MonadIO m) => Bool -> Bool -> [(String, Maybe String)] -> t -> SourcePackageStatus -> m NominalDiffTime
buildDebs noClean _twice setEnv buildTree status =
    do
      root <- rootPath <$> access osRoot
      noSecretKey <- liftIO $ getEnv "HOME" >>= return . (++ "/.gnupg") >>= doesDirectoryExist >>= return . not
      env0 <- liftIO getEnvironment
      -- Set LOGNAME so dpkg-buildpackage doesn't die when it fails to
      -- get the original user's login information
      let run cmd = timeTask . useEnv root forceList . noisier 3 $ runProcessF (Just (" 1> ", " 2> "))
                                                                               (cmd {env = Just (modEnv (("LOGNAME", Just "root") : setEnv) env0),
                                                                                     cwd = dropPrefix root path}) L.empty
      _ <- liftIO $ run (proc "chmod" ["ugo+x", "debian/rules"])
      let buildCmd = proc "dpkg-buildpackage" (concat [["-sa"],
                                                       case status of Indep _ -> ["-B"]; _ -> [],
                                                       if noSecretKey then ["-us", "-uc"] else [],
                                                       if noClean then ["-nc"] else []])
      (result, elapsed) <- liftIO $ run buildCmd
      case keepResult result of
        (ExitFailure n : _) -> fail $ "*** FAILURE: " ++ showCmd (cmdspec buildCmd) ++ " -> " ++ show n
        _ -> return elapsed
    where
      path = debdir buildTree
      showCmd (RawCommand cmd args) = showCommandForUser cmd args
      showCmd (ShellCommand cmd) = cmd

modEnv :: [(String, Maybe String)] -> [(String, String)] -> [(String, String)]
modEnv [] env0 = env0
modEnv pairs env0 = foldl modEnv1 env0 pairs
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

modEnv1 :: [(String, String)] -> (String, Maybe String) -> [(String, String)]
modEnv1 env0 (name, mvalue) = maybe [] (\ v -> [(name, v)]) mvalue ++ filter ((/= name) . fst) env0

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

findOrigTarball :: DebianBuildTreeC t => t -> IO (Maybe FilePath)
findOrigTarball tree =
    do exists <- doesFileExist (origTarballPath tree)
       return $ if exists then Just (origTarballPath tree) else Nothing

origTarballPath :: DebianBuildTreeC t => t -> FilePath
origTarballPath tree =
    topdir tree ++ "/" ++ orig
    where
      orig = name ++ "_" ++ version ++ ".orig.tar.gz"
      name = logPackage . entry $ tree
      version = V.version . logVersion . entry $ tree

-- deprecated
{-
copyDebianSourceTree :: (DebianSourceTreeC t) => t -> FilePath -> IO t
copyDebianSourceTree = copySourceTree

copyDebianBuildTree :: (DebianBuildTreeC t) => t -> FilePath -> IO t
copyDebianBuildTree = copySourceTree
-}

-- |Find all the debian source trees in a directory.
findDebianSourceTrees :: DebianSourceTreeC t => FilePath -> IO [(FilePath, t)]
findDebianSourceTrees path =
    getSubDirectories path >>= \ (subdirs :: [FilePath]) ->
    foldM (\ pairs subdir'' ->
               try (findSourceTree (path </> subdir'')) >>=
               either (\ (_ :: SomeException) -> return pairs) (\ tree -> return ((subdir'', tree) : pairs))) [] subdirs

-- |Find all the debian source trees in a directory.
findDebianBuildTrees :: DebianBuildTreeC t => FilePath -> IO [t]
findDebianBuildTrees path =
    getSubDirectories path >>=
    foldM (\ trees subdir'' ->
               try (findBuildTree path subdir'') >>=
               either (\ (_ :: SomeException) -> return trees) (\ tree -> return $ tree : trees)) []

-- |Find a DebianBuildTree inside a directory.  It finds all the
-- DebianSourceTrees, and if they all have the same package name it
-- returns the newest one according to the version numbers.  If there
-- are none, or there are trees with different package names, Nothing
-- is returned.
findOneDebianBuildTree :: DebianBuildTreeC t => FilePath -> IO (Maybe t)
findOneDebianBuildTree path =
    do trees <- findDebianBuildTrees path
       -- Do all the trees have the same package name?
       case nubBy eqNames trees of
         -- Yes, return the newest one
         [_] -> return . Just . head . sortBy cmpVers $ trees
         -- No trees found
         [] -> return Nothing
         -- No, throw an exception
         names -> error $ "findOneDebianBuildTree: more than one source package name found in " ++ path ++ ": " ++ show (map (logPackage . entry) names)
    where
      eqNames tree1 tree2 = (logPackage . entry $ tree1) == (logPackage . entry $ tree2)
      cmpVers tree1 tree2 = compare (logVersion . entry $ tree1) (logVersion . entry $ tree2)

-- |Any directory containing source code.
data SourceTree =
    SourceTree {dir' :: FilePath} deriving Show

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
data DebianSourceTree =
    DebianSourceTree {tree' :: SourceTree,
                      control' :: Control' Text,
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
      T.readFile controlPath >>= return . parseControl controlPath >>= either (fail . show) (return . removeCommentParagraphs) >>= \ (c :: Control' Text) ->
      -- We only read part of the changelog, so be careful that the file
      -- descriptor gets closed.
      withFile changelogPath ReadMode
        (\ handle ->
          hGetContents handle >>= \ l ->
          case parseEntries l of
            (Right e : _) ->
              -- ePutStrLn ("findDebianSourceTree " ++ show path0 ++ " -> " ++ topdir tree) >>
              return (DebianSourceTree tree c e)
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
          copyTarball (ExitFailure _) = error $ "Failed to copy source tree: " ++ topdir' build ++ " -> " ++ dest
          copyTarball ExitSuccess =
              do exists <- liftIO $ doesFileExist origPath
                 case exists of
                   False -> return (ExitSuccess, "", "")
                   True -> liftIO $ readProcessWithExitCode "cp" ["-p", origPath, dest ++ "/"] ""
          moveBuild (ExitFailure _, _, _) = error $ "Failed to copy Tarball: " ++ origPath ++ " -> " ++ dest ++ "/"
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
    findBuildTree path d = findSourceTree (path </> d) >>= return . DebianBuildTree path d
