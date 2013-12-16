{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- |Install binary packages into and delete binary packages from a repository.
module Debian.Repo.Apt.Package
    ( scanIncoming
    , installPackages
    , deleteTrumped
    , deleteBinaryOrphans
    , deleteGarbage
    , deleteSourcePackages
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Exception as E (ErrorCall(ErrorCall), SomeException(..), try)
import Control.Monad (filterM, foldM, when)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, fromChunks, readFile)
import Data.Digest.Pure.MD5 (md5)
import Data.Either (partitionEithers, rights)
import Data.List as List (filter, groupBy, intercalate, intersperse, isSuffixOf, map, partition, sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.Set as Set (difference, empty, fold, fromList, insert, map, member, null, Set, size, toAscList, toList, union, unions)
import Data.Text as T (pack, Text, unpack)
import qualified Data.Text as T (concat)
import Data.Text.Encoding (encodeUtf8)
import Debian.Apt.Index (Compression(..), controlFromIndex)
import Debian.Arch (Arch(..), prettyArch)
import Debian.Changes (ChangedFileSpec(..), ChangesFile(..), changesFileName)
import Debian.Control (ControlFunctions(stripWS), formatControl, formatParagraph, Paragraph')
import qualified Debian.Control.Text as B (appendFields, Control, Control'(Control), ControlFunctions(lookupP), ControlFunctions(parseControlFromHandle), Field, Field'(Field), fieldValue, modifyField, Paragraph, raiseFields, renameField)
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile))
import Debian.Relation (BinPkgName, PkgName)
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (parseSection', ReleaseName, releaseName', Section(..), sectionName, sectionName', SubSection(section))
import Debian.Repo.Apt (MonadApt)
import Debian.Repo.Apt.Release (findReleases, prepareRelease, signRelease)
import Debian.Repo.Changes (changeName, changePath, findChangesFiles)
import Debian.Repo.EnvPath (EnvPath, outsidePath)
import Debian.Repo.InstallResult (InstallResult(..), isError, mergeResults, Problem(..))
import Debian.Repo.LocalRepository (Layout(..), LocalRepository, poolDir', repoLayout, repoReleaseInfoLocal, repoRoot)
import Debian.Repo.PackageID (makeBinaryPackageID, makeSourcePackageID, PackageID(packageName, packageVersion), prettyPackageID)
import Debian.Repo.PackageIndex (binaryIndexList, BinaryPackage(packageID, packageInfo), BinaryPackage(BinaryPackage, pConflicts, pDepends, pPreDepends, pProvides, pReplaces), PackageIndex(..), packageIndexList, packageIndexPath, prettyBinaryPackage, SourceControl(..), SourceFileSpec(SourceFileSpec, sourceFileName), sourceIndexList, SourcePackage(sourcePackageID), SourcePackage(SourcePackage, sourceControl, sourceDirectory, sourcePackageFiles, sourceParagraph))
import Debian.Repo.Prelude (nub')
import qualified Debian.Repo.Pretty as F (Pretty(..))
import Debian.Repo.Release (Release(releaseAliases, releaseComponents, releaseName, releaseArchitectures))
import Debian.Repo.Repo (Repo, repoArchList, repoKey, RepoKey, repoKeyURI)
import Debian.URI (fileFromURIStrict)
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.Version.Text ()
import Extra.Files (writeAndZipFileWithBackup)
import Extra.GPGSign (PGPKey)
import Extra.Misc (listDiff)
import Network.URI (URI(uriPath))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), splitFileName)
import System.IO ()
import qualified System.Posix.Files as F (createLink, fileSize, getFileStatus)
import System.Process (runInteractiveCommand, waitForProcess)
import System.Process.Progress (ePutStrLn, qPutStr, qPutStrLn)
import Text.PrettyPrint.ANSI.Leijen (cat, pretty, Pretty(..), text)
import Text.Regex (matchRegex, mkRegex, splitRegex)

-- | Find the .changes files in the incoming directory and try to 
-- process each.
scanIncoming :: MonadApt m => Bool -> Maybe PGPKey -> LocalRepository -> m ([ChangesFile], [(ChangesFile, InstallResult)])
scanIncoming createSections keyname repo =
    (\ x -> qPutStrLn ("Uploading packages to " ++ outsidePath (repoRoot repo) ++ "/incoming") >> {-quieter 2-} x) $
    do changes <- liftIO (findChangesFiles (outsidePath (repoRoot repo) </> "incoming"))
       case changes of
         [] -> qPutStrLn "Nothing to install."
         _ -> qPutStrLn ("To install:\n  " ++ (intercalate "\n  " . List.map (show . pretty) $ changes))
       results <- installPackages createSections keyname repo changes
       case results of
         [] -> return ()
         _ -> qPutStrLn ("Upload results:\n  " ++
                         (intercalate "\n  " . List.map (uncurry showResult) $ (zip changes results)))
       let (bad, good) = List.partition (isError . snd) (zip changes results)
       return (List.map fst good, bad)
    where
      showResult changes result =
          changesFileName changes ++ ": " ++
          case result of
            Ok -> "Ok"
            Failed lst -> "Failed -\n      " ++ (intercalate "\n      " $ List.map show lst)
            Rejected lst -> "Rejected -\n      " ++ (intercalate "\n      " $ List.map show lst)

-- | Install several packages into a repository.  This means
-- 1. getting the list of files from the .changes file,
-- 2. verifying the file checksums,
-- 3. deleting any existing version and perhaps other versions which
--    were listed in the delete list,
-- 4. updating the Packages and Sources files, and
-- 5. moving the files from the incoming directory to the proper
--    place in the package pool.
installPackages :: MonadApt m =>
                   Bool				-- ^ ok to create new releases and sections
                -> Maybe PGPKey		-- ^ key to sign repository with
                -> LocalRepository		-- ^ destination repository
                -> [ChangesFile]		-- ^ Package to be installed
                -> m [InstallResult]	-- ^ Outcome of each source package
installPackages createSections keyname repo changeFileList =
    do releases <- findReleases repo
       live <- findLive repo
       (_, releases', results) <- foldM (installFiles (repoRoot repo)) (live, releases, []) changeFileList
       let results' = reverse results
       results'' <- liftIO $ updateIndexes (repoRoot repo) releases' results'
       -- The install is done, now we will try to clean up incoming.
       case elem Ok results'' of
         False ->
             return results''
         True ->
             mapM_ (liftIO . uncurry (finish (repoRoot repo) (maybe Flat id (repoLayout repo)))) (zip changeFileList results'') >>
             mapM_ (liftIO . signRelease keyname repo) (catMaybes . List.map (findRelease releases) . nub' . List.map changeRelease $ changeFileList) >>
             return results''
    where
      -- Hard link the files of each package into the repository pool,
      -- but don't unlink the files in incoming in case of subsequent
      -- failure.
      installFiles :: MonadApt m => EnvPath -> (Set.Set Text, [Release], [InstallResult]) -> ChangesFile -> m (Set.Set Text, [Release], [InstallResult])
      installFiles root (live, releases, results) changes =
          findOrCreateRelease releases (changeRelease changes) >>=
          maybe (return (live, releases, Failed [NoSuchRelease (changeRelease changes)] : results)) installFiles'
          where
            installFiles' release =
                let sections = nub' . List.map (section . changedFileSection) . changeFiles $ changes in
                case (createSections, listDiff sections (releaseComponents release)) of
                  (_, []) -> installFiles'' release
                  (True, missing) ->
                      do qPutStrLn ("Creating missing sections: " ++ intercalate " " (List.map sectionName' missing))
                         release' <- prepareRelease repo (releaseName release) [] missing (releaseArchitectures release)
                         installFiles'' release'
                  (False, missing) ->
                      return (live, releases, Failed [NoSuchSection (releaseName release) missing] : results)
            installFiles'' release' =
                do let releases' = release' : filter ((/= (releaseName $ release')) . releaseName) releases
                   result <- mapM (installFile root) (changeFiles changes) >>= return . mergeResults
                   let live' =
                           case result of
                             -- Add the successfully installed files to the live file set
                             Ok -> foldr Set.insert live (List.map (T.pack . ((outsidePath root) </>) . poolDir' repo changes) (changeFiles changes))
                             _ -> live
                   return (live', releases', result : results)
            installFile root file =
                do let dir = outsidePath root </> poolDir' repo changes file
                   let src = outsidePath root </> "incoming" </> changedFileName file
                   let dst = dir </> changedFileName file
                   installed <- liftIO $ doesFileExist dst
                   available <- liftIO $ doesFileExist src
                   let indexed = Set.member (T.pack dst) live
                   case (available, indexed, installed) of
                     (False, _, _) ->			-- Perhaps this file is about to be uploaded
                         return (Failed [MissingFile src])
                     (True, False, False) ->		-- This just needs to be installed
			 liftIO (createDirectoryIfMissing True dir) >>
                         liftIO (F.createLink src dst) >>
                         return Ok
                     (True, False, True) ->		-- A garbage file is already present
                         qPutStrLn ("  Replacing unlisted file: " ++ dst) >>
			 liftIO (removeFile dst) >>
                         liftIO (F.createLink src dst) >>
                         return Ok
                     (True, True, False) ->		-- Apparantly the repository is damaged.
                         return (Failed [OtherProblem $ ("Missing from repository: " ++ dst)])
                     (True, True, True) ->		-- Further inspection is required
                         do installedSize <- liftIO $ F.getFileStatus dst >>= return . F.fileSize
                            installedMD5sum <- liftIO $ L.readFile dst >>= return . show . md5
                            case () of
                              _ | changedFileSize file < installedSize ->
	                            -- File may be in the process of being uploaded
                                    return (Failed [ShortFile dst (changedFileSize file) installedSize])
                              _ | changedFileSize file > installedSize ->
                                    -- We could skip this size test and just do the checksum test,
                                    -- but a file that is too long indicates a different type of
                                    -- failure than a file with a checksum mismatch.
                                    return (Rejected [LongFile dst (changedFileSize file) installedSize])
                              _ | changedFileMD5sum file /= installedMD5sum ->
                                    return (Rejected [BadChecksum dst (changedFileMD5sum file) installedMD5sum])
                              _ -> return Ok	-- The correct file is already installed - so be it.

      -- Update all the index files affected by the successful
      -- installs.  This is a time consuming operation, so we want to
      -- do this all at once, rather than one package at a time
      updateIndexes :: EnvPath -> [Release] -> [InstallResult] -> IO [InstallResult]
      updateIndexes root releases results =
          do (pairLists :: [Either InstallResult [((LocalRepository, Release, PackageIndex), B.Paragraph)]]) <-
                 mapM (uncurry $ buildInfo root releases) (zip changeFileList results)
             let sortedByIndex = sortBy compareIndex (concat (keepRight pairLists))
             let groupedByIndex = undistribute (groupBy (\ a b -> compareIndex a b == EQ) sortedByIndex)
             result <- addPackagesToIndexes groupedByIndex
             case result of
               Ok -> return $ List.map (either id (const Ok)) pairLists
               problem -> return $ List.map (const problem) results
          where
            compareIndex :: ((LocalRepository, Release, PackageIndex), B.Paragraph) -> ((LocalRepository, Release, PackageIndex), B.Paragraph) -> Ordering
            compareIndex (a, _) (b, _) = compare a b
      -- Build the control information to be added to the package indexes.
      buildInfo :: EnvPath -> [Release] -> ChangesFile -> InstallResult -> IO (Either InstallResult [((LocalRepository, Release, PackageIndex), B.Paragraph)])
      buildInfo root releases changes Ok =
          do case findRelease releases (changeRelease changes) of
               Just release ->
                   do (info :: [Either InstallResult B.Paragraph]) <- mapM (fileInfo root repo) indexFiles
                      case keepLeft info of
                        [] ->
                            let (pairs :: [([(LocalRepository, Release, PackageIndex)], Either InstallResult B.Paragraph)]) = zip (indexLists repo release) info in
                            let (pairs' :: [([(LocalRepository, Release, PackageIndex)], B.Paragraph)]) =
                                    catMaybes $ List.map (\ (a, b) -> either (const Nothing) (\ b' -> Just (a, b')) b) pairs in
                            let (pairs'' :: [((LocalRepository, Release, PackageIndex), B.Paragraph)]) = concat (List.map distribute pairs') in
                            return (Right pairs'')
                        results -> return (Left (mergeResults results))
               Nothing -> return . Left . Failed $ [NoSuchRelease (changeRelease changes)]
          where
            indexLists :: LocalRepository -> Release -> [[(LocalRepository, Release, PackageIndex)]]
            indexLists repo release = List.map (indexes repo release) indexFiles
            indexes :: LocalRepository -> Release -> ChangedFileSpec -> [(LocalRepository, Release, PackageIndex)]
            indexes repo release file = List.map (\ arch -> (repo, release, PackageIndex (section . changedFileSection $ file) arch)) (archList release changes file)
            indexFiles = dsc ++ debs
            (debs :: [ChangedFileSpec]) = filter f files
                where (f :: ChangedFileSpec -> Bool) = (isSuffixOf ".deb" . changedFileName)
                      (files :: [ChangedFileSpec]) = (changeFiles changes)
            dsc = filter (isSuffixOf ".dsc" . changedFileName) (changeFiles changes)
            -- (debs, nonDebs) = partition (isSuffixOf ".deb" . changedFileName) (changeFiles changes)
            -- (indepDebs, archDebs) = partition (isSuffixOf "_all.deb" . changedFileName) debs
            -- (dsc, other) = partition (isSuffixOf ".dsc" . changedFileName) nonDebs
            --fileIndex release file = List.map (PackageIndex release (section . changedFileSection $ file)) (archList release changes file)
            fileInfo :: EnvPath -> LocalRepository -> ChangedFileSpec -> IO (Either InstallResult B.Paragraph)
            fileInfo root repo file =
                getControl >>= return . addFields
                where
                  getControl :: IO (Either InstallResult B.Paragraph)
                  getControl =
                      do control <-
                             case isSuffixOf ".deb" . changedFileName $ file of
                               True -> getDebControl path
                               False -> liftIO $ S.parseControlFromFile path >>= return . either (Left . show) Right
                         case control of
                           Left message -> return . Left . Rejected $ [OtherProblem message]
                           Right (S.Control [info]) -> return (Right info)
                           Right (S.Control _) -> return . Left . Rejected $ [OtherProblem "Invalid control file"]
                  addFields :: (Either InstallResult B.Paragraph) -> (Either InstallResult B.Paragraph)
                  addFields (Left result) = Left result
                  addFields (Right info) =
                      case isSuffixOf ".deb" . changedFileName $ file of
                        True -> addDebFields repo changes file info
                        False -> addSourceFields repo changes file info
                  -- | Extract the control file from a binary .deb.
                  getDebControl :: FilePath -> IO (Either String B.Control)
                  getDebControl path =
                      do let cmd = "ar p " ++ path ++ " control.tar.gz | tar xzO ./control"
                         (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
                         control <- liftIO $ B.parseControlFromHandle cmd outh >>= return . either (Left . show) Right
                         exitcode <- liftIO $ waitForProcess handle
                         case exitcode of
                           ExitSuccess -> return control
                           ExitFailure n -> return . Left $ "Failure: " ++ cmd ++ " -> " ++ show n
                  path = outsidePath root ++ "/incoming/" ++ changedFileName file
      buildInfo _ _ _ notOk = return . Left $ notOk
      -- For a successful install this unlinks the files from INCOMING and
      -- moves the .changes file into INSTALLED.  For a failure it moves
      -- all the files to REJECT.
      finish root layout changes Ok =
          do --vPutStrBl 1 stderr $ "  finish Ok " ++ changesFileName changes
             mapM_ (removeFile . ((outsidePath root ++ "/incoming/") ++) . changedFileName) (changeFiles changes)
             installChangesFile root layout changes
      finish root _ changes (Rejected _) =
          do --vPutStrBl 1 stderr $ "  finish Rejected " ++ changesFileName changes
             mapM_ (\ name -> moveFile (outsidePath root ++ "/incoming/" ++ name) (outsidePath root ++ "/reject/" ++ name))
                      (List.map changedFileName (changeFiles changes))
             moveFile (outsidePath root ++ "/incoming/" ++ changeName changes)
                                (outsidePath root ++ "/reject/" ++ changeName changes)
      finish _ _ changes (Failed _) =
          do qPutStrLn $ "  Finish Failed " ++ changesFileName changes
             return ()
      installChangesFile :: EnvPath -> Layout -> ChangesFile -> IO ()
      installChangesFile root layout changes =
          liftIO (moveFile (changePath changes) dst)
          where dst = case layout of
                        Flat -> outsidePath root </> changeName changes
                        Pool -> outsidePath root ++ "/installed/" ++ changeName changes
      findOrCreateRelease :: MonadApt m => [Release] -> ReleaseName -> m (Maybe Release)
      findOrCreateRelease releases name =
          case createSections of
            False -> return (findRelease releases name)
            True -> do let release = findRelease releases name
                       case release of
                         Nothing ->
                             do newRelease <- prepareRelease repo name [] [parseSection' "main"] (repoArchList repo)
                                return (Just newRelease)
                         Just release -> return (Just release)
      findRelease :: [Release] -> ReleaseName -> Maybe Release
      findRelease releases name = 
          case filter (\ release -> elem name (releaseName release : releaseAliases release)) releases of
            [] -> Nothing
            [x] -> Just x
            _ -> error $ "Internal error 16 - multiple releases named " ++ releaseName' name

archList :: Release -> ChangesFile -> ChangedFileSpec -> [Arch]
archList release changes file =
    case () of
      _ | isSuffixOf "_all.deb" name -> releaseArchitectures release
      _ | isSuffixOf ".deb" name -> [changeArch changes]
      _ | isSuffixOf ".udeb" name -> []
      _ -> [Source]
    where name = changedFileName file

distribute :: ([a], b) -> [(a, b)]
distribute (ilist, p) = List.map (\ i -> (i, p)) ilist

undistribute :: [[(a, b)]] -> [(a, [b])]
undistribute [] = []
undistribute ([] : tail) = undistribute tail
undistribute (((index, info) : items) : tail) =
    (index, info : List.map snd items) : undistribute tail

keepRight :: [Either a b] -> [b]
keepRight xs = catMaybes $ List.map (either (const Nothing) Just) xs

keepLeft :: [Either a b] -> [a]
keepLeft xs = catMaybes $ List.map (either Just (const Nothing)) xs

addDebFields :: LocalRepository -> ChangesFile -> ChangedFileSpec -> Paragraph' Text -> (Either InstallResult (Paragraph' Text))
addDebFields repo changes file info =
    let (binaryVersion :: DebianVersion) =
            maybe (error $ "Missing 'Version' field") parseDebianVersion (B.fieldValue "Version" info) in
    let (newfields :: [B.Field]) =
            [B.Field (T.pack "Source", " " <> source <> T.pack (versionSuffix binaryVersion)),
             B.Field (T.pack "Filename", T.pack (" " ++ poolDir' repo changes file </> changedFileName file)),
             B.Field (T.pack "Size", T.pack (" " ++ show (changedFileSize file))),
             B.Field (T.pack "MD5sum", T.pack (" " ++ changedFileMD5sum file))] in
    Right $ B.appendFields newfields info
    where
      versionSuffix :: DebianVersion -> String
      versionSuffix binaryVersion = if binaryVersion /= sourceVersion then " (" ++ show (prettyDebianVersion sourceVersion) ++ ")" else ""
      source = maybe (error "Missing 'Source' field in .changes file") id (B.fieldValue "Source" (changeInfo changes))
      sourceVersion = changeVersion changes


addSourceFields :: LocalRepository -> ChangesFile -> ChangedFileSpec -> B.Paragraph -> (Either InstallResult B.Paragraph)
addSourceFields repo changes file info =
    Right . append . raise . modify . rename $ info
    where
      rename = B.renameField (T.pack "Source") (T.pack "Package")
      modify = B.modifyField (T.pack "Files") (\ b -> (T.pack (T.unpack b ++ "\n " ++ filesLine file))) .
               B.modifyField (T.pack "Checksums-Sha1") (\ b -> (T.pack (T.unpack b ++ "\n " ++ sha1Line file))) .
               B.modifyField (T.pack "Checksums-Sha256") (\ b -> (T.pack (T.unpack b ++ "\n " ++ sha256Line file)))
      raise = B.raiseFields (== (T.pack "Package"))
      append = B.appendFields $ 
               [B.Field (T.pack "Priority", T.pack (" " ++ changedFilePriority file)),
                B.Field (T.pack "Section", T.pack  (" " ++ (sectionName (changedFileSection file)))),
                B.Field (T.pack "Directory", T.pack (" " ++ poolDir' repo changes file))] ++
               maybe [] (\ s -> [B.Field (T.pack "Build-Info", " " <> s)]) (B.fieldValue "Build-Info" (changeInfo changes))
      filesLine file = changedFileMD5sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha1Line file = changedFileSHA1sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha256Line file = changedFileSHA256sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
{-    
    let info' = B.renameField (T.pack "Source") (T.pack "Package") info in
    let info'' = B.modifyField (T.pack "Files") (\ b -> (T.pack (B.unpack b ++ "\n " ++ changedFileMD5sum file ++ " "  ++ 
                                                                 show (changedFileSize file) ++ " " ++
                                                                 changedFileName file))) info' in
    let info''' = B.raiseFields (== (T.pack "Package")) info'' in
    let newfields = [B.Field (T.pack "Priority", T.pack (" " ++ changedFilePriority file)),
                     B.Field (T.pack "Section", T.pack  (" " ++ (sectionName (changedFileSection file)))),
                     B.Field (T.pack "Directory", T.pack (" " ++ poolDir' release changes file))] ++
                    maybe [] (\ s -> [B.Field (T.pack "Build-Info", T.pack (" " ++ s))])
                              (B.fieldValue "Build-Info" (changeInfo changes)) in
    Right $ B.appendFields newfields info'''    
-}

moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
    do --vPutStrBl 1 stderr ("moveFile " ++ src ++ " " ++ dst)
       doesFileExist dst >>= (flip when) (removeFile dst)
       F.createLink src dst
       removeFile src

-- |Add control information to several package indexes, making sure
-- that that no duplicate package ids are inserted.
addPackagesToIndexes :: [((LocalRepository, Release, PackageIndex), [B.Paragraph])] -> IO InstallResult
addPackagesToIndexes pairs =
    do oldPackageLists <- mapM (\ (repo, release, index) -> getPackages_ (repoKey repo) release index) (List.map fst pairs)
       case partitionEithers oldPackageLists of
         ([], _) -> 
             do let (oldPackageLists' :: [[BinaryPackage]]) = rights oldPackageLists
                let (indexMemberFns :: [BinaryPackage -> Bool]) = List.map indexMemberFn oldPackageLists'
                -- if none of the new packages are already in the index, add them
                case concat (List.map (uncurry filter) (zip indexMemberFns newPackageLists)) of
                  [] -> do mapM_ updateIndex (zip3 indexes oldPackageLists' newPackageLists)
                           return Ok
                  dupes -> return $ Failed [OtherProblem ("Duplicate packages: " ++ intercalate " " (List.map (show . prettyBinaryPackage) dupes))]
         (bad, _) -> return $ Failed (List.map (OtherProblem . show) bad)
    where
      updateIndex ((repo, release, index), oldPackages, newPackages) = putPackages_ repo release index (oldPackages ++ newPackages)
      indexes = List.map fst pairs
      indexMemberFn :: [BinaryPackage] -> BinaryPackage -> Bool
      indexMemberFn packages =
          let set = Set.fromList . List.map packageID $ packages
          in
            \package -> Set.member (packageID package) set
      newPackageLists = List.map (\ ((_repo, release, index), info) -> List.map (toBinaryPackage_ release index) info) pairs

-- Repository Accessors and Inquiries

-- | Return a list of all the files in a release which are
-- 'live', in the sense that they appear in some index files.
findLive :: MonadApt m => LocalRepository -> m (Set Text)
findLive repo = {-(LocalRepository _ Nothing _)-}
    case repoLayout repo of
      Nothing -> return Set.empty	-- Repository is empty
      Just layout ->
          do !releases <- findReleases repo
             !sourcePackages <- mapM (releaseSourcePackages_ (repoKey repo)) releases >>= return . Set.unions
             !binaryPackages <- mapM (releaseBinaryPackages_ (repoKey repo)) releases >>= return . Set.unions
             let sourceFiles = Set.map (T.pack (outsidePath (repoRoot repo) ++ "/") <>) . Set.map T.pack . Set.fold Set.union Set.empty . Set.map sourceFilePaths_ $ sourcePackages
             let binaryFiles = Set.map (T.pack (outsidePath (repoRoot repo) ++ "/") <>) . Set.fold (\ mt s -> maybe s (`Set.insert` s) mt) Set.empty $ Set.map (B.fieldValue "Filename" . packageInfo) binaryPackages
             let changesFiles = Set.map T.pack . Set.fold Set.union Set.empty $ Set.map (Set.fromList . changesFilePaths (repoRoot repo) layout releases) sourcePackages
             let uploadFiles = Set.map T.pack . Set.fold Set.union Set.empty . Set.map (uploadFilePaths (repoRoot repo) releases) $ sourcePackages
             return $ Set.unions [sourceFiles, binaryFiles, changesFiles, uploadFiles]
    where
      changesFilePaths root Flat releases package =
          List.map ((outsidePath root ++ "/") ++) . changesFileNames releases $ package
      changesFilePaths root Pool releases package =
          List.map ((outsidePath root ++ "/installed/") ++) . changesFileNames releases $ package
      changesFileNames releases package =
          List.map (\ arch -> intercalate "_" [show (pretty (packageName . sourcePackageID $ package)),
                                               show (prettyDebianVersion . packageVersion . sourcePackageID $ package),
                                               show (prettyArch arch)] ++ ".changes") (nub' (concat (architectures releases)))
      uploadFilePaths root releases package = Set.map ((outsidePath root ++ "/") ++) . uploadFileNames releases $ package
      uploadFileNames releases package =
          Set.map (\ arch -> intercalate "_" [show (pretty (packageName . sourcePackageID $ package)),
                                              show (prettyDebianVersion . packageVersion . sourcePackageID $ package),
                                              show (prettyArch arch)] ++ ".upload") (Set.fromList (concat (architectures releases)))
      architectures releases = nub' . List.map releaseArchitectures $ releases

instance (F.Pretty r, Repo r) => F.Pretty (r, Release, PackageIndex) where
    pretty (repo, r, i) = text $
        intercalate "/" [show (F.pretty repo),
                         "dist",
		         (releaseName' . releaseName $ r),
		         show (F.pretty (packageIndexComponent i)),
                         show (prettyArch (packageIndexArch i))]

instance F.Pretty (Release, PackageIndex) where
    pretty (r, i) = text $
        intercalate "/" [(releaseName' . releaseName $ r),
		         show (F.pretty (packageIndexComponent i)),
                         show (prettyArch (packageIndexArch i))]

instance (F.Pretty r, Repo r) => F.Pretty (r, Release) where
    pretty (repo, r) = cat [F.pretty repo, text " ", F.pretty r]

instance F.Pretty Release where
    pretty r = text $ intercalate " " (releaseName' (releaseName r) : List.map (show . F.pretty) (releaseComponents r))

instance F.Pretty Section where
    pretty (Section s) = text s

instance F.Pretty (Release, PackageIndex, PackageID BinPkgName) where
    pretty (r, i, b) = text $
        intercalate "/" [(releaseName' . releaseName $ r),
		         show (F.pretty (packageIndexComponent i)),
                         show (prettyArch (packageIndexArch i)),
                         show (F.pretty b)]

instance PkgName name => F.Pretty (PackageID name) where
    pretty p = prettyPackageID p -- packageName p ++ "=" ++ show (prettyDebianVersion (packageVersion p))

instance F.Pretty BinaryPackage where
    pretty p = F.pretty (packageID p)

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
      doIndex index = getPackages_ (repoKey repo) release index >>= return . either Left (Right . (List.map (\ b -> (release, index, b))))

-- |Delete any packages from a dist which are trumped by newer
-- packages.  These packages are not technically garbage because they
-- can still be installed by explicitly giving their version number to
-- apt, but it is not really a good idea to use them.
deleteBinaryOrphans :: (MonadIO m, Functor m) => Bool -> Maybe PGPKey -> LocalRepository -> [Release] -> m ()
deleteBinaryOrphans _ _ _ [] = error "deleteBinaryOrphans called with empty release list"
deleteBinaryOrphans dry keyname repo releases =
    do -- All the source packages in the repository
       ((exns1, sourcePackages) :: ([[SomeException]], [[[(Release, PackageIndex, SourcePackage)]]])) <- unzip <$> mapM (\ release -> partitionEithers <$> mapM (sourcePackagesOfIndex' (repoKey repo) release) (sourceIndexList release)) releases
       -- All the binary packages in the repository
       ((exns2, binaryPackages) :: ([[SomeException]], [[[(Release, PackageIndex, BinaryPackage)]]])) <- unzip <$> mapM (\ release -> partitionEithers <$> mapM (liftIO . getPackages' (repoKey repo) release) (binaryIndexList release)) releases
       case (concat exns1, concat exns2, concat (concat sourcePackages), concat (concat binaryPackages)) of
         ([], [], sps, bps) ->
             do let bps' = Set.fromList (List.map (\ (r, i, b) -> (r, i, packageID b)) bps)
                qPutStrLn ("Number of source packages: " ++ show (length sps))
                qPutStrLn ("Number of binary packages: " ++ show (size bps'))
                let -- The binary packages which are associated with
                    -- some source package.  These need to have their
                    -- architecture set from the release architecture
                    -- list and the source package architecture
                    goodBps :: Set (Release, PackageIndex, PackageID BinPkgName)
                    goodBps =
                        Set.fromList (concatMap f sps)
                        where
                          f (r, i, p) = concatMap (g r i) (sourcePackageBinaryIDs_ p)
                          g r i p' = List.map (h r i p') (releaseArchitectures r)
                          h r i p' a = (r, i {packageIndexArch = a}, p')
{-
                        Set.fromList (concatMap (\ (r, i, p) -> Set.fromList (concatMap (\ bid -> map (\ a -> (r, i {packageindexArch = a}, bid)) (releaseArchitectures r)) (sourcePackageBinaryIDs p)) in

                        Set.fromList (concatMap (\ (r, i, p) -> List.map (\ (a, p') -> (r, (i {packageIndexArch = a}), p')) (concatMap (\ a -> (a, sourcePackageBinaryIDs p)) (releaseArchitectures r))) sps)
-}
                    badBps :: Set (Release, PackageIndex, PackageID BinPkgName)
                    badBps = Set.difference bps' goodBps
                qPutStrLn ("deleteBinaryOrphans - keeping " ++ show (Set.size goodBps) ++ " packages.")
                qPutStrLn ("deleteBinaryOrphans - discarding " ++ show (Set.size badBps) ++ " packages.")
                liftIO $ deleteBinaryPackages dry keyname repo badBps
         (exns1', exns2', _, _) -> error $ "Failure(s) loading package indexes:\n " ++ intercalate "\n " (List.map show (exns1' ++ exns2'))
    where
      -- p :: (Release, PackageIndex, PackageID BinPkgName) -> Bool
      -- p (_, _, pid) = isInfixOf "fay" (unBinPkgName . packageName $ pid)
      getPackages' repo release index = either Left (Right . List.map (\ p -> (release, index, p))) <$> getPackages_ repo release index
      sourcePackagesOfIndex' repo release index = either Left (Right . List.map (\ p -> (release, index, p))) <$> sourcePackagesOfIndex_ repo release index
{-
         (Right sourcePackages', Right binaryPackages') ->
             do 
    mapM (findBinaryOrphans repo) releases >>=
    return . partitionEithers >>=
    \ (bad, good) ->
        case bad of
          [] -> return (concat good) >>=
                ifEmpty (qPutStr "deleteBinaryOrphans: nothing to delete") >>=
                deleteBinaryPackages dry keyname repo . (List.map (\ (r, i, p) -> (r, i, packageID p)))
          _ -> error $ "Error reading package lists"
    where
      ifEmpty :: IO () -> [a] -> IO [a]
      ifEmpty action [] = do action; return []
      ifEmpty _ x = return x
-}

-- | Return a list of binary packages in a release which have no
-- corresponding source package.  This never ought to happen, but due
-- to a broken newdist it did.
{-
findBinaryOrphans :: LocalRepository -> Release -> IO (Either String [(Release, PackageIndex, BinaryPackage)])
findBinaryOrphans repo release =
    mapM (doIndex (sourceIndexList release)) (binaryIndexList release) >>= return . merge
    where
      doIndex sourceIndexes binaryIndex =
          getPackages (repoKey repo) release binaryIndex >>=
          return . either Left (Right . (List.map (\ b -> (release, binaryIndex, b))))
-}

merge :: [Either SomeException [(Release, PackageIndex, BinaryPackage)]] -> Either String [(Release, PackageIndex, BinaryPackage)]
merge packages =
    case partitionEithers packages of
      ([], packages') -> Right . concat . List.map tail . List.map newestFirst . groupByName . concat $ packages'
      (bad, _) -> Left $ "Error(s) reading source indexes: " ++ intercalate ", " (List.map show bad)
    where
      groupByName :: [(Release, PackageIndex, BinaryPackage)] -> [[(Release, PackageIndex, BinaryPackage)]]
      groupByName = groupBy equalNames . sortBy compareNames
      equalNames (_, _, a') (_, _, b') = packageName (packageID a') == packageName (packageID b')
      compareNames (_, _, a') (_, _, b') = compare (packageName (packageID a')) (packageName (packageID b'))
      newestFirst = sortBy (flip compareVersions)
      compareVersions (_, _, a') (_, _, b') = compare (packageVersion (packageID a')) (packageVersion (packageID b'))

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
      changesFileList root Flat = getDirectoryPaths (outsidePath root) >>= return . List.filter (isSuffixOf ".changes")
      getSubPaths path = 
          do
            isDir <- doesDirectoryExist path
            case isDir of
              False -> return [path]
              True -> getDirectoryPaths path
      getDirectoryPaths dir = getDirectoryContents dir >>= return . List.filter filterDots >>= return . List.map ((dir ++ "/") ++)
      filterDots "." = False
      filterDots ".." = False
      filterDots _ = True
      -- upload files only appear when we dupload from a flat repository to another.
      moveToRemoved root file =
          renameFile file (outsidePath root ++ "/removed/" ++ snd (splitFileName file))

-- | Delete specific source packages and their associated binary packages.
deleteSourcePackages :: Bool -> Maybe PGPKey -> LocalRepository -> [(Release, PackageIndex, PackageID BinPkgName)] -> IO [Release]
deleteSourcePackages _ _ _ [] = return []
deleteSourcePackages dry keyname repo packages =
    do qPutStrLn ("deleteSourcePackages:\n " ++ intercalate "\n " (List.map (show . F.pretty . (\ (_, _, x) -> x)) packages))
       mapM doIndex (Set.toList allIndexes)
    where
      doIndex (release, index) = getEntries release index >>= put release index . List.partition (victim release index)
      put :: Release -> PackageIndex -> ([BinaryPackage], [BinaryPackage]) -> IO Release
      put release index ([], _) =
          qPutStrLn ("deleteSourcePackages - nothing to remove from " ++ show index) >>
          return release
      put release index (junk, keep) =
          qPutStrLn ("deleteSourcePackages  - Removing packages from " ++ show (F.pretty (repo, release, index)) ++ ":\n  " ++ intercalate "\n " (List.map (show . F.pretty . packageID) junk)) >>
          putIndex' keyname release index keep
      allIndexes = Set.fold Set.union Set.empty (Set.map (\ r -> Set.fromList (List.map (r,) (packageIndexList r))) (fromList (repoReleaseInfoLocal repo))) -- concatMap allIndexes (Set.toList indexes)
      -- (indexes, invalid) = Set.partition (\ (_, i) -> packageIndexArch i == Source) (Set.fromList (List.map (\ (r, i, _) -> (r, i)) (repoReleaseInfoLocal repo)))
      -- (source, invalid) = Set.partition (\ (r, i, b) -> packageIndexArch i == Source) (Set.fromList packages)
      -- (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (List.map fst packages))
      -- allIndexes (release, sourceIndex) = packageIndexList release

      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: Release -> PackageIndex -> BinaryPackage -> Bool
      victim release index binaryPackage = Set.member (sourceIdent (release, index, binaryPackage)) (Set.fromList packages)
      getEntries :: Release -> PackageIndex -> IO [BinaryPackage]
      getEntries release index = getPackages_ (repoKey repo) release index >>= return . either (error . show) id
      putIndex' :: Maybe PGPKey -> Release -> PackageIndex -> [BinaryPackage] -> IO Release
      putIndex' keyname release index entries =
          do let root = repoRoot repo
             case dry of
               True -> ePutStrLn ("dry run: not changing " ++ show index)
               False -> putIndex root release index entries >> signRelease keyname repo release
             return release
      putIndex :: EnvPath -> Release -> PackageIndex -> [BinaryPackage] -> IO (Either [String] ())
      putIndex root release index packages =
                let text = formatControl (B.Control (List.map packageInfo packages)) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath release index) (L.fromChunks [encodeUtf8 (mconcat text)])

-- | Delete specific source packages and their associated binary packages.
deleteBinaryPackages :: Bool -> Maybe PGPKey -> LocalRepository -> Set (Release, PackageIndex, PackageID BinPkgName) -> IO ()
deleteBinaryPackages _ _ _ s | Set.null s = return ()
deleteBinaryPackages dry keyname repo blacklist =
    mapM_ doIndex (Set.toList allIndexes)
    where
      doIndex (release, index) = getEntries release index >>= put release index . List.partition (victim release index)
      put :: Release -> PackageIndex -> ([BinaryPackage], [BinaryPackage]) -> IO Release
      put release index ([], _) =
          qPutStrLn ("deleteBinaryPackages - nothing to remove from " ++ show index) >>
          return release
      put release index (junk, keep) =
          qPutStrLn ("deleteBinaryPackages - removing " ++ show (length junk) ++ " packages from " ++ show (F.pretty (repo, release, index)) ++ ", leaving " ++ show (length keep) {- ++ ":\n " ++ intercalate "\n " (List.map (show . F.pretty . packageID) junk) -}) >>
          putIndex' keyname release index keep
      allIndexes = Set.fold Set.union Set.empty (Set.map (\ r -> Set.fromList (List.map (r,) (packageIndexList r))) (fromList (repoReleaseInfoLocal repo)))

      -- (invalid, indexes) = Set.partition (\ (_, i) -> packageIndexArch i == Source) (Set.fromList (List.map (\ (r, i, _) -> (r, i)) (toList packages)))
      -- (source, invalid) = Set.partition (\ (r, i, b) -> packageIndexArch i == Source) (Set.fromList packages)
      -- (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (List.map fst packages))
      -- allIndexes (release, sourceIndex) = packageIndexList release

      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: Release -> PackageIndex -> BinaryPackage -> Bool
      victim release index binaryPackage = Set.member (release, index, packageID binaryPackage) blacklist

      getEntries :: Release -> PackageIndex -> IO [BinaryPackage]
      getEntries release index = getPackages_ (repoKey repo) release index >>= return . either (error . show) id
      putIndex' :: Maybe PGPKey -> Release -> PackageIndex -> [BinaryPackage] -> IO Release
      putIndex' keyname release index entries =
          do let root = repoRoot repo
             case dry of
               True -> ePutStrLn ("dry run: not changing " ++ show index)
               False -> putIndex root release index entries >> signRelease keyname repo release
             return release
      putIndex :: EnvPath -> Release -> PackageIndex -> [BinaryPackage] -> IO (Either [String] ())
      putIndex root release index packages =
                let text = formatControl (B.Control (List.map packageInfo packages)) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath release index) (L.fromChunks [encodeUtf8 (mconcat text)])

sourceIdent :: (Release, PackageIndex, BinaryPackage) -> (Release, PackageIndex, PackageID BinPkgName)
sourceIdent (release, index, entry) =
    case packageIndexArch index of
      Source -> (release, index, packageID entry)
      _ -> (release, (index {packageIndexArch = Source}), binaryPackageSourceID_ index entry)

{-
uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""
-}

sourceFilePaths_ :: SourcePackage -> Set FilePath
sourceFilePaths_ package =
    Set.map ((sourceDirectory package) </>) . Set.map sourceFileName . Set.fromList . sourcePackageFiles $ package

toSourcePackage_ :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage_ index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . T.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case (parseSourcesFileList files, parseSourceParagraph package) of
            (Right files', Right para) ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID (T.unpack name) version
                , sourceParagraph = package
                , sourceControl = para
                , sourceDirectory = T.unpack directory
                , sourcePackageFiles = files' }
            (Left messages, _) -> error $ "Invalid file list: " ++ show messages
            (_, Left messages) -> error $ "Error in source paragraph\n package=" ++ show package ++ "\n  index=" ++ show index ++ "\n  messages:\n   " ++ intercalate "\n   " messages
      x -> error $ "Missing info in source package control information in " ++ show index ++ " -> " ++ show x ++ " :\n" ++ T.unpack (formatParagraph package)
    where
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: T.Text -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . List.map parseSourcesFiles . lines . T.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . List.map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . List.map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Either [String] SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    case (B.fieldValue "Package" p,
          B.fieldValue "Maintainer" p) of
      (Just source', Just maintainer') ->
          -- The optional fields can be parsed as pure values
          Right (SourceControl
                  { source = source'
                  , maintainer = maintainer'
                  , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
                  , packageSection = fmap stripWS $ B.fieldValue "Section" p
                  , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
                  , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
                  , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
                  , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
                  , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
                  , standardsVersion = fmap stripWS $ B.fieldValue "Standards-Version" p
                  , homepage = fmap stripWS $ B.fieldValue "Homepage" p })
      _x -> Left ["parseSourceParagraph - One or more required fields (Package, Maintainer, Standards-Version) missing: " ++ show p]

toBinaryPackage_ :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage_ release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID_ :: PackageIndex -> BinaryPackage -> PackageID BinPkgName
binaryPackageSourceID_ (PackageIndex _component _) package =
    case maybe Nothing (matchRegex re . T.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> makeBinaryPackageID name (packageVersion pid)
      Just [name, _, version] -> makeBinaryPackageID name (parseDebianVersion version)
      _ -> error "Missing Source attribute in binary package info"
    where
      -- sourceIndex = PackageIndex component Source
      pid = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryIDs_ :: SourcePackage -> [PackageID BinPkgName]
sourcePackageBinaryIDs_ package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> List.map (binaryID (parseDebianVersion (T.unpack version))) $ splitRegex (mkRegex "[ ,]+") (T.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = makeBinaryPackageID name version
      -- binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

-- | Get the contents of a package index
getPackages_ :: RepoKey -> Release -> PackageIndex -> IO (Either SomeException [BinaryPackage])
getPackages_ repo release index =
    fileFromURIStrict uri' >>= readControl . either (Left . SomeException) Right
    where
      readControl :: Either SomeException L.ByteString -> IO (Either SomeException [BinaryPackage])
      readControl (Left e) = return (Left e)
      readControl (Right s) =
          try (case controlFromIndex Uncompressed (show uri') s of
                 Left e -> return $ Left (SomeException (ErrorCall (show uri' ++ ": " ++ show e)))
                 Right (B.Control control) -> return (Right $ List.map (toBinaryPackage_ release index) control)) >>=
          return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id
      uri' = uri {uriPath = uriPath uri </> packageIndexPath release index}
      uri = repoKeyURI repo
      --toLazy s = L.fromChunks [s]
      --showStream :: Either Exception L.ByteString -> IO (Either Exception L.ByteString)
      --showStream x@(Left e) = hPutStrLn stderr (show uri' ++ " - exception: " ++ show e) >> return x
      --showStream x@(Right s) = hPutStrLn stderr (show uri' ++ " - stream length: " ++ show (L.length s)) >> return x

-- | Get the contents of a package index
binaryPackagesOfIndex_ :: {- MonadRepoCache k r -} MonadIO m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [BinaryPackage])
binaryPackagesOfIndex_ repo release index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> liftIO $ getPackages_ repo release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex_ :: {- MonadRepoCache k r -} MonadIO m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [SourcePackage])
sourcePackagesOfIndex_ repo release index =
    case packageIndexArch index of
      Source -> liftIO (getPackages_ repo release index) >>= return . either Left (Right . List.map (toSourcePackage_ index . packageInfo))
      _ -> return (Right [])

{-
indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> FilePath
indexPrefix repo release index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distro = releaseName $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s, []) -> [s]
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b
-}

-- | Return a list of all source packages.
releaseSourcePackages_ :: {- MonadRepoCache k r -} MonadIO m => RepoKey -> Release -> m (Set SourcePackage)
releaseSourcePackages_ repo release =
    mapM (sourcePackagesOfIndex_ repo release) (sourceIndexList release) >>= return . test
    where
      test :: [Either SomeException [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages_ :: {- MonadRepoCache k r -} MonadIO m => RepoKey -> Release -> m (Set BinaryPackage)
releaseBinaryPackages_ repo release =
    mapM (binaryPackagesOfIndex_ repo release) (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Write a set of packages into a package index.
putPackages_ :: LocalRepository -> Release -> PackageIndex ->  [BinaryPackage] -> IO ()
putPackages_ repo release index packages =
    writeAndZipFileWithBackup (outsidePath (repoRoot repo) </> packageIndexPath release index) (L.fromChunks [encodeUtf8 text]) >>= either (fail . intercalate "\n") return
    where
      text = T.concat (intersperse (T.pack "\n") . List.map formatParagraph . List.map packageInfo $ packages)

{-
readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)			-- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")	-- Debugging output
       return paragraphs
-}
