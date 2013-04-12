{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |Insert packages into a release, remove packages from a release.
module Debian.Repo.Insert
    ( scanIncoming
    , InstallResult(..)
    , deleteTrumped
    , deleteGarbage
    , deleteSourcePackages
    , resultToProblems
    , showErrors
    , explainError
    ) where

import Control.Monad ( filterM, foldM, when )
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B ( pack, unpack )
import qualified Data.ByteString.Lazy.Char8 as L ( fromChunks, readFile )
import Data.Digest.Pure.MD5 (md5)
import Data.Either ( partitionEithers, rights )
import Data.List ( group, sort, intercalate, sortBy, groupBy, isSuffixOf, partition )
import Data.Maybe ( catMaybes )
import qualified Data.Set as Set
import Debian.Changes ( ChangesFile(..), ChangedFileSpec(..), changesFileName )
import Debian.Control ( formatControl )
import qualified Debian.Control.ByteString as B ( Field'(Field), Paragraph, Field, Control'(Control), ControlFunctions(parseControlFromHandle), Control,
                                                  appendFields, fieldValue, modifyField, raiseFields, renameField )
import qualified Debian.Control.String as S ( Control'(Control), ControlFunctions(parseControlFromFile) )
import Debian.Relation (BinPkgName, PkgName)
import Debian.Repo.Changes ( findChangesFiles, poolDir', name, path )
import Debian.Repo.Monads.Apt (MonadApt)
import qualified Debian.Repo.Package as DRP ( sourceFilePaths, toBinaryPackage, binaryPackageSourceID, getPackages, releaseSourcePackages, releaseBinaryPackages, putPackages )
import Debian.Repo.PackageIndex ( packageIndexPath, packageIndexList, sourceIndexList )
import Debian.Repo.Release ( prepareRelease, signRelease, findReleases )
import Debian.Repo.Repository ( repoArchList )
import Debian.Release (SubSection(section), Section(..), ReleaseName, Arch(..), archName, parseSection', releaseName', sectionName, sectionName')
import Debian.Repo.Types ( BinaryPackageLocal, prettyBinaryPackage, binaryPackageName, PackageIDLocal, SourcePackage(sourcePackageID), sourcePackageName,
                           BinaryPackage(packageID, packageInfo), PackageID(packageIndex, packageVersion), prettyPackageID, PackageIndexLocal, PackageIndex(..),
                           PackageVersion(pkgVersion), Release(..), ReleaseInfo(releaseInfoAliases, releaseInfoComponents, releaseInfoName),
                           Layout(..), LocalRepository(LocalRepository, repoLayout, repoRoot), Repository(..), EnvPath, outsidePath,
                           releaseName, releaseComponents, releaseArchitectures )
import Debian.Version ( parseDebianVersion, DebianVersion, prettyDebianVersion )
import Extra.GPGSign ( PGPKey )
import Extra.Files ( writeAndZipFileWithBackup )
import Extra.Misc ( listDiff )
import System.FilePath ( splitFileName, (</>) )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile, renameFile )
import System.Exit ( ExitCode(..) )
import System.IO ()
import qualified System.Posix.Files as F ( createLink, fileSize, getFileStatus )
import System.Posix.Types ( FileOffset )
import System.Process ( runInteractiveCommand, waitForProcess )
import System.Process.Progress (quieter, qPutStr, qPutStrLn)
import qualified Text.Format as F ( Pretty(..) )
import Text.PrettyPrint.ANSI.Leijen (text, cat, pretty)

data InstallResult 
    = Ok
    | Failed [Problem]		-- Package can not currently be installed
    | Rejected [Problem]	-- Package can not ever be installed
    deriving (Show, Eq)

data Problem
    = NoSuchRelease ReleaseName
    | NoSuchSection ReleaseName [Section]
    | ShortFile FilePath FileOffset FileOffset
    | LongFile FilePath FileOffset FileOffset
    | MissingFile FilePath
    | BadChecksum FilePath String String
    | OtherProblem String
    deriving (Eq)

instance Show Problem where
    show (NoSuchRelease rel) = "NoSuchRelease  " ++ releaseName' rel
    show (NoSuchSection rel sect) = "NoSuchSection " ++ releaseName' rel ++ " " ++ show (map sectionName' sect)
    show (ShortFile path a b) = "ShortFile " ++ path ++ " " ++ show a ++ " " ++ show b
    show (LongFile path a b) = "LongFile " ++ path ++ " " ++ show a ++ " " ++ show b
    show (MissingFile path) = "MissingFile " ++ path
    show (BadChecksum path a b) = "BadChecksum " ++ path ++ " " ++ show a ++ " " ++ show b
    show (OtherProblem s) = "OtherProblem " ++ show s

nub :: (Ord a) => [a] -> [a]
nub = map head . group . sort

mergeResults :: [InstallResult] -> InstallResult
mergeResults results =
    doMerge Ok results
    where
      doMerge x [] = x
      doMerge x (Ok : more) = doMerge x more
      doMerge (Rejected p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Rejected p1) (Failed p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Failed p2 : more) = doMerge (Failed (p1 ++ p2)) more
      doMerge Ok (x : more) = doMerge x more

showErrors :: [InstallResult] -> String
showErrors errors = intercalate "\n" (map explainError (concat . map resultToProblems $ errors))

resultToProblems :: InstallResult -> [Problem]
resultToProblems Ok = []
resultToProblems (Failed x) = x
resultToProblems (Rejected x) = x

isError :: InstallResult -> Bool
isError Ok = False
isError _ = True

plural "do" [_] = "does"
plural "do" _ = "do"

plural "s" [_] = ""
plural "s" _ = "s"

plural _ _ = ""


explainError :: Problem -> String
explainError (NoSuchRelease dist) =
    ("\nThe distribution in the .changes file (" ++ releaseName' dist ++ ") does not exist.  There\n" ++
     "are three common reasons this might happen:\n" ++
     " (1) The value in the latest debian/changelog entry is wrong\n" ++
     " (2) A new release needs to be created in the repository.\n" ++
     "       newdist --root <root> --create-release " ++ releaseName' dist ++ "\n" ++
     " (3) A new alias needs to be created in the repository (typically 'unstable', 'testing', or 'stable'.)\n" ++
     "       newdist --root <root> --create-alias <existing release> " ++ releaseName' dist ++ "\n")
explainError (NoSuchSection dist components) =
    ("\nThe component" ++ plural "s" components ++ " " ++ intercalate ", " (map sectionName' components) ++
     " in release " ++ releaseName' dist ++ " " ++
     plural "do" components ++ " not exist.\n" ++
     "either the 'Section' value in debian/control was wrong or the section needs to be created:" ++
     concat (map (\ component -> "\n  newdist --root <root> --create-section " ++ releaseName' dist ++ "," ++ sectionName' component) components))
explainError (ShortFile path a b) =
    ("\nThe file " ++ path ++ "\n" ++
     "is shorter than it should be (expected: " ++ show a ++ ", actual: " ++ show b ++ ".)  This usually\n" ++
     "happens while the package is still being uploaded to the repository.")
explainError (LongFile path _ _) =
    ("\nThe file " ++ path ++
     "\nis longer than it should be.  This can happen when the --force-build\n" ++
     "option is used.  In this case the --flush-pool option should help.")
explainError (BadChecksum path _ _) =
    ("\nThe checksum of the file " ++ path ++ "\n" ++
     "is different from the value in the .changes file.\n" ++
     "This can happen when the --force-build option is used.  In this case the\n" ++
     "--flush-pool option should help.  It may also mean a hardware failure.")
explainError other = show other

-- | Find the .changes files in the incoming directory and try to 
-- process each.
scanIncoming :: MonadApt m => Bool -> Maybe PGPKey -> LocalRepository -> m ([ChangesFile], [(ChangesFile, InstallResult)])
scanIncoming createSections keyname repo@(LocalRepository root _ _) =
    (\ x -> qPutStrLn ("Uploading packages to " ++ outsidePath root ++ "/incoming") >> quieter 2 x) $
    do releases <- findReleases repo
       changes <- liftIO (findChangesFiles (outsidePath root </> "incoming"))
       case changes of
         [] -> qPutStrLn "Nothing to install."
         _ -> qPutStrLn ("To install:\n  " ++ (intercalate "\n  " . map (show . pretty) $ changes))
       results <- installPackages createSections keyname repo releases changes
       case results of
         [] -> return ()
         _ -> qPutStrLn ("Upload results:\n  " ++
                         (intercalate "\n  " . map (uncurry showResult) $ (zip changes results)))
       let (bad, good) = partition (isError . snd) (zip changes results)
       return (map fst good, bad)
    where
      showResult changes result =
          changesFileName changes ++ ": " ++
          case result of
            Ok -> "Ok"
            Failed lst -> "Failed -\n      " ++ (intercalate "\n      " $ map show lst)
            Rejected lst -> "Rejected -\n      " ++ (intercalate "\n      " $ map show lst)

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
                -> LocalRepository		-- ^ target repository
                -> [Release]			-- ^ Releases in target repository
                -> [ChangesFile]		-- ^ Package to be installed
                -> m [InstallResult]	-- ^ Outcome of each source package
installPackages createSections keyname repo@(LocalRepository root layout _) releases !changeFileList =
    do live <- findLive repo >>= return . Set.fromList
       (_, releases', results) <- foldM (installFiles root) (live, releases, []) changeFileList
       let results' = reverse results
       results'' <- liftIO $ updateIndexes root releases' results'
       -- The install is done, now we will try to clean up incoming.
       case elem Ok results'' of
         False ->
             return results''
         True ->
             mapM_ (liftIO . uncurry (finish root (maybe Flat id layout))) (zip changeFileList results'') >>
             mapM_ (liftIO . signRelease keyname) (catMaybes . map (findRelease releases) . nub . sort . map changeRelease $ changeFileList) >>
             return results''
    where
      -- Hard link the files of each package into the repository pool,
      -- but don't unlink the files in incoming in case of subsequent
      -- failure.
      installFiles :: MonadApt m => EnvPath -> (Set.Set FilePath, [Release], [InstallResult]) -> ChangesFile -> m (Set.Set FilePath, [Release], [InstallResult])
      installFiles root (live, releases, results) changes =
          findOrCreateRelease releases (changeRelease changes) >>=
          maybe (return (live, releases, Failed [NoSuchRelease (changeRelease changes)] : results)) installFiles'
          where
            installFiles' release =
                let sections = nub . sort . map (section . changedFileSection) . changeFiles $ changes in
                case (createSections, listDiff sections (releaseComponents release)) of
                  (_, []) -> installFiles'' release
                  (True, missing) ->
                      do qPutStrLn ("Creating missing sections: " ++ intercalate " " (map sectionName' missing))
                         release' <- case releaseRepo release of
                                       LocalRepo repo -> prepareRelease repo (releaseName release) [] missing (releaseArchitectures release)
                                       x -> error $ "Expected local release: " ++ show x
                         installFiles'' release'
                  (False, missing) ->
                      return (live, releases, Failed [NoSuchSection (releaseName release) missing] : results)
            installFiles'' release' =
                do let releases' = release' : filter ((/= releaseName release') . releaseName) releases
                   result <- mapM (installFile root release') (changeFiles changes) >>= return . mergeResults
                   let live' =
                           case result of
                             -- Add the successfully installed files to the live file set
                             Ok -> foldr Set.insert live (map (((outsidePath root) </>) . poolDir' release' changes) (changeFiles changes))
                             _ -> live
                   return (live', releases', result : results)
{-
                   release' <-
                       case (createSections, listDiff sections (releaseComponents release)) of
                         (_, []) -> return release
                         (True, missing) ->
                             qPutStrLn ("Creating missing sections: " ++ show missing) >>
                             prepareRelease (releaseRepo release) (releaseName release) [] missing (releaseArchitectures release)
                         (False, missing) ->
                             error $ "Missing sections: " ++ show missing
                   let releases' = release' : filter ((/= releaseName release') . releaseName) releases
                   result <- mapM (installFile root release') (changeFiles changes) >>= return . mergeResults
                   let live' =
                           case result of
                             -- Add the successfully installed files to the live file set
                             Ok -> foldr Set.insert live (map (((show root) </>) . poolDir' release changes) (changeFiles changes))
                             _ -> live
                   return (live', releases', result : results)
-}
            installFile root release file =
                do let dir = outsidePath root </> poolDir' release changes file
                   let src = outsidePath root </> "incoming" </> changedFileName file
                   let dst = dir </> changedFileName file
                   installed <- liftIO $ doesFileExist dst
                   available <- liftIO $ doesFileExist src
                   let indexed = Set.member dst live
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
          do (pairLists :: [Either InstallResult [(PackageIndexLocal, B.Paragraph)]]) <-
                 mapM (uncurry $ buildInfo root releases) (zip changeFileList results)
             let sortedByIndex = sortBy compareIndex (concat (keepRight pairLists))
             let groupedByIndex = undistribute (groupBy (\ a b -> compareIndex a b == EQ) sortedByIndex)
             result <- addPackagesToIndexes groupedByIndex
             case result of
               Ok -> return $ map (either id (const Ok)) pairLists
               problem -> return $ map (const problem) results 
          where
            compareIndex :: (PackageIndexLocal, B.Paragraph) -> (PackageIndexLocal, B.Paragraph) -> Ordering
            compareIndex (a, _) (b, _) = compare a b
      -- Build the control information to be added to the package indexes.
      buildInfo :: EnvPath -> [Release] -> ChangesFile -> InstallResult -> IO (Either InstallResult [(PackageIndexLocal, B.Paragraph)])
      buildInfo root releases changes Ok =
          do case findRelease releases (changeRelease changes) of
               Just release ->
                   do (info :: [Either InstallResult B.Paragraph]) <- mapM (fileInfo root release) indexFiles
                      case keepLeft info of
                        [] ->
                            let (pairs :: [([PackageIndexLocal], Either InstallResult B.Paragraph)]) = zip (indexLists release) info in
                            let (pairs' :: [([PackageIndexLocal], B.Paragraph)]) =
                                    catMaybes $ map (\ (a, b) -> either (const Nothing) (\ b' -> Just (a, b')) b) pairs in
                            let (pairs'' :: [(PackageIndexLocal, B.Paragraph)]) = concat (map distribute pairs') in
                            return (Right pairs'')
                        results -> return (Left (mergeResults results))
               Nothing -> return . Left . Failed $ [NoSuchRelease (changeRelease changes)]
          where
            indexLists :: Release -> [[PackageIndexLocal]]
            indexLists release = map (indexes release) indexFiles
            indexes :: Release  -> ChangedFileSpec -> [PackageIndexLocal]
            indexes release file = map (PackageIndex release (section . changedFileSection $ file)) (archList release changes file)
            indexFiles = dsc ++ debs
            (debs :: [ChangedFileSpec]) = filter f files
                where (f :: ChangedFileSpec -> Bool) = (isSuffixOf ".deb" . changedFileName)
                      (files :: [ChangedFileSpec]) = (changeFiles changes)
            dsc = filter (isSuffixOf ".dsc" . changedFileName) (changeFiles changes)
            -- (debs, nonDebs) = partition (isSuffixOf ".deb" . changedFileName) (changeFiles changes)
            -- (indepDebs, archDebs) = partition (isSuffixOf "_all.deb" . changedFileName) debs
            -- (dsc, other) = partition (isSuffixOf ".dsc" . changedFileName) nonDebs
            --fileIndex release file = map (PackageIndex release (section . changedFileSection $ file)) (archList release changes file)
            fileInfo :: EnvPath -> Release -> ChangedFileSpec -> IO (Either InstallResult B.Paragraph)
            fileInfo root release file =
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
                  addFields (Right !info) =
                      case isSuffixOf ".deb" . changedFileName $ file of
                        True -> addDebFields release changes file info
                        False -> addSourceFields release changes file info
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
      finish root layout !changes Ok =
          do --vPutStrBl 1 stderr $ "  finish Ok " ++ changesFileName changes
             mapM_ (removeFile . ((outsidePath root ++ "/incoming/") ++) . changedFileName) (changeFiles changes)
             installChangesFile root layout changes
      finish root _ !changes (Rejected _) =
          do --vPutStrBl 1 stderr $ "  finish Rejected " ++ changesFileName changes
             mapM_ (\ name -> moveFile (outsidePath root ++ "/incoming/" ++ name) (outsidePath root ++ "/reject/" ++ name))
                      (map changedFileName (changeFiles changes))
             moveFile (outsidePath root ++ "/incoming/" ++ Debian.Repo.Changes.name changes)
                                (outsidePath root ++ "/reject/" ++ Debian.Repo.Changes.name changes)
      finish _ _ !changes (Failed _) =
          do qPutStrLn $ "  Finish Failed " ++ changesFileName changes
             return ()
      installChangesFile :: EnvPath -> Layout -> ChangesFile -> IO ()
      installChangesFile root layout changes =
          liftIO (moveFile (Debian.Repo.Changes.path changes) dst)
          where dst = case layout of
                        Flat -> outsidePath root </> Debian.Repo.Changes.name changes
                        Pool -> outsidePath root ++ "/installed/" ++ Debian.Repo.Changes.name changes
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
          case filter (\ release -> elem name (releaseName release : releaseInfoAliases (releaseInfo release))) releases of
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
distribute (ilist, p) = map (\ i -> (i, p)) ilist

undistribute :: [[(a, b)]] -> [(a, [b])]
undistribute [] = []
undistribute ([] : tail) = undistribute tail
undistribute (((index, info) : items) : tail) =
    (index, info : map snd items) : undistribute tail

keepRight :: [Either a b] -> [b]
keepRight xs = catMaybes $ map (either (const Nothing) Just) xs

keepLeft :: [Either a b] -> [a]
keepLeft xs = catMaybes $ map (either Just (const Nothing)) xs

addDebFields :: Release -> ChangesFile -> ChangedFileSpec -> B.Paragraph -> (Either InstallResult B.Paragraph)
addDebFields release changes file info =
    let (binaryVersion :: DebianVersion) =
            maybe (error $ "Missing 'Version' field") (parseDebianVersion . B.unpack) (B.fieldValue "Version" info) in
    let (newfields :: [B.Field]) =
            [B.Field (B.pack "Source", B.pack (" " ++ source ++ versionSuffix binaryVersion)),
             B.Field (B.pack "Filename", B.pack (" " ++ poolDir' release changes file </> changedFileName file)),
             B.Field (B.pack "Size", B.pack (" " ++ show (changedFileSize file))),
             B.Field (B.pack "MD5sum", B.pack (" " ++ changedFileMD5sum file))] in
    Right $ B.appendFields newfields info
    where
      versionSuffix :: DebianVersion -> String
      versionSuffix binaryVersion = if binaryVersion /= sourceVersion then " (" ++ show (prettyDebianVersion sourceVersion) ++ ")" else ""
      source = maybe (error "Missing 'Source' field in .changes file") id (B.fieldValue "Source" (changeInfo changes))
      sourceVersion = changeVersion changes


addSourceFields :: Release -> ChangesFile -> ChangedFileSpec -> B.Paragraph -> (Either InstallResult B.Paragraph)
addSourceFields release changes file info =
    Right . append . raise . modify . rename $ info
    where
      rename = B.renameField (B.pack "Source") (B.pack "Package")
      modify = B.modifyField (B.pack "Files") (\ b -> (B.pack (B.unpack b ++ "\n " ++ filesLine file))) .
               B.modifyField (B.pack "Checksums-Sha1") (\ b -> (B.pack (B.unpack b ++ "\n " ++ sha1Line file))) .
               B.modifyField (B.pack "Checksums-Sha256") (\ b -> (B.pack (B.unpack b ++ "\n " ++ sha256Line file)))
      raise = B.raiseFields (== (B.pack "Package"))
      append = B.appendFields $ 
               [B.Field (B.pack "Priority", B.pack (" " ++ changedFilePriority file)),
                B.Field (B.pack "Section", B.pack  (" " ++ (sectionName (changedFileSection file)))),
                B.Field (B.pack "Directory", B.pack (" " ++ poolDir' release changes file))] ++
               maybe [] (\ s -> [B.Field (B.pack "Build-Info", B.pack (" " ++ s))]) (B.fieldValue "Build-Info" (changeInfo changes))
      filesLine file = changedFileMD5sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha1Line file = changedFileSHA1sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
      sha256Line file = changedFileSHA256sum file ++ " "  ++ show (changedFileSize file) ++ " " ++ changedFileName file
{-    
    let info' = B.renameField (B.pack "Source") (B.pack "Package") info in
    let info'' = B.modifyField (B.pack "Files") (\ b -> (B.pack (B.unpack b ++ "\n " ++ changedFileMD5sum file ++ " "  ++ 
                                                                 show (changedFileSize file) ++ " " ++
                                                                 changedFileName file))) info' in
    let info''' = B.raiseFields (== (B.pack "Package")) info'' in
    let newfields = [B.Field (B.pack "Priority", B.pack (" " ++ changedFilePriority file)),
                     B.Field (B.pack "Section", B.pack  (" " ++ (sectionName (changedFileSection file)))),
                     B.Field (B.pack "Directory", B.pack (" " ++ poolDir' release changes file))] ++
                    maybe [] (\ s -> [B.Field (B.pack "Build-Info", B.pack (" " ++ s))])
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
addPackagesToIndexes :: [(PackageIndexLocal, [B.Paragraph])] -> IO InstallResult
addPackagesToIndexes pairs =
    do oldPackageLists <- mapM DRP.getPackages (map fst pairs)
       case partitionEithers oldPackageLists of
         ([], _) -> 
             do let (oldPackageLists' :: [[BinaryPackageLocal]]) = rights oldPackageLists
                let (indexMemberFns :: [BinaryPackageLocal -> Bool]) = map indexMemberFn oldPackageLists'
                -- if none of the new packages are already in the index, add them
                case concat (map (uncurry filter) (zip indexMemberFns newPackageLists)) of
                  [] -> do mapM_ updateIndex (zip3 indexes oldPackageLists' newPackageLists)
                           return Ok
                  dupes -> return $ Failed [OtherProblem ("Duplicate packages: " ++ intercalate " " (map (show . prettyBinaryPackage) dupes))]
         (bad, _) -> return $ Failed (map (OtherProblem . show) bad)
{-
    do (oldPackageLists :: [[BinaryPackageLocal]]) <- mapM getPackages (map fst pairs) >>= return . rights
       -- package membership predicates
       let (indexMemberFns :: [BinaryPackageLocal -> Bool]) = map indexMemberFn oldPackageLists
       -- if none of the new packages are already in the index, add them
       case concat (map (uncurry filter) (zip indexMemberFns newPackageLists)) of
         [] -> do mapM (liftIO . updateIndex) (zip3 indexes oldPackageLists newPackageLists)
                  return Ok
         dupes -> return $ Failed [OtherProblem ("Duplicate packages: " ++ (intercalate " " (map show dupes)))]
-}
    where
      updateIndex (index, oldPackages, newPackages) = DRP.putPackages index (oldPackages ++ newPackages)
      indexes = map fst pairs
      indexMemberFn :: [BinaryPackageLocal] -> BinaryPackageLocal -> Bool
      indexMemberFn packages =
          let set = Set.fromList . map packageID $ packages
          in
            \package -> Set.member (packageID package) set
      newPackageLists = map (\ (index, info) -> map (DRP.toBinaryPackage index) info) pairs

-- |Delete any packages from a dist which are trumped by newer
-- packages.  These packages are not technically garbage because they
-- can still be installed by explicitly giving their version number to
-- apt, but it is not really a good idea to use them.
deleteTrumped :: Maybe PGPKey -> [Release] -> IO [Release]
deleteTrumped _ [] = error "deleteTrumped called with empty release list"
deleteTrumped keyname releases =
    case nub . map releaseRepo $ releases of
      [_] ->
          mapM findTrumped releases >>=
          return . partitionEithers >>=
          \ (bad, good) -> 
              case bad of 
                [] -> return (concat good) >>=
                      ifEmpty (qPutStr "deleteTrumped: nothing to delete") >>=
                      deleteSourcePackages keyname . map packageID
                _ -> error $ "Error reading package lists"
      [] -> error "internal error"
      repos -> error ("Multiple repositories passed to deleteTrumped:\n  " ++
                      (intercalate "\n  " $ map show repos) ++ "\n")
    where
      ifEmpty :: IO () -> [a] -> IO [a]
      ifEmpty action [] = do action; return []
      ifEmpty _ x = return x

-- | Return a list of packages in a release which are trumped by some
-- newer version.
findTrumped :: Release -> IO (Either String [BinaryPackage])
findTrumped release =
    do
      --ePutStr ("findTrumped " ++ show release)
      packages <- mapM DRP.getPackages (sourceIndexList release)
      case partitionEithers packages of
        ([], packages') ->
            do let groups = map newestFirst . groupByName . concat $ packages'
               mapM_ (qPutStrLn) (catMaybes . map formatGroup $ groups)
               return . Right . concat . map tail $ groups
        (bad, _) -> return (Left $ "Error reading source indexes: " ++ intercalate ", " (map show bad))
    where
      groupByName :: [BinaryPackage] -> [[BinaryPackage]]
      groupByName = groupBy equalNames . sortBy compareNames
      equalNames a b = binaryPackageName a == binaryPackageName b
      compareNames a b = compare (binaryPackageName a) (binaryPackageName b)
      newestFirst = sortBy (flip compareVersions)
      compareVersions a b = compare (pkgVersion a) (pkgVersion b)
      formatGroup :: [BinaryPackage] -> Maybe String
      formatGroup [] = Nothing
      formatGroup [_] = Nothing
      formatGroup (newest : other) =
          Just ("Trumped by " ++ show (F.pretty newest) ++ " in " ++ show (F.pretty (packageIndex (packageID newest))) ++ ":\n " ++
                intercalate "\n " (map (show . F.pretty) other))

-- | Collect files that no longer appear in any package index and move
-- them to the removed directory.  The .changes files are treated
-- specially: they don't appear in any index files, but the package
-- they belong to can be constructed from their name.
deleteGarbage :: MonadApt m => LocalRepository -> m LocalRepository
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
            let deadFiles = Set.toList (Set.difference (Set.fromList allFiles) (Set.fromList liveFiles))
            qPutStrLn ("Removing:\n  " ++ intercalate "\n  " (sort deadFiles) ++ "\n")
            mapM_ (liftIO . moveToRemoved root) deadFiles
            return repo
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
      getDirectoryPaths dir = getDirectoryContents dir >>= return . filter filterDots >>= return . map ((dir ++ "/") ++)
      filterDots "." = False
      filterDots ".." = False
      filterDots _ = True
      -- upload files only appear when we dupload from a flat repository to another.
      moveToRemoved root file =
          renameFile file (outsidePath root ++ "/removed/" ++ snd (splitFileName file))

-- Repository Accessors and Inquiries

-- | Return a list of all the files in a release which are
-- 'live', in the sense that they appear in some index files.
findLive :: MonadApt m => LocalRepository -> m [FilePath]
findLive (LocalRepository _ Nothing _) = return []	-- Repository is empty
findLive repo@(LocalRepository root (Just layout) _) =
    do releases <- findReleases repo
       sourcePackages <- mapM (liftIO . DRP.releaseSourcePackages) releases >>= return . map (either (error . show) id) >>= return . concat
       binaryPackages <- mapM (liftIO . DRP.releaseBinaryPackages) releases >>= return . map (either (error . show) id) >>= return . concat
       let sourceFiles = map ((outsidePath root ++ "/") ++) . concat . map DRP.sourceFilePaths $ sourcePackages
       let binaryFiles =
               map ((outsidePath root ++ "/") ++) . map B.unpack . catMaybes $ map (B.fieldValue "Filename" . packageInfo) binaryPackages
       let changesFiles = concat . map (changesFilePaths root layout releases) $ sourcePackages
       let uploadFiles = concat . map (uploadFilePaths root releases) $ sourcePackages
       return $ sourceFiles ++ binaryFiles ++ changesFiles ++ uploadFiles
    where
      changesFilePaths root Flat releases package =
          map ((outsidePath root ++ "/") ++) . changesFileNames releases $ package
      changesFilePaths root Pool releases package =
          map ((outsidePath root ++ "/installed/") ++) . changesFileNames releases $ package
      changesFileNames releases package = 
          map (\ arch -> intercalate "_" [show (pretty (sourcePackageName package)),
                                          show . prettyDebianVersion . packageVersion . sourcePackageID $ package,
                                          archName arch] ++ ".changes") (nub (concat (architectures releases)))
      uploadFilePaths root releases package = map ((outsidePath root ++ "/") ++) . uploadFileNames releases $ package
      uploadFileNames releases package =
          map (\ arch -> intercalate "_" [show (pretty (sourcePackageName package)),
                                          show . prettyDebianVersion . packageVersion . sourcePackageID $ package,
                                          archName arch] ++ ".upload") (nub (concat (architectures releases)))
      architectures releases = map head . group . sort . map releaseArchitectures $ releases

deleteSourcePackages :: Maybe PGPKey -> [PackageIDLocal BinPkgName] -> IO [Release]
deleteSourcePackages keyname packages =
    if Set.null invalid
    then qPutStrLn (unlines ("Removing packages:" : map (show . F.pretty) packages)) >>
         mapM getEntries indexes' >>=
         return . zip indexes' . map (partition victim) >>=
         mapM put
    else error "deleteSourcePackages: not a source index"
    where
      put (index, (junk, keep)) =
          when (junk /= []) (qPutStrLn ("Removing packages from " ++ show (F.pretty index) ++ ": " ++ intercalate " " (map (show . F.pretty . packageID) junk))) >>
          putIndex' keyname index keep
      indexes' = concatMap allIndexes (Set.toList indexes)
      (indexes, invalid) = Set.partition (\ index -> packageIndexArch index == Source) (Set.fromList (map packageIndex packages))
      allIndexes sourceIndex = packageIndexList (packageIndexRelease sourceIndex)
      -- Compute the id of the source package this entry is from, and see if
      -- it is one of the packages we are deleting.
      victim :: BinaryPackage -> Bool
      victim entry = Set.member (sourceIdent entry) (Set.fromList packages)
      sourceIdent entry =
          case packageIndexArch (packageIndex (packageID entry)) of
            Source -> packageID entry
            _ -> DRP.binaryPackageSourceID entry
      getEntries index = DRP.getPackages index >>= return . either (error . show) id
      putIndex' :: Maybe PGPKey -> PackageIndexLocal -> [BinaryPackageLocal] -> IO Release
      putIndex' keyname index entries =
          do let release@(Release {releaseRepo = LocalRepo repo}) = packageIndexRelease index
                 root = repoRoot repo
             putIndex root index entries
             signRelease keyname release
             return release
      putIndex :: EnvPath -> PackageIndexLocal -> [BinaryPackageLocal] -> IO (Either [String] ())
      putIndex root index packages =
                let text = L.fromChunks (formatControl (B.Control (map packageInfo packages))) in
                liftIO $ writeAndZipFileWithBackup (outsidePath root </> packageIndexPath index) text

instance PkgName name => F.Pretty (PackageID name) where
    pretty p = prettyPackageID p -- packageName p ++ "=" ++ show (prettyDebianVersion (packageVersion p))

instance F.Pretty PackageIndex where
    pretty i = text $
        intercalate "/" [show (F.pretty (releaseRepo . packageIndexRelease $ i)),
                         "dist",
		         (releaseName' . releaseInfoName . releaseInfo . packageIndexRelease $ i),
		         show (F.pretty (packageIndexComponent i)),
                         show (F.pretty (packageIndexArch i))]

instance F.Pretty Release where
    pretty r = cat [F.pretty (releaseRepo r), text " ", F.pretty (releaseInfo r)]

instance F.Pretty Repository where
    pretty (LocalRepo r) = text $ outsidePath (repoRoot r)
    pretty (VerifiedRepo s _) = text s
    pretty (UnverifiedRepo s) = text s

instance F.Pretty ReleaseInfo where
    pretty r = text $ intercalate " " (releaseName' (releaseInfoName r) : map (show . F.pretty) (releaseInfoComponents r))

instance F.Pretty Section where
    pretty (Section s) = text s

instance F.Pretty Arch where
    pretty x@(Binary _) = text $ "binary-" ++ archName x
    pretty x = text $ archName x

instance F.Pretty BinaryPackage where
    pretty p = F.pretty (packageID p)
