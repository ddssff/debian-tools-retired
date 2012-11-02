{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
-- | The quilt target takes two other targets, one a base source
-- directory and another a quilt-style patch directory, and creates
-- a build target with the patches applied to the source directory.
module Debian.AutoBuilder.BuildTarget.Quilt where

import Control.Applicative.Error (Failing(..))
import Control.Exception (SomeException, try, throw)
import Control.Monad (when)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Time
import Data.Time.LocalTime ()
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (ChangeLogEntry(..), prettyEntry, parseLog, parseEntry)
import Debian.Repo (DebianSourceTreeC(debdir), SourceTreeC(topdir), SourceTree, findSourceTree, findOneDebianBuildTree, copySourceTree, AptIOT)
import Debian.Version
import Extra.Files (replaceFile)
import "Extra" Extra.List ()
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, renameDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (CmdSpec(..))
import System.Process.Progress (unpackOutputs, mergeToStderr, runProcessF, runProcess, qPutStrLn, quieter)
import Text.Regex

qMessage s x = qPutStrLn s >> return x

documentation = [ "quilt:(<target1>):(<target2>) - In a target of this form, target1 is"
                , "any source tree, and target2 is a quilt directory which contains"
                , "a debian style changelog file named 'changelog', a file named"
                , "'series' with a list of patch file names, and finally the patch"
                , "files listed in the series file.  The quilt system is used to apply"
                , "the patches to the source tree before building." ]

data EntryType = Base ChangeLogEntry | Patch ChangeLogEntry

getEntry (Base x) = x
getEntry (Patch x) = x

quiltPatchesDir = "quilt-patches"

makeQuiltTree :: P.CacheRec -> P.RetrieveMethod -> T.Download -> T.Download -> IO (SourceTree, FilePath)
makeQuiltTree cache m base patch =
    do qPutStrLn $ "Quilt base: " ++ T.getTop base
       qPutStrLn $ "Quilt patch: " ++ T.getTop patch
       -- This will be the top directory of the quilt target
       let copyDir = P.topDir cache ++ "/quilt/" ++ show (md5 (L.pack (show m)))
       liftIO (createDirectoryIfMissing True (P.topDir cache ++ "/quilt"))
       baseTree <- try (findSourceTree (T.getTop base))
       patchTree <- try (findSourceTree (T.getTop patch))
       case (baseTree, patchTree) of
         (Right baseTree, Right patchTree) ->
             do copyTree <- copySourceTree baseTree copyDir
                -- If this is already a DebianBuildTree we need to apply
                -- the patch to the subdirectory containing the DebianSourceTree.
                debTree <- findOneDebianBuildTree copyDir
                -- Compute the directory where the patches will be applied
                let quiltDir = maybe copyDir debdir debTree
                qPutStrLn $ "copyDir: " ++ copyDir
                qPutStrLn $ "quiltDir: " ++ quiltDir
                let patchDir = topdir patchTree
                -- Set up links to the quilt directory, and use quilt to get a
                -- list of the unapplied patches.
                let cmd1 = ("set -x && cd '" ++ quiltDir ++ "' && rm -f '" ++ quiltPatchesDir ++
                            "' && ln -s '" ++ patchDir ++ "' '" ++ quiltPatchesDir ++ "'")
                -- runTaskAndTest (linkStyle (commandTask cmd1))
                _output <- runProcessF id (ShellCommand cmd1) L.empty
                -- Now we need to have created a DebianSourceTree so
                -- that there is a changelog for us to reconstruct.
                return (copyTree, quiltDir)
         (Left (e :: SomeException), _) -> throw e
         (_, Left (e :: SomeException)) -> throw e

failing f _ (Failure x) = f x
failing _ s (Success x) = s x

prepare :: P.CacheRec -> P.Packages -> T.Download -> T.Download -> AptIOT IO T.Download
prepare cache package base patch = liftIO $
    (\ x -> qPutStrLn "Preparing quilt target" >> quieter 1 x) $
    makeQuiltTree cache (P.spec package) base patch >>= withUpstreamQuiltHidden make
    where
      withUpstreamQuiltHidden make (quiltTree, quiltDir) =
          hide >> make (quiltTree, quiltDir) >>= unhide
          where hide = doesDirectoryExist pc >>= (flip when) (rmrf pch >> renameDirectory pc pch)
                unhide x = doesDirectoryExist pch >>= (flip when) (rmrf pc >> renameDirectory pch pc) >> return x
                pc = (quiltDir ++ "/.pc")
                pch = (quiltDir ++ "/.pc.hide")
                rmrf d = runProcess id (ShellCommand ("rm -rf '"  ++ d ++ "'")) L.empty
      make :: (SourceTree, FilePath) -> IO T.Download
      make (quiltTree, quiltDir) =
          do applied <- runProcess id (ShellCommand cmd1a) L.empty >>= qMessage "Checking for applied patches" >>= return . unpackOutputs
             case applied of
               (ExitFailure 1 : _, _, err, _)
                   | err == "No patches applied\n" ->
                          findUnapplied >>= apply >> buildLog >> cleanSource
                          where
                            findUnapplied = do unapplied <- liftIO (runProcess id (ShellCommand cmd1b) L.empty) >>= qMessage "Checking for unapplied patches" . unpackOutputs
                                               case unapplied of
                                                 ([ExitSuccess], text, _, _) -> return (lines text)
                                                 _ -> fail $ target ++ " - No patches to apply"
                            apply patches =
                                do result2 <- liftIO (runProcess id (ShellCommand (cmd2 patches)) L.empty) >>= qMessage "Patching Quilt target" . unpackOutputs . mergeToStderr
                                   case result2 of
                                     ([ExitSuccess], _, _, _) -> return ()
                                     (_, _, err, _) -> fail $ target ++ " - Failed to apply quilt patches: " ++ err
                                         -- fail $ target ++ " - Failed to apply quilt patches: " ++ (cmd2 patches) ++ " ->\n" ++ L.unpack err
                            buildLog =
                                -- If there is a changelog file in the quilt directory,
                                -- interleave its entries with those in changelog of the base
                                -- tree by date.
                                do qPutStrLn "Merging changelogs"
                                   exists <- doesFileExist (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                                   case exists of
                                     False -> fail (target ++ "- Missing changelog file: " ++ show (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                                     True -> mergeChangelogs' (quiltDir ++ "/debian/changelog") (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                            cleanSource =
                                do result3 <- liftIO (runProcess id (ShellCommand cmd3) L.empty) >>= qMessage "Cleaning Quilt target" . unpackOutputs
                                   case result3 of
                                     ([ExitSuccess], _, _, _) ->
                                         do tree <- findSourceTree (topdir quiltTree)
                                            -- return $ Quilt base patch tree m
                                            return $ T.Download {
                                                         T.package = package
                                                       , T.getTop = topdir tree
                                                       , T.logText = "Quilt revision " ++ show (P.spec package)
                                                       , T.mVersion = Nothing
                                                       , T.origTarball = Nothing
                                                       , T.cleanTarget = \ top -> T.cleanTarget base top
                                                       , T.buildWrapper = id
                                                       }
                                     _ -> fail $ target ++ " - Failure removing quilt directory: " ++ cmd3
               (ExitFailure _ : _, _, err, _) -> fail $ target ++ " - Unexpected output from quilt applied: " ++ err
               (_, _, _, _) -> fail $ target ++ " - Unexpected result code (ExitSuccess) from " ++ show cmd1a
          where
            cmd1a = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt applied")
            cmd1b = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt unapplied")
            -- Apply all the unapplied patches, which should be all of
            -- the patches.  This somewhat roundabout two step process
            -- is required to make sure we get an error result if any
            -- of the patches fail.
            cmd2 patches =
                ("export QUILT_PATCHES=" ++ quiltPatchesDir ++
                 " && cd '" ++ quiltDir ++ "' && " ++
                 intercalate " && " (map ("quilt -v --leave-reject push " ++) patches))
            cmd3 = ("cd '" ++ quiltDir ++ "' && " ++
                    "rm -rf '" ++ quiltDir ++ "/.pc' '" ++ quiltDir ++ "/" ++ quiltPatchesDir ++ "'")
            target = "quilt:(" ++ show (T.method base) ++ "):(" ++ show (T.method patch) ++ ")"

mergeChangelogs' :: FilePath -> FilePath -> IO (Either String ())
mergeChangelogs' basePath patchPath =
    do patchText <- liftIO (try (readFile patchPath))
       baseText <- liftIO (try (readFile basePath))
       case (patchText, baseText) of
         (Right patchText, Right baseText) ->
             do -- vEPutStrBl 1 $ "Merging changelogs: " ++ baseText ++ "\npatch:\n\n" ++ patchText
                either (return . Left) replace (mergeChangelogs baseText patchText)
         (Left (e :: SomeException), _) -> return $ Left (show e)
         (_, Left (e :: SomeException)) -> return $ Left (show e)
    where
      replace newText = liftIO (try (replaceFile basePath $! newText)) >>=
                        return. either (\ (e :: SomeException) -> Left . show $ e) Right

partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing [] = ([], [])
partitionFailing (x : xs) =
    f x (partitionFailing xs)
    where
      f (Failure x) (failures, successes) = (x : failures, successes)
      f (Success x) (failures, successes) = (failures, x : successes)

-- Merge the entries in the patch changelog into the base changelog,
-- merging the base and patch version numbers as we go.  It is
-- important that we read the base changelog lazily since there are
-- lots of bizarre formats in the older entries that we can't parse.
mergeChangelogs :: String -> String -> Either String String
mergeChangelogs baseText patchText =
    case partitionEithers (parseLog patchText) of
      ([], patchEntries) ->
          let patchEntries' = map Patch patchEntries in
          let oldest = zonedTimeToUTC . myParseTimeRFC822 . logDate . getEntry . head . reverse $ patchEntries' in
          let (baseEntries, baseText') = partitionChangelog oldest baseText in
          let basePackage = maybe Nothing (Just . logPackage) (listToMaybe baseEntries) in
          let patchPackage = maybe Nothing (Just . logPackage) (listToMaybe patchEntries) in
          case basePackage == patchPackage of
            True ->
                let baseEntries' = map Base baseEntries in
                let mergedEntries = third . appendVersionNumbers . sortBy compareDate $ baseEntries' ++ patchEntries' in
                Right $ (intercalate "\n" (map (show . prettyEntry) mergedEntries)) ++ baseText'
            False ->
                Left $ "Package name mismatch between base and patch changelogs: " ++
                       maybe "?" id basePackage ++ " /= " ++ maybe "?" id patchPackage
      (failures, _) ->
          Left $ "Error(s) in patch changelog:\n  " ++ intercalate "\n  " (concat failures)
    where
      third (_, _, c) = c
      compareDate a b = compare (zonedTimeToUTC . myParseTimeRFC822 . getDate $ a) (zonedTimeToUTC . myParseTimeRFC822 . getDate $ b)
      getDate (Base entry) = logDate entry
      getDate (Patch entry) = logDate entry
      -- The version numbers of the patch entries need to be prefixed
      -- with the previous base version.  The base version numbers
      -- need to be suffixed with the previous patch version number.
      appendVersionNumbers entries = foldl modifyVersion (Nothing, Nothing, []) entries
      modifyVersion :: (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry]) -> EntryType
                    -> (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry])
      -- A base entry before the first patch entry, do nothing
      modifyVersion (_, Nothing, modified) (Base entry) = (Just (logVersion entry), Nothing, entry : modified)
      -- A patch entry before the first base entry, an error
      modifyVersion x@(Nothing, _, _modified) (Patch _entry) =
          -- This used to be an error:
          -- error "Patch changelog entry is older than oldest base entry"
          x
      -- Prefix a patch entry with the base version
      modifyVersion (Just baseVersion, _, modified) (Patch entry) =
          (Just baseVersion, (Just . logVersion $ entry), (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion baseVersion (logVersion entry)}
      -- Suffix a base entry with the patch version
      modifyVersion (_, Just patchVersion, modified) (Base entry) =
          ((Just . logVersion $ entry), Just patchVersion, (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion (logVersion entry) patchVersion}
      buildQuiltVersion baseVersion patchVersion =
          case Debian.Version.revision baseVersion of
            Just _ -> parseDebianVersion (show (prettyDebianVersion baseVersion) ++ "++" ++ show (prettyDebianVersion patchVersion))
            Nothing -> parseDebianVersion (show (prettyDebianVersion baseVersion) ++ "-" ++ show (prettyDebianVersion patchVersion))

partitionChangelog :: UTCTime -> String -> ([ChangeLogEntry], String)
partitionChangelog date text =
    case parseEntry text of
      Left _msgs -> ([], text)
      Right (entry, text') ->
          if date >= (zonedTimeToUTC . myParseTimeRFC822 . logDate $ entry)
          then ([entry], text')
          else case partitionChangelog date text' of
                 (entries, text'') -> (entry : entries, text'')

-- |This function is a bit less stringent than the official one -
-- e.g., it will accept "Wed, 10 Oct 2007 06:00:57 +0000", which the
-- official function won't.
myParseTimeRFC822 s =
    case matchRegex (mkRegex "^..., (.?.) (...) (....) (..):(..):(..) (.)(..)(..)$") s of
      Just [dom, mon, year, hour, min, sec, zoneSign, zoneHours, zoneMinutes] ->
          ZonedTime (localTime dom mon year hour min sec) (timeZone zoneSign zoneHours zoneMinutes)
      _ -> error ("Invalid date string: " ++ s)
    where
      -- spaceToZero ' ' = '0'
      -- spaceToZero x = x
      localTime dom mon year hr min sec = LocalTime (localDay dom mon year) (timeOfDay hr min sec)
      localDay dom mon year = fromGregorian (read year) (monthNumber mon) (read dom)
      timeZone "+" zoneHours zoneMinutes = TimeZone (60 * read zoneHours + read zoneMinutes) False ""
      timeZone "-" zoneHours zoneMinutes = TimeZone (- (60 * read zoneHours + read zoneMinutes)) False ""
      timeZone _ _ _ = error ("Time string has invalid time zone: " ++ s)
      timeOfDay hr min sec = TimeOfDay (read hr) (read min) (fromInteger . read $ sec)
      monthNumber "Jan" = 1
      monthNumber "Feb" = 2
      monthNumber "Mar" = 3
      monthNumber "Apr" = 4
      monthNumber "May" = 5
      monthNumber "Jun" = 6
      monthNumber "Jul" = 7
      monthNumber "Aug" = 8
      monthNumber "Sep" = 9
      monthNumber "Oct" = 10
      monthNumber "Nov" = 11
      monthNumber "Dec" = 12
      monthNumber _ = error ("Invalid month in time string: " ++ s)
