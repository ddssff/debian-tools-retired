{-# LANGUAGE PackageImports, OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- |Basic types for the Apt library.
module Debian.Repo.Changes
    (-- * read, show
    --  parseLog			-- String -> [ChangeLogEntry]
    --, parseEntry		-- String -> Maybe (ChangeLogEntry, String)
    --, parseChanges		-- String -> Maybe ChangeLogEntry
    --, showHeader		-- ChangeLogEntry -> String
    -- * Changes File
      findChangesFiles
    --, Section
    --, sectionName
    --, subSectionName
    --, load
    , parseChangesFilename
    , parseChangesFile
    , save
    , key
    , matchKey
    , base
    , Debian.Repo.Changes.path
    , name
    --, poolDir			-- PackageIndex -> ChangesFile -> FilePath
    , uploadLocal
    --, ChangesFile(..)
    --, changesFileName
    --, ChangedFileSpec(..)
    --, ChangeLogEntry(..)
    ) where

import "mtl" Control.Monad.Trans ( MonadIO(..) )
import Data.List ( isSuffixOf )
import Data.Maybe ( catMaybes )
import Data.Monoid ((<>), mconcat)
import Data.Text (Text, pack, unpack)
import Debian.Arch (Arch, prettyArch, parseArch)
import Debian.Changes ( ChangesFile(..), ChangedFileSpec(..), changesFileName, parseChanges )
import qualified Debian.Control.Text as S ( Paragraph'(..), Control'(Control), ControlFunctions(parseControlFromFile), fieldValue, modifyField )
import Debian.Release (parseReleaseName, parseSection)
import Debian.Repo.Types (outsidePath)
import Debian.Repo.Types.Repository (LocalRepository(repoRoot))
import Debian.Version ( parseDebianVersion, DebianVersion, prettyDebianVersion )
import Extra.Files ( replaceFile )
import System.FilePath ( splitFileName, (</>) )
import System.Directory ( doesFileExist, getDirectoryContents )
import qualified System.Posix.Files as F ( createLink, removeLink )
import Text.PrettyPrint.ANSI.Leijen (pretty, text)
import Text.Regex ( matchRegex, matchRegexAll, mkRegex )
import qualified Debian.Control.Text as B ()
import Debian.URI ()

{-
-- |A file generated by dpkg-buildpackage describing the result of a
-- package build
data ChangesFile =
    Changes { changeDir :: FilePath		-- ^ The full pathname of the directory holding the .changes file.
            , changePackage :: String		-- ^ The package name parsed from the .changes file name
            , changeVersion :: DebianVersion	-- ^ The version number parsed from the .changes file name
            , changeRelease :: ReleaseName	-- ^ The Distribution field of the .changes file
            , changeArch :: Arch		-- ^ The architecture parsed from the .changes file name
            , changeInfo :: S.Paragraph		-- ^ The contents of the .changes file
            , changeEntry :: ChangeLogEntry	-- ^ The value of the Changes field of the .changes file
            , changeFiles :: [ChangedFileSpec]	-- ^ The parsed value of the Files attribute
            }

-- |An entry in the list of files generated by the build.
data ChangedFileSpec =
    ChangedFileSpec { changedFileMD5sum :: String
                    , changedFileSize :: FileOffset
                    , changedFileSection :: SubSection
                    , changedFilePriority :: String
                    , changedFileName :: FilePath
                    }

-- |A changelog is a series of ChangeLogEntries
data ChangeLogEntry = Entry { logPackage :: String
                            , logVersion :: DebianVersion
                            , logDists :: [ReleaseName]
                            , logUrgency :: String
                            , logComments :: String
                            , logWho :: String
                            , logDate :: String
                            }

instance Show ChangesFile where
    show = changesFileName

changesFileName :: ChangesFile -> String
changesFileName changes =
    changePackage changes ++ "_" ++ show (changeVersion changes) ++ "_" ++ archName (changeArch changes) ++ ".changes"

instance Show ChangedFileSpec where
    show file = changedFileMD5sum file ++ " " ++
                show (changedFileSize file) ++ " " ++
                sectionName (changedFileSection file) ++ " " ++
                changedFilePriority file ++ " " ++
                changedFileName file

instance Show ChangeLogEntry where
    show (Entry package version dists urgency details who date) =
        package ++ " (" ++ show version ++ ") " ++ intercalate " " (map releaseName' dists) ++ "; urgency=" ++ urgency ++ "\n\n" ++
             details ++ " -- " ++ who ++ "  " ++ date ++ "\n\n"

-- |Show just the top line of a changelog entry (for debugging output.)
showHeader :: ChangeLogEntry -> String
showHeader (Entry package version dists urgency _ _ _) =
    package ++ " (" ++ show version ++ ") " ++ intercalate " " (map releaseName' dists) ++ "; urgency=" ++ urgency ++ "..."

-- |Parse a Debian Changelog and return a lazy list of entries
parseLog :: String -> [Either String ChangeLogEntry]
parseLog text =
    case parseEntry text of
      Nothing -> []
      Just (Left message) -> [Left message]
      Just (Right (entry, text')) -> Right entry : parseLog text'

-- |Parse a single changelog entry, returning the entry and the remaining text.
parseEntry :: String -> Maybe (Either String (ChangeLogEntry, String))
parseEntry text | dropWhile (\ x -> elem x " \t\n") text == "" = Nothing
parseEntry text =
    case matchRegexAll entryRE text of
      Nothing -> Just (Left ("Parse error in changelog:\n" ++ text))
      Just ("", _, remaining, [_, name, version, dists, urgency, _, details, _, _, _, _, _, who, date, _]) ->
          let entry =
                  Entry name 
                        (parseDebianVersion version)
                        (map parseReleaseName . words $ dists)
                        urgency
			details
                        who
                        date in
          Just (Right (entry, remaining))
      Just ("", _, _remaining, submatches) -> Just (Left ("Internal error 15, submatches=" ++ show submatches))
      Just (before, _, _, _) -> Just (Left ("Parse error in changelog at:\n" ++ show before ++ "\nin:\n" ++ text))
    where
      entryRE = mkRegex $ bol ++ blankLines ++ headerRE ++ nonSigLines ++ blankLines ++ signature ++ blankLines
      nonSigLines = "(((  .*)|([ \t]*)\n)+)"
      -- In the debian repository, sometimes the extra space in front of the
      -- day-of-month is missing, sometimes an extra one is added.
      signature = "( -- ([^\n]*)  (..., ? ?.. ... .... ........ .....))[ \t]*\n"

-- |Parse the changelog information that shows up in the .changes
-- file, i.e. a changelog entry with no signature.
parseChanges :: String -> Maybe ChangeLogEntry
parseChanges text =
    case matchRegex changesRE text of
      Nothing -> Nothing
      Just [_, name, version, dists, urgency, _, details] ->
          Just $ Entry name 
                       (parseDebianVersion version)
                       (map parseReleaseName . words $ dists)
                       urgency
		       details
                       "" ""
      Just x -> error $ "Unexpected match: " ++ show x
    where
      changesRE = mkRegexWithOpts (bol ++ blankLines ++ optWhite ++ headerRE ++ "(.*)$") False False

headerRE =
    package ++ version ++ dists ++ urgency
    where
      package = "([^ \t(]*)" ++ optWhite
      version = "\\(([^)]*)\\)" ++ optWhite
      dists = "([^;]*);" ++ optWhite
      urgency = "urgency=([^\n]*)\n" ++ blankLines

blankLines = blankLine ++ "*"
blankLine = "(" ++ optWhite ++ "\n)"
optWhite = "[ \t]*"
bol = "^"
-}

findChangesFiles :: FilePath -> IO [ChangesFile]
findChangesFiles dir =
    getDirectoryContents dir >>=
    return . filter (isSuffixOf ".changes") >>=
    mapM (load dir) >>= return . catMaybes

load :: FilePath -> String -> IO (Maybe ChangesFile)
load dir file =
    do
      case parseChangesFilename file of
        Just (name, ver, arch) ->
            do
              result <- parseChangesFile dir file
              case result of
                Right (S.Control changes) ->
                    -- The .changes file should be a single paragraph,
                    -- but there have been instances where extra newlines
                    -- are inserted.  To be forgiving we will concat all
                    -- the paragraphs into one (rather than erroring out
                    -- or discarding all but the first paragraph.)
                    let changes' = mergeParagraphs changes in
                    case (S.fieldValue "Files" changes' :: Maybe Text,
                          S.fieldValue "Checksums-Sha1" changes' :: Maybe Text,
                          S.fieldValue "Checksums-Sha256" changes' :: Maybe Text,
                          maybe Nothing parseChanges (S.fieldValue "Changes" changes'),
                          S.fieldValue "Distribution" changes') of
                      (Just text, sha1text, sha256text, Just entry, Just release) ->
                          do return . Just $ Changes { changeDir = dir
                                                     , changePackage = name
                                                     , changeVersion = ver
                                                     , changeRelease = parseReleaseName (unpack release)
                                                     , changeArch = arch
                                                     , changeInfo = changes'
                                                     , changeEntry = entry
                                                     , changeFiles = changedFileSpecs text sha1text sha256text }
                      _ -> return Nothing	-- Missing 'Files', 'Checksums-Sha1', 'Checksums-Sha256', 'Changes', or 'Distribution' field in .changes
                Left _error -> return Nothing
        Nothing -> return Nothing		-- Couldn't parse changes filename

mergeParagraphs :: [S.Paragraph' Text] -> S.Paragraph' Text
mergeParagraphs paragraphs =
    S.Paragraph . concat . map fieldsOf $ paragraphs
    where fieldsOf (S.Paragraph fields) = fields

save :: ChangesFile -> IO ()
save changes =
    replaceFile path (show (pretty (updateFiles (changeFiles changes) (changeInfo changes))))
    where
      path = changeDir changes ++ "/" ++ changesFileName changes
      updateFiles files info =
          S.modifyField "Checksums-Sha1" (const (showSHA1List files)).
          S.modifyField "Checksums-Sha256" (const (showSHA256List files)) .
          S.modifyField "Files" (const (showFileList files)) $ info

key :: ChangesFile -> (String, DebianVersion, Arch)
key changes = (changePackage changes, changeVersion changes, changeArch changes)

matchKey :: ChangesFile -> (String, DebianVersion, Arch) -> Bool
matchKey changes key = key == (changePackage changes, changeVersion changes, changeArch changes)

base :: ChangesFile -> String
base changes =
    changePackage changes ++ "_" ++ show (prettyDebianVersion (changeVersion changes) <> text "_" <> prettyArch (changeArch changes))

name :: ChangesFile -> FilePath
name changes = base changes ++ ".changes"

path :: ChangesFile -> FilePath
path changes = changeDir changes ++ "/" ++ name changes

-- | I suspect it would be more correct to read the contents of the file and
-- use that to construct the info we are inferring here from the filename.
parseChangesFilename :: String -> Maybe (String, DebianVersion, Arch)
parseChangesFilename name =
    case matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.changes$") name of
      Just [_, name, version, arch] -> Just (name, parseDebianVersion version, parseArch arch)
      _ -> error ("Invalid .changes file name: " ++ name)

-- parseChangesFile :: FilePath -> String -> IO (Either ParseError S.Control)
parseChangesFile dir file = S.parseControlFromFile (dir ++ "/" ++ file)

changedFileSpecs :: Text -> Maybe Text -> Maybe Text -> [ChangedFileSpec]
changedFileSpecs text sha1text sha256text =
    map changedFileSpec fileInfo
    where
      changedFileSpec (md5sum, size, section, priority, filename) =
                ChangedFileSpec { changedFileMD5sum = md5sum
                                , changedFileSHA1sum = maybe "" fst (lookup filename sha1sums)
                                , changedFileSHA256sum = maybe "" fst (lookup filename sha256sums)
                                , changedFileSize = read size
                                , changedFileSection = parseSection section
                                , changedFilePriority = priority
                                , changedFileName = filename }
      fileInfo = parseFileList (unpack text)
      sha1sums = maybe [] (parseChecksums . unpack) sha1text
      sha256sums = maybe [] (parseChecksums . unpack) sha256text

parseFileList :: String -> [(String, String, String, String, String)]
parseFileList text =
    case matchRegexAll re text of
      Just (_, _, remaining, [md5sum, size, section, priority, filename]) ->
          (md5sum, size, section, priority, filename) : parseFileList remaining
      _ -> []
    where
      re = mkRegex ("^[ \t\n]*" ++ g ++w++ g ++w++ g ++w++ g ++w++ g ++ "[ \t\n]*")
      g = "(" ++ t ++ ")"
      t = "[^ \t\n]+"
      w = "[ \t]+"

parseChecksums :: String -> [(String, (String, String))]
parseChecksums text =
    case matchRegexAll re text of
      Just (_, _, remaining, [checksum, size, name]) -> (name, (checksum, size)) : parseChecksums remaining
      _ -> []
    where
      re = mkRegex ("^[ \t\n]*" ++ g ++w++ g ++w++ g ++ "[ \t\n]*")
      g = "(" ++ t ++ ")"
      t = "[^ \t\n]+"
      w = "[ \t]+"

showFileList :: [ChangedFileSpec] -> Text
showFileList files = pack $ concat (map (("\n " <>) . show . pretty) files)

showSHA1List :: [ChangedFileSpec] -> Text
showSHA1List files = mconcat (map (("\n " <>) . showSHA1) files)
    where showSHA1 x = pack $ changedFileSHA1sum x ++ " " ++ show (changedFileSize x) ++ " " ++ changedFileName x

showSHA256List :: [ChangedFileSpec] -> Text
showSHA256List files = mconcat (map (("\n " <>) . showSHA256) files)
    where showSHA256 x = pack $ changedFileSHA256sum x ++ " " ++ show (changedFileSize x) ++ " " ++ changedFileName x

-- | Move a build result into a local repository's 'incoming' directory.
uploadLocal :: LocalRepository -> ChangesFile -> IO ()
uploadLocal repo changesFile =
    do let paths = map (\ file -> changeDir changesFile </> changedFileName file) (changeFiles changesFile)
       mapM_ (liftIO . install (outsidePath root)) (Debian.Repo.Changes.path changesFile : paths)
    where
      root = repoRoot repo
      -- Hard link a file into the incoming directory
      install root path =
	  do removeIfExists (dest root path)
	     F.createLink path (dest root path)
             -- F.removeLink path
      dest root path = root ++ "/incoming/" ++ snd (splitFileName path)
      removeIfExists path =
	  do exists <- doesFileExist path
	     if exists then F.removeLink path  else return ()
