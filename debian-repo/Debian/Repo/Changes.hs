-- | A .changes file is a file generated by dpkg-buildpackage
-- describing the result of a package build.  Not to be confused
-- with a debian/changelog file.  After it is uploaded it goes into
-- the @installed/@ subdirectory of the repository.
{-# LANGUAGE PackageImports, OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
module Debian.Repo.Changes
    ( findChangesFiles
    , saveChangesFile
{-
    , parseChangesFilename
    , parseChangesFile
-}
    , changeKey
    , changePath
    , changeName
    ) where

import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.Text (pack, Text, unpack)
import Debian.Arch (Arch, parseArch, prettyArch)
import Debian.Changes (ChangedFileSpec(..), ChangesFile(..), changesFileName, parseChanges)
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile), fieldValue, modifyField, Paragraph'(..))
import Debian.Release (parseReleaseName, parseSection)
import Debian.Repo.Prelude (replaceFile)
import Debian.URI ()
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import System.Directory (getDirectoryContents)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex (matchRegex, matchRegexAll, mkRegex)

findChangesFiles :: FilePath -> IO [ChangesFile]
findChangesFiles dir =
    getDirectoryContents dir >>=
    return . filter (isSuffixOf ".changes") >>=
    mapM (loadChangesFile dir) >>= return . catMaybes

loadChangesFile :: FilePath -> String -> IO (Maybe ChangesFile)
loadChangesFile dir file =
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

saveChangesFile :: ChangesFile -> IO ()
saveChangesFile changes =
    replaceFile path (show (pretty (updateFiles (changeFiles changes) (changeInfo changes))))
    where
      path = changeDir changes ++ "/" ++ changesFileName changes
      updateFiles files info =
          S.modifyField "Checksums-Sha1" (const (showSHA1List files)).
          S.modifyField "Checksums-Sha256" (const (showSHA256List files)) .
          S.modifyField "Files" (const (showFileList files)) $ info

-- | They primary key for .changes files is the triple containing the
-- package name, version, and architecture.  There is a 1-1 correspondence
-- between a key value and the name of the .changes file.
changeKey :: ChangesFile -> (String, DebianVersion, Arch)
changeKey changes = (changePackage changes, changeVersion changes, changeArch changes)

changeName :: ChangesFile -> FilePath
changeName changes =
    changePackage changes <> "_" <>
    show (prettyDebianVersion (changeVersion changes)) <> "_" <>
    show (prettyArch (changeArch changes)) <> ".changes"

changePath :: ChangesFile -> FilePath
changePath changes = changeDir changes ++ "/" ++ changeName changes

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
