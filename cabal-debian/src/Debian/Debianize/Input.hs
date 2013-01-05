-- | Read an existing Debianization from a directory file.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputChangeLog
    ) where

import Debug.Trace (trace)

import Control.Applicative (pure, (<$>), (<*>))
import Control.Exception (SomeException, catch)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (mempty, mconcat)
import Data.Set (fromList, insert)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
import Debian.Changes (ChangeLog(..), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import Debian.Debianize.Default (newSourceDebDescription, newBinaryDebDescription)
import Debian.Debianize.Types.Debianization (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), DebAtom(..),
                                             VersionControlSpec(..), XField(..))
import Debian.Orphans ()
import Debian.Policy (parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer, parseUploaders)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
import Prelude hiding (readFile, lines, words, break, null, log, sum)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.IO.Error (catchIOError)

inputDebianization :: FilePath -> IO Debianization
inputDebianization top =
    Debianization <$> (fst <$> inputSourceDebDescription debian `catchIOError` (\ e -> error ("Failure parsing SourceDebDescription: " ++ show e)))
                  <*> inputChangeLog debian `catchIOError` (\ e -> error ("Failure parsing changelog: " ++ show e))
                  <*> inputRulesFile debian
                  <*> inputCompat debian
                  <*> (Right <$> inputCopyright debian)
                  <*> pure mempty
                  <*> pure mempty
                  <*> (inputAtomsFromDirectory debian `catch` (\ (e :: SomeException) -> error ("Failure parsing atoms: " ++ show e)))
    where
      debian = top </> "debian"

inputSourceDebDescription :: FilePath -> IO (SourceDebDescription, [Field])
inputSourceDebDescription debian =
    do paras <- parseControlFromFile (debian </> "control") >>= return . either (error . show) unControl
       case paras of
         [] -> error "Missing source paragraph"
         [_] -> error "Missing binary paragraph"
         (hd : tl) -> return $ parseSourceDebDescription hd tl

parseSourceDebDescription :: Paragraph' String -> [Paragraph' String] -> (SourceDebDescription, [Field])
parseSourceDebDescription (Paragraph fields) binaryParagraphs =
    foldr readField (src, []) fields'
    where
      fields' = map stripField fields
      src = (newSourceDebDescription findSource findMaint findStandards) {binaryPackages = bins}
      findSource = findMap "Source" SrcPkgName fields'
      findMaint = findMap "Maintainer" (either error id . parseMaintainer) fields'
      findStandards = findMap "Standards-Version" parseStandardsVersion fields'

      (bins, _extra) = unzip $ map parseBinaryDebDescription binaryParagraphs
      readField :: Field -> (SourceDebDescription, [Field]) -> (SourceDebDescription, [Field])
      readField (Field ("Source", _)) x = x
      readField (Field ("Maintainer", _)) x = x
      readField (Field ("Standards-Version", _)) x = x
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (desc {uploaders = either (const []) id (parseUploaders value)}, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (desc {dmUploadAllowed = yes value}, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (desc {priority = Just (readPriority value)}, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (desc {section = Just (pack value)}, unrecognized)
      readField (Field ("Build-Depends", value)) (desc, unrecognized) = (desc {buildDepends = rels value}, unrecognized)
      readField (Field ("Build-Conflicts", value)) (desc, unrecognized) = (desc {buildConflicts = rels value}, unrecognized)
      readField (Field ("Build-Depends-Indep", value)) (desc, unrecognized) = (desc {buildDependsIndep = rels value}, unrecognized)
      readField (Field ("Build-Conflicts-Indep", value)) (desc, unrecognized) = (desc {buildConflictsIndep = rels value}, unrecognized)
      readField (Field ("Vcs-Browser", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSBrowser (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Arch", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSArch (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Bzr", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSBzr (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Cvs", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSCvs (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Darcs", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSDarcs (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Git", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSGit (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Hg", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSHg (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Mtn", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSMtn (pack s)) (vcsFields desc)}, unrecognized)
      readField (Field ("Vcs-Svn", s)) (desc, unrecognized) = (desc {vcsFields = insert (VCSSvn (pack s)) (vcsFields desc)}, unrecognized)
      readField field@(Field ('X' : fld, value)) (desc, unrecognized) =
          case span (`elem` "BCS") fld of
            (xs, '-' : more) -> (desc {xFields = insert (XField (fromList (map (read' . (: [])) xs)) (pack more) (pack value)) (xFields desc)}, unrecognized)
            _ -> (desc, field : unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

parseBinaryDebDescription :: Paragraph' String -> (BinaryDebDescription, [Field])
parseBinaryDebDescription (Paragraph fields) =
    foldr readField (bin, []) fields'
    where
      fields' = map stripField fields
      bin = newBinaryDebDescription findPackage findArchitecture
      findPackage = findMap "Package" BinPkgName fields'
      findArchitecture = findMap "Architecture" parsePackageArchitectures fields'
{-
(BinPkgName (fromJust (fieldValue "Package" bin)))
(read' (fromJust (fieldValue "Architecture" bin)))
, []
    foldr readField (newBinaryDebDescription (BinPkgName (fromJust (fieldValue "Package" bin))) (read' (fromJust (fieldValue "Architecture" bin))), []) (map stripField fields)
-}

      readField :: Field -> (BinaryDebDescription, [Field]) -> (BinaryDebDescription, [Field])
      readField (Field ("Package", value)) (desc, unrecognized) = (desc {package = BinPkgName value}, unrecognized)
      readField (Field ("Architecture", value)) (desc, unrecognized) = (desc {architecture = parsePackageArchitectures value}, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (desc {binarySection = Just (readSection value)}, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (desc {binaryPriority = Just (readPriority value)}, unrecognized)
      readField (Field ("Essential", value)) (desc, unrecognized) = (desc {essential = yes value}, unrecognized)
      readField (Field ("Depends", value)) (desc, unrecognized) = (desc {relations = (relations desc) {depends = rels value}}, unrecognized)
      readField (Field ("Recommends", value)) (desc, unrecognized) = (desc {relations = (relations desc) {recommends = rels value}}, unrecognized)
      readField (Field ("Suggests", value)) (desc, unrecognized) = (desc {relations = (relations desc) {suggests = rels value}}, unrecognized)
      readField (Field ("Pre-Depends", value)) (desc, unrecognized) = (desc {relations = (relations desc) {preDepends = rels value}}, unrecognized)
      readField (Field ("Breaks", value)) (desc, unrecognized) = (desc {relations = (relations desc) {breaks = rels value}}, unrecognized)
      readField (Field ("Conflicts", value)) (desc, unrecognized) = (desc {relations = (relations desc) {conflicts = rels value}}, unrecognized)
      readField (Field ("Provides", value)) (desc, unrecognized) = (desc {relations = (relations desc) {provides = rels value}}, unrecognized)
      readField (Field ("Replaces", value)) (desc, unrecognized) = (desc {relations = (relations desc) {replaces = rels value}}, unrecognized)
      readField (Field ("Built-Using", value)) (desc, unrecognized) = (desc {relations = (relations desc) {builtUsing = rels value}}, unrecognized)
      readField (Field ("Description", value)) (desc, unrecognized) = (desc {description = pack value}, unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

-- | Look for a field and apply a function to its value
findMap :: String -> (String -> a) -> [Field] -> a
findMap field f fields =
    fromMaybe (error $ "Missing field: " ++ field) (foldr findMap' Nothing fields)
    where
      findMap' (Field (fld, val)) x = if fld == field then Just (f val) else x
      findMap' _ x = x

read' :: Read a => String -> a
read' s = trace ("read " ++ show s) (read s)

stripField :: ControlFunctions a => Field' a -> Field' a
stripField (Field (a, b)) = Field (a, stripWS b)
stripField x = x

rels :: String -> Relations
rels s =
    either (\ e -> error ("Relations field error: " ++ show e ++ "\n  " ++ s)) id (parseRelations s)

yes :: String -> Bool
yes "yes" = True
yes "no" = False
yes x = error $ "Expecting yes or no: " ++ x

inputChangeLog :: FilePath -> IO ChangeLog
inputChangeLog debian = readFile (debian </> "changelog") >>= return . parseChangeLog . unpack  -- `catch` handleDoesNotExist :: IO ChangeLog

inputRulesFile :: FilePath -> IO Text
inputRulesFile debian = readFile (debian </> "rules") -- Treat the whole thing as the head.

inputCompat :: FilePath -> IO Int
inputCompat debian = read . unpack <$> readFile (debian </> "compat")

inputCopyright :: FilePath -> IO Text
inputCopyright debian = readFile (debian </> "copyright")

inputAtomsFromDirectory :: FilePath -> IO [DebAtom] -- .install files, .init files, etc.
inputAtomsFromDirectory debian =
    (++) <$> debianFiles <*> intermediateFiles (debian </> "cabalInstall")
    where
      debianFiles =
          getDirectoryContents' debian >>=
          return . (++ ["source/format"]) >>=
          mapM doName >>=
          return . mconcat
      intermediateFiles tmp =
          do sums <- getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . concat
             files <- mapM (readFile . (tmp </>)) paths
             return $ map (\ (path, file) -> DHIntermediate ("debian/cabalInstall" </> path) file) (zip paths files)
      getDirectoryContents' dir = getDirectoryContents dir >>= return . filter (not . dotFile)
      doName name = doesFileExist (debian </> name) >>= doFile name
      doFile _ False = return mempty
      doFile name True = inputAtoms debian name
      dotFile "." = True
      dotFile ".." = True
      dotFile _ = False

inputAtoms :: FilePath -> FilePath -> IO [DebAtom]
inputAtoms _ path | elem path ["changelog", "control", "compat", "copyright", "rules"] = return mempty
inputAtoms debian name@"source/format" = (: []) . DebSourceFormat <$> readFile (debian </> name)
inputAtoms debian name@"watch" = (: []) . DebWatch <$> readFile (debian </> name)
inputAtoms debian name =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   mapMaybe (readInstall p) . lines   <$> readFile (debian </> name)
      (p, ".dirs") ->      map (readDir p) . lines       <$> readFile (debian </> name)
      (p, ".init") ->      (: []) . DHInstallInit p              <$> readFile (debian </> name)
      (p, ".logrotate") -> (: []) . DHInstallLogrotate p         <$> readFile (debian </> name)
      (p, ".links") ->     mapMaybe (readLink p) . lines <$> readFile (debian </> name)
      (p, ".postinst") ->  (: []) . DHPostInst p                 <$> readFile (debian </> name)
      (p, ".postrm") ->    (: []) . DHPostRm p                   <$> readFile (debian </> name)
      (p, ".preinst") ->   (: []) . DHPreInst p                  <$> readFile (debian </> name)
      (p, ".prerm") ->     (: []) . DHPreRm p                    <$> readFile (debian </> name)
      (_, ".log") ->       return mempty -- Generated by debhelper
      (_, ".debhelper") -> return mempty -- Generated by debhelper
      (_, ".hs") ->        return mempty -- Code that uses this library
      (_, ".setup") ->     return mempty -- Compiled Setup.hs file
      (_, ".substvars") -> return mempty -- Unsupported
      (_, "") ->           return mempty -- File with no extension
      (_, x) | last x == '~' -> return mempty -- backup file
      _ -> trace ("Ignored: " ++ debian </> name) (return mempty)

readLink :: BinPkgName -> Text -> Maybe DebAtom
readLink p line =
    case words line of
      [a, b] -> Just $ DHLink p (unpack a) (unpack b)
      [] -> Nothing
      _ -> trace ("readLink: " ++ show line) Nothing

readInstall :: BinPkgName -> Text -> Maybe DebAtom
readInstall name line =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show name ++ ": " ++ show line
      (a, b) -> Just $ DHInstall name (unpack (strip a)) (unpack (strip b))

readDir :: BinPkgName -> Text -> DebAtom
readDir p line = DHInstallDir p (unpack line)
