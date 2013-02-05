-- | Read an existing Debianization from a directory file.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputChangeLog
    ) where

import Debug.Trace (trace)

import Control.Exception (SomeException, catch)
import Control.Monad (foldM, filterM)
import Data.Char (isSpace)
import Data.Lens.Lazy (setL)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, insert)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
import Debian.Changes (ChangeLog(..), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import Debian.Debianize.Atoms (HasAtoms(rulesHead, compat, sourceFormat, watch, changelog),
                                   Atoms, install, installDir, modControl,
                                   defaultAtoms, intermediateFile, warning, logrotateStanza, putPostInst,
                                   putCopyright, installInit, postRm, preInst, preRm, link)
import Debian.Debianize.ControlFile (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                     VersionControlSpec(..), XField(..), newSourceDebDescription', newBinaryDebDescription)
import Debian.Debianize.Utility (getDirectoryContents')
import Debian.Orphans ()
import Debian.Policy (Section(..), parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer,
                      parseUploaders, readSourceFormat)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
import Prelude hiding (readFile, lines, words, break, null, log, sum)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.IO.Error (catchIOError)

inputDebianization :: FilePath -> IO Atoms
inputDebianization top =
    do (ctl, _) <- inputSourceDebDescription debian `catchIOError` (\ e -> error ("Failure parsing SourceDebDescription: " ++ show e))
       -- Different from snd of above?
       atoms <- inputAtomsFromDirectory debian defaultAtoms `catch` (\ (e :: SomeException) -> error ("Failure parsing atoms: " ++ show e))
       return $ modControl (const ctl) atoms
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
      src = (newSourceDebDescription' findSource findMaint findStandards) {binaryPackages = bins}
      findSource = findMap "Source" SrcPkgName fields'
      findMaint = findMap "Maintainer" (either error id . parseMaintainer) fields'
      findStandards = findMap "Standards-Version" parseStandardsVersion fields'

      (bins, _extra) = unzip $ map parseBinaryDebDescription binaryParagraphs
      readField :: Field -> (SourceDebDescription, [Field]) -> (SourceDebDescription, [Field])
      readField (Field ("Source", _)) x = x
      readField (Field ("Maintainer", _)) x = x
      readField (Field ("Standards-Version", _)) x = x
      readField (Field ("Homepage", value)) (desc, unrecognized) = (desc {homepage = Just (strip (pack value))}, unrecognized)
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (desc {uploaders = either (const []) id (parseUploaders value)}, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (desc {dmUploadAllowed = yes value}, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (desc {priority = Just (readPriority value)}, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (desc {section = Just (MainSection value)}, unrecognized)
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

inputAtomsFromDirectory :: FilePath -> Atoms -> IO Atoms -- .install files, .init files, etc.
inputAtomsFromDirectory debian xs =
    debianFiles xs >>= intermediateFiles (debian </> "cabalInstall")
    where
      debianFiles :: Atoms -> IO Atoms
      debianFiles xs' =
          getDirectoryContents' debian >>=
          return . (++ ["source/format"]) >>=
          filterM (doesFileExist . (debian </>)) >>=
          foldM (\ xs'' name -> inputAtoms debian name xs'') xs'
      intermediateFiles :: FilePath -> Atoms -> IO Atoms
      intermediateFiles tmp xs' =
          do sums <- getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . concat
             files <- mapM (readFile . (tmp </>)) paths
             foldM (\ xs'' (path, file) -> return $ intermediateFile ("debian/cabalInstall" </> path) file xs'') xs' (zip paths files)

inputAtoms :: FilePath -> FilePath -> Atoms -> IO Atoms
inputAtoms _ path xs | elem path ["control"] = return xs
inputAtoms debian name@"source/format" xs = readFile (debian </> name) >>= \ text -> return $ (either warning (setL sourceFormat . Just) (readSourceFormat text)) xs
inputAtoms debian name@"watch" xs = readFile (debian </> name) >>= \ text -> return $ setL watch (Just text) xs
inputAtoms debian name@"rules" xs = readFile (debian </> name) >>= \ text -> return $ setL rulesHead (Just text) xs
inputAtoms debian name@"compat" xs = readFile (debian </> name) >>= \ text -> return $ setL compat (Just (read (unpack text))) xs
inputAtoms debian name@"copyright" xs = readFile (debian </> name) >>= \ text -> return $ putCopyright (Right text) xs
inputAtoms debian name@"changelog" xs =
    readFile (debian </> name) >>= return . parseChangeLog . unpack >>= \ log -> return $ setL changelog (Just log) xs
inputAtoms debian name xs =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   readFile (debian </> name) >>= \ text -> return $ foldr (readInstall p) xs (lines text)
      (p, ".dirs") ->      readFile (debian </> name) >>= \ text -> return $ foldr (readDir p) xs (lines text)
      (p, ".init") ->      readFile (debian </> name) >>= \ text -> return $ installInit p text xs
      (p, ".logrotate") -> readFile (debian </> name) >>= \ text -> return $ logrotateStanza p text xs
      (p, ".links") ->     readFile (debian </> name) >>= \ text -> return $ foldr (readLink p) xs (lines text)
      (p, ".postinst") ->  readFile (debian </> name) >>= \ text -> return $ putPostInst p text xs
      (p, ".postrm") ->    readFile (debian </> name) >>= \ text -> return $ postRm p text xs
      (p, ".preinst") ->   readFile (debian </> name) >>= \ text -> return $ preInst p text xs
      (p, ".prerm") ->     readFile (debian </> name) >>= \ text -> return $ preRm p text xs
      (_, ".log") ->       return xs -- Generated by debhelper
      (_, ".debhelper") -> return xs -- Generated by debhelper
      (_, ".hs") ->        return xs -- Code that uses this library
      (_, ".setup") ->     return xs -- Compiled Setup.hs file
      (_, ".substvars") -> return xs -- Unsupported
      (_, "") ->           return xs -- File with no extension
      (_, x) | last x == '~' -> return xs -- backup file
      _ -> trace ("Ignored: " ++ debian </> name) (return xs)

readLink :: BinPkgName -> Text -> Atoms -> Atoms
readLink p line atoms =
    case words line of
      [a, b] -> link p (unpack a) (unpack b) atoms
      [] -> atoms
      _ -> trace ("readLink: " ++ show line) atoms

readInstall :: BinPkgName -> Text -> Atoms -> Atoms
readInstall p line atoms =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> install p (unpack (strip a)) (unpack (strip b)) atoms

readDir :: BinPkgName -> Text -> Atoms -> Atoms
readDir p line atoms = installDir p (unpack line) atoms
