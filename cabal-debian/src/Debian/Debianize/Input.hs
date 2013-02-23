-- | Read an existing Debianization from a directory file.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , inputCabalization
    , inputLicenseFile
    , inputMaintainer
    ) where

import Debug.Trace (trace)

import Control.Exception (bracket)
import Control.Monad (when, foldM, filterM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.Lens.Lazy (getL, setL, modL)
import Data.Map as Map (insertWith)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (mempty)
import Data.Set as Set (toList, fromList, insert, union, singleton)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
import Debian.Changes (ChangeLog(..), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import Debian.Debianize.Atoms as Atoms
    (Atoms, rulesHead, compat, sourceFormat, watch, changelog, control, copyright,
     intermediateFiles, postInst, postRm, preInst, preRm, install, installDir, warning,
     logrotateStanza, installInit, link, packageDescription, compiler, maintainer, verbosity,
     compilerVersion, cabalFlagAssignments)
import Debian.Debianize.ControlFile (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                     VersionControlSpec(..), XField(..), newSourceDebDescription', newBinaryDebDescription)
import Debian.Debianize.Types (Top(Top, unTop))
import Debian.Debianize.Utility (getDirectoryContents', withCurrentDirectory, readFileMaybe)
import Debian.Orphans ()
import Debian.Policy (Section(..), parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer,
                      parseUploaders, readSourceFormat, getDebianMaintainer, haskellMaintainer)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
import Distribution.Package (Package(packageId))
import Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, maintainer))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, intToVerbosity)
import Prelude hiding (readFile, lines, words, break, null, log, sum)
import System.Cmd (system)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Posix.Files (setFileCreationMask)
import System.IO.Error (catchIOError)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputDebianization :: Top -> IO Atoms
inputDebianization top =
    do (ctl, _) <- inputSourceDebDescription top
       atoms <- inputAtomsFromDirectory top mempty
       return $ modL control (const ctl) atoms

-- | Try to input a file and if successful add it to the debianization.
inputDebianizationFile :: Top -> FilePath -> Atoms -> IO Atoms
inputDebianizationFile (Top top) path atoms =
    readFileMaybe (top </> path) >>= return . maybe atoms (\ text -> modL intermediateFiles (insert (path, text)) atoms)

inputSourceDebDescription :: Top -> IO (SourceDebDescription, [Field])
inputSourceDebDescription top =
    do paras <- parseControlFromFile (unTop top </> "debian/control") >>= either (error . show) (return . unControl)
       case paras of
         [] -> error "Missing source paragraph"
         [_] -> error "Missing binary paragraph"
         (hd : tl) -> return $ parseSourceDebDescription hd tl

parseSourceDebDescription :: Paragraph' String -> [Paragraph' String] -> (SourceDebDescription, [Field])
parseSourceDebDescription (Paragraph fields) binaryParagraphs =
    foldr readField (src, []) fields'
    where
      fields' = map stripField fields
      src = (newSourceDebDescription' findSource findMaint) {binaryPackages = bins}
      findSource = findMap "Source" SrcPkgName fields'
      findMaint = findMap "Maintainer" (either error id . parseMaintainer) fields'
      -- findStandards = findMap "Standards-Version" parseStandardsVersion fields'

      (bins, _extra) = unzip $ map parseBinaryDebDescription binaryParagraphs
      readField :: Field -> (SourceDebDescription, [Field]) -> (SourceDebDescription, [Field])
      -- Mandatory
      readField (Field ("Source", _)) x = x
      readField (Field ("Maintainer", _)) x = x
      -- readField (Field ("Standards-Version", _)) x = x
      -- Recommended
      readField (Field ("Standards-Version", value)) (desc, unrecognized) = (desc {standardsVersion = Just (parseStandardsVersion value)}, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (desc {priority = Just (readPriority value)}, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (desc {section = Just (MainSection value)}, unrecognized)
      -- Optional
      readField (Field ("Homepage", value)) (desc, unrecognized) = (desc {homepage = Just (strip (pack value))}, unrecognized)
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (desc {uploaders = either (const []) id (parseUploaders value)}, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (desc {dmUploadAllowed = yes value}, unrecognized)
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
    fromMaybe (error $ "Missing " ++ show field ++ " field in " ++ show fields) (foldr findMap' Nothing fields)
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

inputChangeLog :: Top -> IO ChangeLog
inputChangeLog (Top top) = readFile (top </> "debian/changelog") >>= return . parseChangeLog . unpack

inputAtomsFromDirectory :: Top -> Atoms -> IO Atoms -- .install files, .init files, etc.
inputAtomsFromDirectory top xs =
    findFiles xs >>= doFiles (unTop top </> "debian/cabalInstall")
    where
      findFiles :: Atoms -> IO Atoms
      findFiles xs' =
          getDirectoryContents' (unTop top </> "debian") >>=
          return . (++ ["source/format"]) >>=
          filterM (doesFileExist . ((unTop top </> "debian") </>)) >>=
          foldM (\ xs'' name -> inputAtoms (unTop top </> "debian") name xs'') xs'
      doFiles :: FilePath -> Atoms -> IO Atoms
      doFiles tmp xs' =
          do sums <- getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . concat
             files <- mapM (readFile . (tmp </>)) paths
             foldM (\ xs'' (path, file) -> return $ modL intermediateFiles (Set.insert ("debian/cabalInstall" </> path, file)) xs'') xs' (zip paths files)

inputAtoms :: FilePath -> FilePath -> Atoms -> IO Atoms
inputAtoms _ path xs | elem path ["control"] = return xs
inputAtoms debian name@"source/format" xs = readFile (debian </> name) >>= \ text -> return $ (either (modL warning . Set.insert) (setL sourceFormat . Just) (readSourceFormat text)) xs
inputAtoms debian name@"watch" xs = readFile (debian </> name) >>= \ text -> return $ setL watch (Just text) xs
inputAtoms debian name@"rules" xs = readFile (debian </> name) >>= \ text -> return $ setL rulesHead (Just text) xs
inputAtoms debian name@"compat" xs = readFile (debian </> name) >>= \ text -> return $ setL compat (Just (read (unpack text))) xs
inputAtoms debian name@"copyright" xs = readFile (debian </> name) >>= \ text -> return $ setL copyright (Just (Right text)) xs
inputAtoms debian name@"changelog" xs =
    readFile (debian </> name) >>= return . parseChangeLog . unpack >>= \ log -> return $ setL changelog (Just log) xs
inputAtoms debian name xs =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   readFile (debian </> name) >>= \ text -> return $ foldr (readInstall p) xs (lines text)
      (p, ".dirs") ->      readFile (debian </> name) >>= \ text -> return $ foldr (readDir p) xs (lines text)
      (p, ".init") ->      readFile (debian </> name) >>= \ text -> return $ modL installInit (insertWith (error "inputAtoms") p text) xs
      (p, ".logrotate") -> readFile (debian </> name) >>= \ text -> return $ modL logrotateStanza (insertWith Set.union p (singleton text)) xs
      (p, ".links") ->     readFile (debian </> name) >>= \ text -> return $ foldr (readLink p) xs (lines text)
      (p, ".postinst") ->  readFile (debian </> name) >>= \ text -> return $ modL postInst (insertWith (error "inputAtoms") p text) xs
      (p, ".postrm") ->    readFile (debian </> name) >>= \ text -> return $ modL postRm (insertWith (error "inputAtoms") p text) xs
      (p, ".preinst") ->   readFile (debian </> name) >>= \ text -> return $ modL preInst (insertWith (error "inputAtoms") p text) xs
      (p, ".prerm") ->     readFile (debian </> name) >>= \ text -> return $ modL preRm (insertWith (error "inputAtoms") p text) xs
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
      [a, b] -> modL link (insertWith Set.union p (singleton (unpack a, unpack b))) atoms
      [] -> atoms
      _ -> trace ("readLink: " ++ show line) atoms

readInstall :: BinPkgName -> Text -> Atoms -> Atoms
readInstall p line atoms =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> modL install (insertWith union p (singleton (unpack (strip a), unpack (strip b)))) atoms

readDir :: BinPkgName -> Text -> Atoms -> Atoms
readDir p line atoms = modL installDir (insertWith union p (singleton (unpack line))) atoms

inputCabalization :: Top -> Atoms -> IO Atoms
inputCabalization top atoms =
    withCurrentDirectory (unTop top) $ do
      descPath <- defaultPackageDesc vb
      genPkgDesc <- readPackageDescription vb descPath
      (compiler', _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration vb
      let compiler'' = case getL compilerVersion atoms of
                         (Just ver) -> compiler' {compilerId = CompilerId GHC ver}
                         _ -> compiler'
      case finalizePackageDescription (toList (getL cabalFlagAssignments atoms)) (const True) (Platform buildArch buildOS) (compilerId compiler'') [] genPkgDesc of
        Left e -> error $ "Failed to load cabal package description: " ++ show e
        Right (pkgDesc, _) -> do
          liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
          return $ setL compiler (Just compiler'') $
                   setL packageDescription (Just pkgDesc) $ atoms
    where
      vb = intToVerbosity' (getL verbosity atoms)

-- | Run the package's configuration script.
autoreconf :: Verbosity -> PackageDescription -> IO ()
autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

-- | Try to read the license file specified in the cabal package,
-- otherwise return a text representation of the License field.
inputLicenseFile :: PackageDescription -> IO (Maybe Text)
inputLicenseFile pkgDesc = readFileMaybe (licenseFile pkgDesc)

-- | Try to compute the debian maintainer from the maintainer field of the
-- cabal package, or from the value returned by getDebianMaintainer.
inputMaintainer :: Atoms -> IO (Maybe NameAddr)
inputMaintainer atoms =
    debianPackageMaintainer >>= maybe cabalPackageMaintainer (return . Just) >>=
                                maybe getDebianMaintainer (return . Just) >>=
                                maybe lastResortMaintainer (return . Just)
    where
      debianPackageMaintainer :: IO (Maybe NameAddr)
      debianPackageMaintainer = return (getL Atoms.maintainer atoms)
      cabalPackageMaintainer :: IO (Maybe NameAddr)
      cabalPackageMaintainer = return $ case fmap Cabal.maintainer (getL packageDescription atoms) of
                                          Nothing -> Nothing
                                          Just "" -> Nothing
                                          Just x -> either (const Nothing) Just (parseMaintainer (takeWhile (\ c -> c /= ',' && c /= '\n') x))
      lastResortMaintainer :: IO (Maybe NameAddr)
      lastResortMaintainer = return (Just haskellMaintainer)

intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))
