-- | Read an existing Debianization from a directory file.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , inputCompiler
    , inputCabalization
    , inputLicenseFile
    , inputMaintainer
    , dataDir
    ) where

import Debug.Trace (trace)

import Control.Exception (bracket)
import Control.Monad (when, foldM, filterM)
import Control.Monad.State (get, put)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.Char (isSpace, toLower)
import Data.Lens.Lazy (getL, access)
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, toList, fromList, insert)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(logWho), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import qualified Debian.Debianize.Facts.Lenses as Lenses (maintainer, changelog)
import Debian.Debianize.Facts.Types as Debian
    (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
     VersionControlSpec(..), XField(..), newSourceDebDescription', newBinaryDebDescription)
import Debian.Debianize.Facts.Lenses
    (control, warning, sourceFormat, watch, rulesHead, compat, packageDescription, compiler,
     copyright, changelog, installInit, postInst, postRm, preInst, preRm,
     logrotateStanza, link, install, installDir, intermediateFiles, compilerVersion, cabalFlagAssignments, verbosity)
import Debian.Debianize.Facts.Monad (Atoms, DebT, execDebT)
import Debian.Debianize.Facts.Types (Top(unTop), newAtoms)
import Debian.Debianize.Utility (getDirectoryContents', withCurrentDirectory, readFileMaybe, read', intToVerbosity', (~=), (~?=), (+=), (++=), (+++=))
import Debian.Orphans ()
import Debian.Policy (Section(..), parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer,
                      parseUploaders, readSourceFormat, getDebianMaintainer)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
import Distribution.Package (Package(packageId), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, maintainer, package))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (readFile, lines, words, break, null, log, sum)
import System.Cmd (system)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Posix.Files (setFileCreationMask)
import System.IO.Error (catchIOError, tryIOError)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputDebianization :: Top -> DebT IO ()
inputDebianization top =
    do -- Erase any the existing information
       put newAtoms
       (ctl, _) <- inputSourceDebDescription top
       inputAtomsFromDirectory top
       control ~= ctl

-- | Try to input a file and if successful add it to the debianization.
inputDebianizationFile :: Top -> FilePath -> DebT IO ()
inputDebianizationFile top path =
    do inputAtomsFromDirectory top
       lift (readFileMaybe (unTop top </> path)) >>= maybe (return ()) (\ text -> intermediateFiles += (path, text))

inputSourceDebDescription :: Top -> DebT IO (SourceDebDescription, [Field])
inputSourceDebDescription top =
    do paras <- lift $ parseControlFromFile (unTop top </> "debian/control") >>= either (error . show) (return . unControl)
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
      findMaint = findMap "Maintainer" (\ m -> either (\ e -> error $ "Failed to parse maintainer field " ++ show m ++ ": " ++ show e) id . parseMaintainer $ m) fields'
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
            (xs, '-' : more) -> (desc {xFields = insert (XField (fromList (map (read' (\ s -> error $ "parseSourceDebDescription: " ++ show s) . (: [])) xs)) (pack more) (pack value)) (xFields desc)}, unrecognized)
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
      readField (Field ("Package", value)) (desc, unrecognized) = (desc {Debian.package = BinPkgName value}, unrecognized)
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
      readField (Field ("Provides", value)) (desc, unrecognized) = (desc {relations = (relations desc) {provides_ = rels value}}, unrecognized)
      readField (Field ("Replaces", value)) (desc, unrecognized) = (desc {relations = (relations desc) {replaces_ = rels value}}, unrecognized)
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

inputChangeLog :: MonadIO m => Top -> DebT m ()
inputChangeLog top =
    do log <- liftIO $ tryIOError (readFile (unTop top </> "debian/changelog") >>= return . parseChangeLog . unpack)
       changelog ~?= either (\ _ -> Nothing) Just log

inputAtomsFromDirectory :: Top -> DebT IO () -- .install files, .init files, etc.
inputAtomsFromDirectory top =
    do atoms <- get
       atoms' <- lift $ findFiles atoms
       atoms'' <- lift $ doFiles (unTop top </> "debian/cabalInstall") atoms'
       put atoms''
    where
      -- Find regular files matching debian/* and debian/source/format and
      -- add them to the debianization.
      findFiles :: Atoms -> IO Atoms
      findFiles atoms =
          getDirectoryContents' (unTop top </> "debian") >>=
          return . (++ ["source/format"]) >>=
          filterM (doesFileExist . ((unTop top </> "debian") </>)) >>=
          foldM (\ atoms' name -> inputAtoms (unTop top </> "debian") name atoms') atoms
      doFiles :: FilePath -> Atoms -> IO Atoms
      doFiles tmp atoms =
          do sums <- getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . filter ((/= '~') . last) . concat
             files <- mapM (readFile . (tmp </>)) paths
             execDebT (mapM_ (intermediateFiles +=) (zip (map ("debian/cabalInstall" </>) paths) files)) atoms

-- | Construct a file path from the debian directory and a relative
-- path, read its contents and add the result to the debianization.
-- This may mean using a specialized parser from the debian package
-- (e.g. parseChangeLog), and some files (like control) are ignored
-- here, though I don't recall why at the moment.
inputAtoms :: FilePath -> FilePath -> Atoms -> IO Atoms
inputAtoms _ path xs | elem path ["control"] = return xs
inputAtoms debian name@"source/format" xs = readFile (debian </> name) >>= \ text -> execDebT (either (warning +=) ((sourceFormat ~=) . Just) (readSourceFormat text)) xs
inputAtoms debian name@"watch" xs = readFile (debian </> name) >>= \ text -> execDebT (watch ~= Just text) xs
inputAtoms debian name@"rules" xs = readFile (debian </> name) >>= \ text -> execDebT (rulesHead ~= (Just text)) xs
inputAtoms debian name@"compat" xs = readFile (debian </> name) >>= \ text -> execDebT (compat ~= Just (read' (\ s -> error $ "compat: " ++ show s) (unpack text))) xs
inputAtoms debian name@"copyright" xs = readFile (debian </> name) >>= \ text -> execDebT (copyright ~= Just (Right text)) xs
inputAtoms debian name@"changelog" xs =
    readFile (debian </> name) >>= return . parseChangeLog . unpack >>= \ log -> execDebT (changelog ~= Just log) xs
inputAtoms debian name xs =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   readFile (debian </> name) >>= \ text -> execDebT (mapM_ (readInstall p) (lines text)) xs
      (p, ".dirs") ->      readFile (debian </> name) >>= \ text -> execDebT (mapM_ (readDir p) (lines text)) xs
      (p, ".init") ->      readFile (debian </> name) >>= \ text -> execDebT (installInit ++= (p, text)) xs
      (p, ".logrotate") -> readFile (debian </> name) >>= \ text -> execDebT (logrotateStanza +++= (p, text)) xs
      (p, ".links") ->     readFile (debian </> name) >>= \ text -> execDebT (mapM_ (readLink p) (lines text)) xs
      (p, ".postinst") ->  readFile (debian </> name) >>= \ text -> execDebT (postInst ++= (p, text)) xs
      (p, ".postrm") ->    readFile (debian </> name) >>= \ text -> execDebT (postRm ++= (p, text)) xs
      (p, ".preinst") ->   readFile (debian </> name) >>= \ text -> execDebT (preInst ++= (p, text)) xs
      (p, ".prerm") ->     readFile (debian </> name) >>= \ text -> execDebT (preRm ++= (p, text)) xs
      (_, ".log") ->       return xs -- Generated by debhelper
      (_, ".debhelper") -> return xs -- Generated by debhelper
      (_, ".hs") ->        return xs -- Code that uses this library
      (_, ".setup") ->     return xs -- Compiled Setup.hs file
      (_, ".substvars") -> return xs -- Unsupported
      (_, "") ->           return xs -- File with no extension
      (_, x) | last x == '~' -> return xs -- backup file
      _ -> trace ("Ignored: " ++ debian </> name) (return xs)

-- | Read a line from a debian .links file
readLink :: Monad m => BinPkgName -> Text -> DebT m ()
readLink p line =
    case words line of
      [a, b] -> link +++= (p, (unpack a, unpack b))
      [] -> return ()
      _ -> trace ("Unexpected value passed to readLink: " ++ show line) (return ())

-- | Read a line from a debian .install file
readInstall :: Monad m => BinPkgName -> Text -> DebT m ()
readInstall p line =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> install +++= (p, (unpack (strip a), unpack (strip b)))

-- | Read a line from a debian .dirs file
readDir :: Monad m => BinPkgName -> Text -> DebT m ()
readDir p line = installDir +++= (p, unpack line)

inputCabalization :: MonadIO m => Top -> DebT m ()
inputCabalization top =
    do vb <- access verbosity >>= return . intToVerbosity'
       comp <- inputCompiler top
       compiler ~= Just comp
       flags <- access cabalFlagAssignments
       pkgDesc <- liftIO $ withCurrentDirectory (unTop top) $ do
         descPath <- defaultPackageDesc vb
         genPkgDesc <- readPackageDescription vb descPath
         case finalizePackageDescription (toList flags) (const True) (Platform buildArch buildOS) (compilerId comp) [] genPkgDesc of
           Left deps -> error $ "Missing dependencies in cabal package at " ++ show (unTop top) ++ ": " ++ show deps
           Right (pkgDesc, _) ->
               do bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                  return pkgDesc
       packageDescription ~= Just pkgDesc

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

inputCompiler :: MonadIO m => Top -> DebT m Compiler
inputCompiler top =
    do vb <- access verbosity >>= return . intToVerbosity'
       mCompilerVersion <- access compilerVersion
       inputCompiler' top vb mCompilerVersion

-- | Read the (GHC) compiler version specified by Cabal, optionally
-- changing the version number.
inputCompiler' :: MonadIO m => Top -> Verbosity -> Set Version -> DebT m Compiler
inputCompiler' top vb mCompilerVersion =
    liftIO $ withCurrentDirectory (unTop top) $ do
      (compiler', _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration vb
      let compiler'' = case Set.toList mCompilerVersion of
                         [] -> compiler'
                         [ver] -> compiler' {compilerId = CompilerId GHC ver}
                         xs -> error $ "Conflicting ghc versions requestion: " ++ show xs
      return compiler''

-- | Try to read the license file specified in the cabal package,
-- otherwise return a text representation of the License field.
inputLicenseFile :: MonadIO m => Top -> DebT m ()
inputLicenseFile top =
    do pkgDesc <- access packageDescription
       text <- liftIO $ maybe (return Nothing) (readFileMaybe . ((unTop top) </>) . licenseFile) pkgDesc
       copyright ~?= fmap Right text

-- | Try to compute a string for the the debian "Maintainer:" field using, in this order
--    1. the maintainer explicitly specified using 'Debian.Debianize.Monad.maintainer'
--    2. the maintainer field of the cabal package,
--    3. the value returned by getDebianMaintainer, which looks in several environment variables,
--    4. the signature from the latest entry in debian/changelog,
--    5. the Debian Haskell Group, pkg-haskell-maintainers@lists.alioth.debian.org
inputMaintainer :: MonadIO m => DebT m ()
inputMaintainer =
    do cabalMaintainer <- access packageDescription >>=
                          maybe (return Nothing)
                                (\ pkgDesc ->
                                      return $ case Cabal.maintainer pkgDesc of
                                                 "" -> Nothing
                                                 x -> either (const Nothing)
                                                             Just
                                                             (parseMaintainer (takeWhile (\ c -> c /= ',' && c /= '\n') x)))
       Lenses.maintainer ~?= cabalMaintainer
       debianMaintainer <- liftIO getDebianMaintainer
       Lenses.maintainer ~?= debianMaintainer
       changelogMaintainer <-
           do log <- get >>= return . getL Lenses.changelog
              case log of
                Just (ChangeLog (entry : _)) ->
                    case (parseMaintainer (logWho entry)) of
                      Left _e -> return $ Nothing -- Just $ NameAddr (Just "Invalid signature in changelog") (show e)
                      Right x -> return (Just x)
                _ -> return Nothing
       Lenses.maintainer ~?= changelogMaintainer

-- | Compute the Cabal data directory for a Linux install from a Cabal
-- package description.  This needs to match the path cabal assigns to
-- datadir in the dist/build/autogen/Paths_packagename.hs module, or
-- perhaps the path in the cabal_debian_datadir environment variable.
dataDir :: PackageDescription -> FilePath
dataDir p =
    let PackageName pkgname = pkgName . Cabal.package $ p in
    "usr/share" </> map toLower pkgname
