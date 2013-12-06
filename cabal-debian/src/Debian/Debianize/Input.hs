-- | Read an existing Debianization from a directory file.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , inputCompiler
    , inputCompiler'
    , inputCabalization
    , inputCabalization'
    , inputMaintainer
    , dataDir
    ) where

import Debug.Trace (trace)

import Control.Category ((.))
import Control.Exception (bracket)
import Control.Monad (when, foldM, filterM)
import Control.Monad.State (get, put)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.Char (isSpace, toLower)
import Data.Lens.Lazy (getL, setL, modL, access)
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, toList, fromList, insert, singleton)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(logWho), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import qualified Debian.Debianize.Types as T (maintainer)
import qualified Debian.Debianize.Types.Atoms as T (changelog)
import Debian.Debianize.Types.BinaryDebDescription (BinaryDebDescription, newBinaryDebDescription)
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.Types.Atoms
    (newAtoms, control, warning, sourceFormat, watch, rulesHead, compat, packageDescription, compiler,
     license, licenseFile, copyright, changelog, installInit, postInst, postRm, preInst, preRm,
     logrotateStanza, link, install, installDir, intermediateFiles, compilerVersion, cabalFlagAssignments, verbosity)
import Debian.Debianize.Monad (Atoms, DebT, execDebT)
import Debian.Debianize.Prelude (getDirectoryContents', withCurrentDirectory, readFileMaybe, read', intToVerbosity', (~=), (~?=), (+=), (++=), (+++=))
import Debian.Debianize.Types.Base (Top(unTop))
import Debian.Orphans ()
import Debian.Policy (Section(..), parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer,
                      parseUploaders, readSourceFormat, getDebianMaintainer)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
import Distribution.Package (Package(packageId), PackageIdentifier(..), PackageName(PackageName), Dependency)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, maintainer, package, license, copyright {-, synopsis, description-}))
import Distribution.PackageDescription as Cabal (PackageDescription, FlagName)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (readFile, lines, words, break, null, log, sum, (.))
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

inputSourceDebDescription :: Top -> DebT IO (S.SourceDebDescription, [Field])
inputSourceDebDescription top =
    do paras <- lift $ parseControlFromFile (unTop top </> "debian/control") >>= either (error . show) (return . unControl)
       case paras of
         [] -> error "Missing source paragraph"
         [_] -> error "Missing binary paragraph"
         (hd : tl) -> return $ parseSourceDebDescription hd tl

parseSourceDebDescription :: Paragraph' String -> [Paragraph' String] -> (S.SourceDebDescription, [Field])
parseSourceDebDescription (Paragraph fields) binaryParagraphs =
    foldr readField (src, []) fields'
    where
      fields' = map stripField fields
      src = setL S.binaryPackages bins (S.newSourceDebDescription' findSource findMaint)
      findSource = findMap "Source" SrcPkgName fields'
      findMaint = findMap "Maintainer" (\ m -> either (\ e -> error $ "Failed to parse maintainer field " ++ show m ++ ": " ++ show e) id . parseMaintainer $ m) fields'
      -- findStandards = findMap "Standards-Version" parseStandardsVersion fields'

      (bins, _extra) = unzip $ map parseBinaryDebDescription binaryParagraphs
      readField :: Field -> (S.SourceDebDescription, [Field]) -> (S.SourceDebDescription, [Field])
      -- Mandatory
      readField (Field ("Source", _)) x = x
      readField (Field ("Maintainer", _)) x = x
      -- readField (Field ("Standards-Version", _)) x = x
      -- Recommended
      readField (Field ("Standards-Version", value)) (desc, unrecognized) = (setL S.standardsVersion (Just (parseStandardsVersion value)) desc, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (setL S.priority (Just (readPriority value)) desc, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (setL S.section (Just (MainSection value)) desc, unrecognized)
      -- Optional
      readField (Field ("Homepage", value)) (desc, unrecognized) = (setL S.homepage (Just (strip (pack value))) desc, unrecognized)
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (setL S.uploaders (either (const []) id (parseUploaders value)) desc, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (setL S.dmUploadAllowed (yes value) desc, unrecognized)
      readField (Field ("Build-Depends", value)) (desc, unrecognized) = (setL S.buildDepends (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts", value)) (desc, unrecognized) = (setL S.buildConflicts (rels value) desc, unrecognized)
      readField (Field ("Build-Depends-Indep", value)) (desc, unrecognized) = (setL S.buildDependsIndep (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts-Indep", value)) (desc, unrecognized) = (setL S.buildConflictsIndep (rels value) desc, unrecognized)
      readField (Field ("Vcs-Browser", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSBrowser (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Arch", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSArch (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Bzr", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSBzr (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Cvs", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSCvs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Darcs", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSDarcs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Git", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSGit (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Hg", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSHg (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Mtn", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSMtn (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Svn", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSSvn (pack s)) vcsFields) desc, unrecognized)
      readField field@(Field ('X' : fld, value)) (desc, unrecognized) =
          case span (`elem` "BCS") fld of
            (xs, '-' : more) -> (modL S.xFields (\ xFields -> insert (S.XField (fromList (map (read' (\ s -> error $ "parseSourceDebDescription: " ++ show s) . (: [])) xs)) (pack more) (pack value)) xFields) desc, unrecognized)
            _ -> (desc, field : unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

parseBinaryDebDescription :: Paragraph' String -> (BinaryDebDescription, [Field])
parseBinaryDebDescription (Paragraph fields) =
    foldr readField (bin, []) fields'
    where
      fields' = map stripField fields
      bin = setL B.architecture (Just arch) (newBinaryDebDescription b)
      b :: BinPkgName
      b = findMap "Package" BinPkgName fields'
      arch = findMap "Architecture" parsePackageArchitectures fields'
{-
(BinPkgName (fromJust (fieldValue "Package" bin)))
(read' (fromJust (fieldValue "Architecture" bin)))
, []
    foldr readField (newBinaryDebDescription (BinPkgName (fromJust (fieldValue "Package" bin))) (read' (fromJust (fieldValue "Architecture" bin))), []) (map stripField fields)
-}

      readField :: Field -> (BinaryDebDescription, [Field]) -> (BinaryDebDescription, [Field])
      readField (Field ("Package", x)) (desc, unrecognized) = (setL B.package (BinPkgName x) desc, unrecognized)
      readField (Field ("Architecture", x)) (desc, unrecognized) = (setL B.architecture (Just (parsePackageArchitectures x)) desc, unrecognized)
      readField (Field ("Section", x)) (desc, unrecognized) = (setL B.binarySection (Just (readSection x)) desc, unrecognized)
      readField (Field ("Priority", x)) (desc, unrecognized) = (setL B.binaryPriority (Just (readPriority x)) desc, unrecognized)
      readField (Field ("Essential", x)) (desc, unrecognized) = (setL B.essential (Just (yes x)) desc, unrecognized)
      readField (Field ("Depends", x)) (desc, unrecognized) = (setL (B.depends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Recommends", x)) (desc, unrecognized) = (setL (B.recommends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Suggests", x)) (desc, unrecognized) = (setL (B.suggests . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Pre-Depends", x)) (desc, unrecognized) = (setL (B.preDepends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Breaks", x)) (desc, unrecognized) = (setL (B.breaks . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Conflicts", x)) (desc, unrecognized) = (setL (B.conflicts . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Provides", x)) (desc, unrecognized) = (setL (B.provides . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Replaces", x)) (desc, unrecognized) = (setL (B.replaces . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Built-Using", x)) (desc, unrecognized) = (setL (B.builtUsing . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Description", x)) (desc, unrecognized) = (setL B.description (Just (pack x)) desc, unrecognized)
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
inputAtoms debian name@"copyright" xs = readFile (debian </> name) >>= \ text -> execDebT (copyright ~= Just text) xs
inputAtoms debian name@"changelog" xs =
    readFile (debian </> name) >>= return . parseChangeLog . unpack >>= \ log -> execDebT (changelog ~= Just log) xs
inputAtoms debian name xs =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   readFile (debian </> name) >>= \ text -> execDebT (mapM_ (readInstall p) (lines text)) xs
      (p, ".dirs") ->      readFile (debian </> name) >>= \ text -> execDebT (mapM_ (readDir p) (lines text)) xs
      (p, ".init") ->      readFile (debian </> name) >>= \ text -> execDebT (installInit ++= (p, text)) xs
      (p, ".logrotate") -> readFile (debian </> name) >>= \ text -> execDebT (logrotateStanza +++= (p, singleton text)) xs
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
      [a, b] -> link +++= (p, singleton (unpack a, unpack b))
      [] -> return ()
      _ -> trace ("Unexpected value passed to readLink: " ++ show line) (return ())

-- | Read a line from a debian .install file
readInstall :: Monad m => BinPkgName -> Text -> DebT m ()
readInstall p line =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> install +++= (p, singleton (unpack (strip a), unpack (strip b)))

-- | Read a line from a debian .dirs file
readDir :: Monad m => BinPkgName -> Text -> DebT m ()
readDir p line = installDir +++= (p, singleton (unpack line))

inputCabalization :: MonadIO m => Top -> DebT m ()
inputCabalization top =
    do vb <- access verbosity >>= return . intToVerbosity'
       comp <- inputCompiler top
       compiler ~= Just comp
       flags <- access cabalFlagAssignments
       ePkgDesc <- liftIO $ inputCabalization' top vb comp flags
       either (\ deps -> error $ "Missing dependencies in cabal package at " ++ show (unTop top) ++ ": " ++ show deps)
              (\ pkgDesc -> do
                 packageDescription ~= Just pkgDesc
                 -- This will contain either the contents of the file given in
                 -- the license-file: field or the contents of the license:
                 -- field.
                 license ~?= (Just (Cabal.license pkgDesc))
                 licenseFileText <- liftIO $ case Cabal.licenseFile pkgDesc of
                                               "" -> return Nothing
                                               path -> readFileMaybe (unTop top </> path)
                 licenseFile ~?= licenseFileText
                 copyright ~?= (case Cabal.copyright pkgDesc of
                                  "" -> Nothing
                                  s -> Just (pack s)))
              ePkgDesc

inputCabalization' :: Top -> Verbosity -> CompilerId -> Set (FlagName, Bool) -> IO (Either [Dependency] PackageDescription)
inputCabalization' top vb compId flags =
    withCurrentDirectory (unTop top) $ do
         descPath <- defaultPackageDesc vb
         genPkgDesc <- readPackageDescription vb descPath
         case finalizePackageDescription (toList flags) (const True) (Platform buildArch buildOS) compId [] genPkgDesc of
           Left deps -> return (Left deps)
           Right (pkgDesc, _) ->
               do bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                  return (Right pkgDesc)

-- | Run the package's configuration script.
autoreconf :: Verbosity -> Cabal.PackageDescription -> IO ()
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

inputCompiler :: MonadIO m => Top -> DebT m CompilerId
inputCompiler top =
    do vb <- access verbosity >>= return . intToVerbosity'
       mCompilerVersion <- access compilerVersion
       liftIO $ inputCompiler' top vb mCompilerVersion

-- | Read the compiler version specified by Cabal, optionally
-- changing the version number.
inputCompiler' :: Top -> Verbosity -> Maybe Version -> IO CompilerId
inputCompiler' top vb mCompilerVersion =
    withCurrentDirectory (unTop top) $ do
      (Compiler {compilerId = CompilerId flavour version}, _) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration vb
      return $ case mCompilerVersion of
                         Nothing -> CompilerId flavour version
                         Just version' -> CompilerId flavour version'

-- | Try to compute a string for the the debian "Maintainer:" field using, in this order
--    1. the maintainer explicitly specified using "Debian.Debianize.Monad.maintainer"
--    2. the maintainer field of the cabal package,
--    3. the value returned by getDebianMaintainer, which looks in several environment variables,
--    4. the signature from the latest entry in debian/changelog,
--    5. the Debian Haskell Group, @pkg-haskell-maintainers\@lists.alioth.debian.org@
inputMaintainer :: MonadIO m => DebT m ()
inputMaintainer =
    do Just pkgDesc <- access packageDescription
       let cabalMaintainer = case Cabal.maintainer pkgDesc of
                               "" -> Nothing
                               x -> either (const Nothing) Just (parseMaintainer (takeWhile (\ c -> c /= ',' && c /= '\n') x))
       T.maintainer ~?= cabalMaintainer
       debianMaintainer <- liftIO getDebianMaintainer
       T.maintainer ~?= debianMaintainer
       changelogMaintainer <-
           do log <- get >>= return . getL T.changelog
              case log of
                Just (ChangeLog (entry : _)) ->
                    case (parseMaintainer (logWho entry)) of
                      Left _e -> return $ Nothing -- Just $ NameAddr (Just "Invalid signature in changelog") (show e)
                      Right x -> return (Just x)
                _ -> return Nothing
       T.maintainer ~?= changelogMaintainer

-- | Compute the Cabal data directory for a Linux install from a Cabal
-- package description.  This needs to match the path cabal assigns to
-- datadir in the dist/build/autogen/Paths_packagename.hs module, or
-- perhaps the path in the cabal_debian_datadir environment variable.
dataDir :: Cabal.PackageDescription -> FilePath
dataDir p =
    let PackageName pkgname = pkgName . Cabal.package $ p in
    "usr/share" </> map toLower pkgname
