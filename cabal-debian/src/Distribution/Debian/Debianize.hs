{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Distribution.Debian.Debianize
    ( withEnvironmentArgs
    , withEnvironmentFlags
    , putEnvironmentArgs
    , debianize
    , runDebianize
    , callDebianize
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (SomeException, catch, throw)
import Control.Monad (mplus)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.Either (partitionEithers)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Version (showVersion)
import Debian.Control
import qualified Debian.Relation as D
import Debian.Release (parseReleaseName)
import Debian.Changes (ChangeLogEntry(..), parseChangeLog)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Debian.Version.String
import Distribution.Debian.Config (Flags(..), defaultFlags, missingDependencies')
import Distribution.Debian.DebHelper (DebAtom(..), controlFile, changeLog, toFiles)
import Distribution.Debian.Dependencies (PackageType(..), debianExtraPackageName, debianUtilsPackageName, debianSourcePackageName,
                                         debianDocPackageName, debianDevPackageName, debianProfPackageName)
import Distribution.Debian.Options (compileArgs)
import Distribution.Debian.PackageDescription (withSimplePackageDescription)
import Distribution.Debian.Relations (buildDependencies, docDependencies, allBuildDepends, extraDebianLibs)
import Distribution.Debian.Server (execAtoms, Executable(..))
import Distribution.Debian.Splits (versionSplits)
import Distribution.Debian.Utility
import Distribution.License (License(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(buildable), Executable(exeName, buildInfo))
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Text (display)
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(NoSuchThing))
import Prelude hiding (catch, writeFile)
import System.Directory
import System.Environment
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory, splitFileName)
import System.IO (hPutStrLn, stderr)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.PrettyPrint.ANSI.Leijen (pretty)

type Debianization = [DebAtom]

-- deriving instance Ord (Field' String)

-- | Call a function with a list of strings read from the value of
-- $CABALDEBIAN.
withEnvironmentArgs :: ([String] -> IO a) -> IO a
withEnvironmentArgs f =
  (getEnv "CABALDEBIAN" >>= return . read) `catch` handle >>= f
  where
    handle :: IOError -> IO [String]
    handle _ = return []

-- | Read the value of $CABALDEBIAN as a list of command line
-- arguments, construct a ('Flags' -> 'Flags') function from them, and
-- compose it with a function that takes a 'Flags' record.
withEnvironmentFlags :: Flags -> (Flags -> IO a) -> IO a
withEnvironmentFlags flags0 f =
  (getEnv "CABALDEBIAN" >>= return . compileArgs . read) `catch` handle >>= \ ff ->
  let flags = ff flags0 in
  f flags
  where
    handle :: IOError -> IO (Flags -> Flags)
    handle _ = return id

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs flags = setEnv "CABALDEBIAN" (show flags) True

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- | Try to run the custom script in debian/Debianize.hs.
runDebianize :: [String] -> IO Bool
runDebianize args =
    do exists <- doesFileExist "debian/Debianize.hs"
       case exists of
         True ->
             do result <- run `catch` (\ (e :: SomeException) -> ePutStrLn ("runDebianize failed: " ++ show e) >> return False)
                if result then ePutStrLn "runDebianize succeeded" else ePutStrLn "runDebianize failed:"
                return result
         False ->
             do ePutStrLn "No custom debianization"
                return False
    where
      run =
          do putEnvironmentArgs args
             (code, _out, _err) <- readProcessWithExitCode "runhaskell" ("debian/Debianize.hs" : args) ""
             return (code == ExitSuccess)
       -- foldFailure (\ n -> error "Setup.debianize failed") out
       -- exists <- doesFileExist "debian/compat" `catch` (\ (_ :: IOError) -> return False)
       -- unless exists (error "debian/compat was not created")

callDebianize :: [String] -> IO ()
callDebianize args =
    debianize ((compileArgs args defaultFlags) {buildDir = "dist-ghc/build"})

-- | Generate a debianization for the cabal package in the current
-- directory using information from the .cabal file and from the
-- options in Flags.  This ignores any existing debianization except
-- for the debian/changelog file.  A new entry changelog is generated,
-- and any entries already there that look older than the new one are
-- preserved.
debianize :: Flags -> IO ()
debianize flags0 =
    withEnvironmentFlags flags0 $ \ flags ->
    withSimplePackageDescription flags $ \ pkgDesc compiler -> do
      old <- readDebianization
      let flags' = flags {buildDeps = buildDeps flags ++ if selfDepend flags then ["libghc-cabal-debian-dev"] else []}
      new <- (modifyAtoms flags <$> debianization flags' pkgDesc compiler old) >>= mapM (prepareAtom flags) >>= return . concat
      -- It is imperitive that during the time that dpkg-buildpackage
      -- runs the version number in the changelog and the source and
      -- package names in the control file do not change, or the bulid
      -- will fail.  To ensure this set the validate flag.  This means
      -- you probably need to run debianize before starting
      -- dpkg-buildpackage.  However, it is still good to be able to
      -- put the debianize parameters in the Setup file, rather than
      -- storing them apart from the package in the autobuilder
      -- configuration.
      case () of
        _ | (validate flags') ->
              do let oldVersion = logVersion (head (changeLog old))
                     newVersion = logVersion (head (changeLog new))
                     oldSource = fromJust (lookupP "Source" (head (unControl (controlFile old))))
                     newSource = fromJust (lookupP "Source" (head (unControl (controlFile new))))
                     oldPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile old))))
                     newPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile new))))
                 case () of
                   _ | oldVersion /= newVersion -> error ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion))
                     | oldSource /= newSource -> error ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource))
                     | oldPackages /= newPackages -> error ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages))
                     | True -> return ()
          | dryRun flags' -> putStrLn "Debianization (dry run):" >> describeDebianization flags new >>= putStr
          | True -> writeDebianization flags new

-- | Turn the DHFile atoms into DHInstal atoms, and create the file
-- they are supposed to install.  These files are part of the
-- debianization, and they need to go somewhere they won't be removed
-- by dh_clean.
prepareAtom :: Flags -> DebAtom -> IO [DebAtom]
prepareAtom _flags (DHFile b path s) =
    do let s' = fromString s
           (destDir, destName) = splitFileName path
           tmpDir = "debian/cabalInstall" </> show (md5 s')
       createDirectoryIfMissing True tmpDir
       L.writeFile (tmpDir </> destName) s'
       return [DHInstall b (tmpDir </> destName) destDir]
prepareAtom _ x = return [x]

readDebianization :: IO Debianization
readDebianization = do
  oldLog <- (readFile "debian/changelog" >>= return . (: []) . DebChangelog . parseChangeLog) `catch` handleDoesNotExist :: IO [DebAtom]
  oldControl <- (parseControlFromFile "debian/control" >>= return . either (error . show) ((: []) . DebControl)) `catch` handleDoesNotExist :: IO [DebAtom]
  return $ oldLog ++ oldControl
  -- let oldControl' = either (\ (e :: SomeException) -> []) (either (\ _ -> []) (\ c -> [DebControl c])) oldControl
  -- return $ oldControl' ++ [DebChangelog oldLog]

handleDoesNotExist :: IOError -> IO Debianization
handleDoesNotExist e | ioe_type e == NoSuchThing = return []
handleDoesNotExist e = throw e

debianization :: Flags		 -- ^ command line flags
              -> PackageDescription  -- ^ info from the .cabal file
              -> Compiler            -- ^ compiler details
              -> Debianization
              -> IO Debianization
debianization flags pkgDesc compiler oldDeb =
    do date <- getCurrentLocalRFC822Time
       copyright <- readFile' (licenseFile pkgDesc) `catch` (\ (_ :: SomeException) -> return . showLicense . license $ pkgDesc)
       maint <- getMaintainer flags pkgDesc >>= maybe (error "Missing value for --maintainer") return
       let newLog = updateChangelog flags maint pkgDesc date (changeLog oldDeb)
       return $ control flags compiler maint pkgDesc ++
                   [ cdbsRules pkgDesc
                   , DebChangelog newLog
                   , DebCompat 7 -- should this be hardcoded, or automatically read from /var/lib/dpkg/status?
                   , DebCopyright copyright
                   , DebSourceFormat (sourceFormat flags ++ "\n")
                   , watch pkgname ] ++
                   concatMap execAtoms (executablePackages flags)
    where
        PackageName pkgname = pkgName . package $ pkgDesc

-- | This is the only file I am confident I can merge with an existing
-- file, since the entries are dated and marked with well understood
-- version numbers.
updateChangelog :: Flags -> String -> PackageDescription -> String -> [ChangeLogEntry] -> [ChangeLogEntry]
updateChangelog flags debianMaintainer pkgDesc date oldEntries =
    case dropWhile (\ entry -> logVersion entry > logVersion newEntry) oldEntries of
      entry@(Entry {logVersion = d}) : older | d == logVersion newEntry -> merge entry newEntry : older
      entries -> newEntry : entries
    where
      newEntry = Entry { logPackage = show (D.prettyPkgName (debianSourcePackageName' pkgDesc))
                       , logVersion = updateOriginal f $ debianVersionNumber pkgDesc
                       , logDists = [parseReleaseName "unstable"]
                       , logUrgency = "low"
                       , logComments = "  * Debianization generated by cabal-debian\n\n"
                       , logWho = debianMaintainer
                       , logDate = date }
      f s = maybe (g s) (\ d -> if older d s then error ("Version from --deb-version (" ++ show d ++ ") is older than hackage version (" ++ show s ++ "), maybe you need to unpin this package?") else d) (debVersion flags)
      g s = maybe "" (\ n -> show n ++ ":") (Map.lookup (pkgName (package pkgDesc)) (epochMap flags)) ++ s ++ revision flags
      older debv cabv = parseDebianVersion debv < parseDebianVersion cabv
      merge :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
      merge old new =
          old { logComments = logComments old ++ logComments new
              , logDate = date }

updateOriginal :: (String -> String) -> DebianVersion -> DebianVersion
updateOriginal f v = parseDebianVersion . f . show . prettyDebianVersion $ v

debianVersionNumber :: PackageDescription -> DebianVersion
debianVersionNumber pkgDesc = parseDebianVersion . showVersion . pkgVersion . package $ pkgDesc

describeDebianization :: Flags -> Debianization -> IO String
describeDebianization flags d =
    mapM (\ (path, text) -> liftIO (doFile path text)) (debianizationFiles flags d) >>= return . concat
    where
      doFile :: FilePath -> String -> IO String
      doFile path text =
          doesFileExist path >>= \ exists ->
              if exists
              then diffFile path text >>= return . maybe (path ++ ": Unchanged\n") (\ diff -> path ++ ": Modified\n" ++ indent " | " diff)
              else return $ path ++ ": Created\n" ++ indent " | " text

indent :: [Char] -> String -> String
indent prefix text = unlines (map (prefix ++) (lines text))

writeDebianization :: Flags -> Debianization -> IO ()
writeDebianization flags d =
    mapM_ (uncurry doFile) (debianizationFiles flags d) >>
    getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
    where
      doFile path text =
          createDirectoryIfMissing True (takeDirectory path) >>
          replaceFile path text

debianizationFiles :: Flags -> Debianization -> [(FilePath, String)]
debianizationFiles flags = toFiles (buildDir flags)

watch :: String -> DebAtom
watch pkgname =
    DebWatch $
    "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
    "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
    " \\\n    ([\\d\\.]*\\d)/\n"

getMaintainer :: Flags -> PackageDescription -> IO (Maybe String)
getMaintainer flags pkgDesc =
    do debianMaintainer <- getDebianMaintainer
       return $ case (debMaintainer flags, cabalMaintainer, debianMaintainer) of
                  (Just x, _, _) -> Just x
                  (_, Just x, _) -> Just x
                  (_, _, x) -> x
    where
      cabalMaintainer =
          case maintainer pkgDesc of
            "" -> Nothing
            maint -> Just $ takeWhile (\ c -> c /= ',' && c /= '\n') maint

{-
Create a debian maintainer field from the environment variables:

  DEBFULLNAME (preferred) or NAME
  DEBEMAIL (preferred) or EMAIL

More work could be done to match dch, but this is sufficient for
now. Here is what the man page for dch has to say:

 If the environment variable DEBFULLNAME is set, this will be used for
 the maintainer full name; if not, then NAME will be checked.  If the
 environment variable DEBEMAIL is set, this will be used for the email
 address.  If this variable has the form "name <email>", then the
 maintainer name will also be taken from here if neither DEBFULLNAME
 nor NAME is set.  If this variable is not set, the same test is
 performed on the environment variable EMAIL.  Next, if the full name
 has still not been determined, then use getpwuid(3) to determine the
 name from the pass‐word file.  If this fails, use the previous
 changelog entry.  For the email address, if it has not been set from
 DEBEMAIL or EMAIL, then look in /etc/mailname, then attempt to build
 it from the username and FQDN, otherwise use the email address in the
 previous changelog entry.  In other words, it’s a good idea to set
 DEBEMAIL and DEBFULLNAME when using this script.

-}
getDebianMaintainer :: IO (Maybe String)
getDebianMaintainer =
    do env <- map (second decodeString) `fmap` getEnvironment
       return $ do fullname <- lookup "DEBFULLNAME" env `mplus` lookup "NAME" env
                   email    <- lookup "DEBEMAIL" env `mplus` lookup "EMAIL" env
                   return (fullname ++ " <" ++ email ++ ">")

-- | Generate the debian/rules file.
cdbsRules :: PackageDescription -> DebAtom
cdbsRules pkgDesc =
    DebRulesHead $ unlines $
          ["#!/usr/bin/make -f",
           "",
           -- This is marked obsolete by cdbs.  However, it needs to
           -- be a string that would work between "libghc-" and
           -- "-dev", so no underscores and no capital letters.  In
           -- hlibrary.mk there is a python expression that looks like
           -- it would do this for us.
           "DEB_CABAL_PACKAGE = " ++ show (D.prettyPkgName (debianExtraPackageName' pkgDesc)),
           "",
           "include /usr/share/cdbs/1/rules/debhelper.mk",
           "include /usr/share/cdbs/1/class/hlibrary.mk",
{-         "# How to install an extra file into the documentation package",
           "#binary-fixup/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "::",
           "#\techo \"Some informative text\" > debian/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "/usr/share/doc/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "/AnExtraDocFile", -}
          "" ]
    -- where
      -- p = pkgName . package $ pkgDesc
      -- v = pkgVersion . package $ pkgDesc
      -- libDir = unPackageName p ++ "-" ++ showVersion v
      -- docDeb = debianDocPackageName versionSplits (pkgName (package pkgDesc)) (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))
      -- utilsDeb = debianUtilsPackageName versionSplits (pkgName (package pkgDesc)) (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))
      --exeDeb e = debianExtraPackageName (PackageName (exeName e)) Nothing

-- | Functions that apply the mapping from cabal names to debian names based on version numbers.
debianSourcePackageName' :: PackageDescription -> D.SrcPkgName
debianSourcePackageName' pkgDesc =
         debianSourcePackageName versionSplits (pkgName . package $ pkgDesc)
                                     (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianUtilsPackageName' :: PackageDescription -> D.BinPkgName
debianUtilsPackageName' pkgDesc =
         debianUtilsPackageName versionSplits (pkgName (package pkgDesc))
                                    (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianDocPackageName' :: PackageDescription -> D.BinPkgName
debianDocPackageName' pkgDesc =
         debianDocPackageName versionSplits (pkgName (package pkgDesc))
                                  (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianDevPackageName' :: PackageDescription -> D.BinPkgName
debianDevPackageName' pkgDesc =
         debianDevPackageName versionSplits (pkgName (package pkgDesc))
                                  (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianProfPackageName' :: PackageDescription -> D.BinPkgName
debianProfPackageName' pkgDesc =
         debianProfPackageName versionSplits (pkgName (package pkgDesc))
                                   (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianExtraPackageName' :: PackageDescription -> D.BinPkgName
debianExtraPackageName' pkgDesc =
         debianExtraPackageName versionSplits (pkgName (package pkgDesc))
                                    (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

-- | The control file consists of a Source paragraph and one or more
-- Binary paragraphs, each one representing a binary package to be
-- produced.  If the package contains a library we usually want dev,
-- prof, and doc packages.
control :: Flags -> Compiler -> String -> PackageDescription -> [DebAtom] -- (Control' String, [(FilePath, String)], [String])
control flags compiler maint pkgDesc =
    -- trace ("allBuildDepends " ++ show flags ++ " -> " ++ show (allBuildDepends flags pkgDesc)) $
    (DebControl (Control {unControl = ([sourceSpec] ++ librarySpecs ++ controlSpecs)}) : concat atoms)
    where
      librarySpecs = maybe [] (const (develLibrarySpec ++ profileLibrarySpec ++ docLibrarySpec)) (library pkgDesc)
      develLibrarySpec = [librarySpec "any" Development debianDevPackageName']
      profileLibrarySpec = if debLibProf flags then [librarySpec "any" Profiling debianProfPackageName'] else []
      docLibrarySpec = if haddock flags then [docSpecsParagraph] else []

      (controlSpecs, atoms) = unzip $ execAndUtilSpecs flags pkgDesc debianDescription

      sourceSpec =
          Paragraph
          ([Field ("Source", " " ++ show (D.prettyPkgName (debianSourcePackageName' pkgDesc))),
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities
            Field ("Priority", " " ++ "optional"),
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-subsections
            Field ("Section", " " ++ "haskell"),
            Field ("Maintainer", " " ++ maint),
            Field ("Build-Depends", " " ++ showDeps' "Build-Depends:" (filterMissing (missingDependencies' flags) debianBuildDeps)),
            Field ("Build-Depends-Indep", " " ++ showDeps' "Build-Depends-Indep:" (filterMissing (missingDependencies' flags) debianBuildDepsIndep)),
            --Field ("Build-Depends-Indep", " " ++ buildDepsIndep),
            Field ("Standards-Version", " " ++ "3.9.3"),
            Field ("Homepage",
                   " " ++
                   if homepage pkgDesc == ""
                   then "http://hackage.haskell.org/package/" ++
                        unPackageName (pkgName $ package pkgDesc)
                   else homepage pkgDesc)])

      librarySpec arch typ debianName =
          Paragraph
          [Field ("Package", " " ++ show (D.prettyPkgName (debianName pkgDesc))),
           Field ("Architecture", " " ++ arch),
           Field ("Depends", " " ++ showDeps' "Depends:" (filterMissing (missingDependencies' flags) (
                     (if typ == Development
                      then [anyrel "${shlibs:Depends}"] ++
                           map anyrel (extraDevDeps flags) ++
                           extraDebianLibs (extraLibMap flags) pkgDesc
                      else []) ++
                     ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                      extraDeps (debianName pkgDesc) (binaryPackageDeps flags))))),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Provides", " " ++ "${haskell:Provides}"),
           Field ("Description", " " ++ libraryDescription typ)]

      docSpecsParagraph =
          Paragraph
          [Field ("Package", " " ++ show (D.prettyPkgName (debianDocPackageName' pkgDesc))),
           Field ("Architecture", " " ++ "all"),
           Field ("Section", " " ++ "doc"),
           Field ("Depends", " " ++ showDeps' "Depends:" (filterMissing (missingDependencies' flags)
                                                          ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                                                           extraDeps (debianDocPackageName' pkgDesc) (binaryPackageDeps flags)))),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Description", " " ++ libraryDescription Documentation)]

      -- The haskell-cdbs package contains the hlibrary.mk file with
      -- the rules for building haskell packages.
      debianBuildDeps :: D.Relations
      debianBuildDeps =
          nub $
          [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion "7.0"))) Nothing],
           [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion "0.8"))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
          (map anyrel (buildDeps flags)) ++
          (if debLibProf flags then [anyrel "ghc-prof"] else []) ++
          (concat . map (buildDependencies (epochMap flags) (execMap flags) compiler) . allBuildDepends (extraLibMap flags) $ pkgDesc)
      debianBuildDepsIndep :: D.Relations
      debianBuildDepsIndep =
          nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies (epochMap flags) compiler) . allBuildDepends (extraLibMap flags) $ pkgDesc)
      debianDescription =
          (unwords . words . synopsis $ pkgDesc) ++
          case description pkgDesc of
            "" -> ""
            text ->
                let text' = text ++ "\n" ++
                            list "" ("\n Author: " ++) (author pkgDesc) ++
                            list "" ("\n Upstream-Maintainer: " ++) (maintainer pkgDesc) ++
                            list "" ("\n Url: " ++) (pkgUrl pkgDesc) in
                "\n " ++ (trim . intercalate "\n " . map addDot . lines $ text')
      addDot line = if all (flip elem " \t") line then "." else line
      libraryDescription Profiling = debianDescription ++ "\n .\n This package contains the libraries compiled with profiling enabled."
      libraryDescription Development = debianDescription ++ "\n .\n This package contains the normal library files."
      libraryDescription Documentation = debianDescription ++ "\n .\n This package contains the documentation files."
      libraryDescription x = error $ "Unexpected library package name suffix: " ++ show x

      list :: b -> ([a] -> b) -> [a] -> b
      list d f l = case l of [] -> d; _ -> f l

execAndUtilSpecs :: Flags -> PackageDescription -> String -> [(Paragraph' String, [DebAtom])] -- [(Paragraph' String, (FilePath, String), [String])]
execAndUtilSpecs flags pkgDesc debianDescription =
    map makeExecutablePackage (executablePackages flags) ++ makeUtilsPackage
    where
      makeExecutablePackage p =
          (Paragraph
           ([Field ("Package", " " ++ debName p),
             Field ("Architecture", " " ++ "any"),
             Field ("Section", " " ++ "misc"),
             Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps (D.BinPkgName (debName p)) (binaryPackageDeps flags)))),
             Field ("Description", " " ++ maybe debianDescription (const executableDescription) (library pkgDesc))] ++
            conflicts (filterMissing (missingDependencies' flags) (extraDeps (b p) (binaryPackageConflicts flags)))),
           [DebRules ("build" </> debName p ++ ":: build-ghc-stamp")])
      b p = D.BinPkgName (debName p)
      executableDescription = " " ++ "An executable built from the " ++ display (pkgName (package pkgDesc)) ++ " package."
      makeUtilsPackage =
          case (bundledExecutables, dataFiles pkgDesc) of
            ([], []) ->
                []
            _ ->
                let p = debianUtilsPackageName' pkgDesc
                    p' = show (D.prettyPkgName p)
                    -- s = debianSourcePackageName' pkgDesc
                    -- s' = show (D.prettySrcPkgName s)
                    c = package pkgDesc
                    c' = display c in
                [(Paragraph
                  ([Field ("Package", " " ++ p'),
                    Field ("Architecture", " " ++ "any"),
                    Field ("Section", " " ++ "misc"),
                    Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps p (binaryPackageDeps flags)))),
                    Field ("Description", " " ++ maybe debianDescription (const utilsDescription) (library pkgDesc))] ++
                   conflicts (extraDeps p (binaryPackageConflicts flags))),
                  (map (\ e -> DHInstallCabalExec p (exeName e) "usr/bin") bundledExecutables ++
                   map (\ f -> DHInstall p f (takeDirectory ("usr/share" </> c' </> f))) (dataFiles pkgDesc) ++
                   [DebRules ("build" </> p' ++ ":: build-ghc-stamp")])
                 )]
      utilsDescription = " " ++ "Utility files associated with the " ++ display (package pkgDesc) ++ " package."
      bundledExecutables = filter (\ p -> not (elem (exeName p) (map execName (executablePackages flags))))
                                  (filter (buildable . buildInfo) (executables pkgDesc))

conflicts :: [[D.Relation]] -> [Field' String]
conflicts [] = []
conflicts [[]] = [] -- I don't think this happens
conflicts rels = [Field ("Conflicts", " " ++ showDeps rels)]

extraDeps :: D.BinPkgName -> [(D.BinPkgName, D.BinPkgName)] -> [[D.Relation]]
extraDeps p deps =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map (mkDep . snd) pairs
    where mkDep name = [D.Rel name Nothing Nothing]

anyrel :: String -> [D.Relation]
anyrel x = [D.Rel (D.BinPkgName x) Nothing Nothing]

-- generated with:
-- apt-cache show ghc \
--   | grep ^Provides: \
--   | cut -d\  -f2-
--   | sed 's/, /\n/g' \
--   | grep libghc- \
--   | cut -d- -f2- \
--   | grep dev$ \
--   | sed 's/-dev//;s/$/",/;s/^/"/'

{-
base :: Set String
base
  = Data.Set.fromList
    ["array",
      "base",
      "bin-package-db",
      "bytestring",
      "cabal",
      "containers",
      "directory",
      "extensible-exceptions",
      "filepath",
      "ghc-binary",
      "ghc-prim",
      "haskell2010",
      "haskell98",
      "hpc",
      "integer-gmp",
      "old-locale",
      "old-time",
      "pretty",
      "process",
      "random",
      "rts",
      "template-haskell",
      "time",
      "unix"]
-}

-- | Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.

showLicense :: License -> String
showLicense (GPL _) = "GPL"
showLicense (LGPL _) = "LGPL"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD-like"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Non-distributable"
showLicense MIT = "MIT"
showLicense (UnknownLicense _) = "Unknown"
