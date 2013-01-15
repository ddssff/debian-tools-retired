{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Debian.Debianize.Debianize
    ( cabalDebian
    , callDebianize
    , runDebianize
    , cabalToDebianization
    ) where

import Data.Maybe
import Data.Text (Text)
import Debian.Debianize.Atoms (packageDescription, dependencyHints, defaultAtoms, flags, watchAtom)
import Debian.Debianize.Cabal (withSimplePackageDescription, inputCopyright, inputMaintainer)
import Debian.Debianize.Combinators (control, cdbsRules, putCopyright, filterMissing, versionInfo, addExtraLibDependencies, putStandards)
import Debian.Debianize.Flags (flagOptions, atomOptions)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Output (outputDebianization)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types.Atoms (HasAtoms(getAtoms, putAtoms), Flags(..), DebAction(..), AtomMap)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..))
import Debian.Debianize.Types.Dependencies (missingDependencies)
import Debian.Debianize.Utility (withCurrentDirectory)
import Debian.Policy (StandardsVersion)
import Debian.Time (getCurrentLocalRFC822Time)
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, unlines)
import System.Console.GetOpt (ArgOrder(..), getOpt', OptDescr(..), usageInfo)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (catchIOError)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

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
withEnvironmentFlags :: Config -> (Config -> IO a) -> IO a
withEnvironmentFlags config0 f =
  (getEnv "CABALDEBIAN" >>= return . compileArgs . read) `catch` handle >>= \ ff ->
  let config = config0 {flags = ff (flags config0)} in
  f config
  where
    handle :: IOError -> IO (Flags -> Flags)
    handle _ = return id

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs flags = setEnv "CABALDEBIAN" (show flags) True

-- | Try to run the custom script in debian/Debianize.hs to create the
-- debianization.  This can first put some extra arguments into the
-- @CABALDEBIAN@ environment variable.  Often this is used to set the
-- dryRun option by passing @["-n"]@.
runDebianize :: [String] -> IO Bool
runDebianize args =
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True ->
          putEnvironmentArgs args >> readProcessWithExitCode "runhaskell" ("debian/Debianize.hs" : args) "" >>= \ result ->
          case result of
            (ExitSuccess, _, _) -> return True
            (code, out, err) ->
              error ("runDebianize failed with " ++ show code ++ ":\n stdout: " ++ show out ++"\n stderr: " ++ show err)

-- | Run the debianize function with arguments pulled out of the
-- @CABALDEBIAN@ environment variable, with the build directory set to
-- match what dpkg-buildpackage will use later when it uses the
-- resulting debianization.
callDebianize :: [String] -> IO ()
callDebianize args =
    debianize (defaultConfig (compileArgs args defaultFlags))

-- | Generate a debianization for the cabal package in the current
-- directory using information from the .cabal file and from the
-- options in Flags.  This ignores any existing debianization except
-- for the debian/changelog file.  A new entry changelog is generated,
-- and any entries already there that look older than the new one are
-- preserved.
debianize :: Config -> IO ()
debianize config0 =
    withEnvironmentFlags config0 $ \ config ->
    withSimplePackageDescription config $ \ pkgDesc compiler -> do
      old <- readDebianization
      new <- debianization config pkgDesc compiler old >>= mapM (prepareAtom config) >>= return . concat
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
        _ | (validate (flags config)) ->
              do let oldVersion = logVersion (head (unChangeLog (changeLog old)))
                     newVersion = logVersion (head (unChangeLog (changeLog new)))
                     oldSource = fromJust (lookupP "Source" (head (unControl (controlFile old))))
                     newSource = fromJust (lookupP "Source" (head (unControl (controlFile new))))
                     oldPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile old))))
                     newPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile new))))
                 case () of
                   _ | oldVersion /= newVersion -> error ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion))
                     | oldSource /= newSource -> error ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource))
                     | oldPackages /= newPackages -> error ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages))
                     | True -> return ()
          | dryRun (flags config) -> putStrLn "Debianization (dry run):" >> describeDebianization (flags config) new >>= putStr
          | True -> writeDebianization (flags config) new

unChangeLog :: ChangeLog -> [ChangeLogEntry]
unChangeLog (ChangeLog x) = x

-- | Turn the DHFile atoms into DHInstal atoms, and create the file
-- they are supposed to install.  These files are part of the
-- debianization, and they need to go somewhere they won't be removed
-- by dh_clean.
prepareAtom :: Config -> DebAtom -> IO [DebAtom]
prepareAtom _config (DHFile b path s) =
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

debianization :: Config		     -- ^ configuration info
              -> PackageDescription  -- ^ info from the .cabal file
              -> Compiler            -- ^ compiler details
              -> Debianization
              -> IO Debianization
debianization config pkgDesc compiler oldDeb =
    do date <- getCurrentLocalRFC822Time
       copyright <- readFile' (licenseFile pkgDesc) `catch` (\ (_ :: SomeException) -> return . showLicense . license $ pkgDesc)
       maint <- getMaintainer (flags config) pkgDesc >>= maybe (error "Missing value for --maintainer") return
       let newLog = updateChangelog (flags config) maint pkgDesc date (changeLog oldDeb)
           atoms = control (flags config) compiler maint pkgDesc ++
                   [ cdbsRules pkgDesc
                   , DebChangelog newLog
                   , DebCompat 7 -- should this be hardcoded, or automatically read from /var/lib/dpkg/status?
                   , DebCopyright copyright
                   , DebSourceFormat (sourceFormat (flags config) ++ "\n")
                   , watch pkgname ] ++
                   concatMap execAtoms (executablePackages (flags config))
       return $ map fixups $ modifyAtoms config $ atoms
    where
      -- Turn the DHInstallData directives into DHInstallTo
      fixups (DHInstallData p s d) = DHInstallTo p s ("usr/share" </> (pkgname ++ "-" ++ (showVersion . pkgVersion . package $ pkgDesc)) </> makeRelative "/" d)
      fixups x = x
      PackageName pkgname = pkgName . package $ pkgDesc

-- | This is the only file I am confident I can merge with an existing
-- file, since the entries are dated and marked with well understood
-- version numbers.
updateChangelog :: Flags -> String -> PackageDescription -> String -> ChangeLog -> ChangeLog
updateChangelog flags debianMaintainer pkgDesc date (ChangeLog oldEntries) =
    case dropWhile (\ entry -> logVersion entry > logVersion newEntry) oldEntries of
      entry@(Entry {logVersion = d}) : older | d == logVersion newEntry -> ChangeLog (merge entry newEntry : older)
      entries -> ChangeLog (newEntry : entries)
    where
      newEntry = Entry { logPackage = show (pretty (debianSourcePackageName' pkgDesc))
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
                  (_, _, Just x) -> Just x
                  _ -> Just "Debian Haskell Group <pkg-haskell-maintainers@lists.alioth.debian.org>"
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
           "DEB_CABAL_PACKAGE = " ++ show (pretty (debianExtraPackageName' pkgDesc)),
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
          ([Field ("Source", " " ++ show (pretty (debianSourcePackageName' pkgDesc))),
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
          [Field ("Package", " " ++ show (pretty (debianName pkgDesc))),
           Field ("Architecture", " " ++ arch),
           Field ("Depends", " " ++ showDeps' "Depends:" (filterMissing (missingDependencies' flags) (
                     (if typ == Development
                      then [anyrel "${shlibs:Depends}"] ++
                           map anyrel (extraDevDeps flags) ++
                           extraDebianLibs (extraLibMap flags) pkgDesc
                      else []) ++
                     ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                      extraDeps (debianName pkgDesc) (binaryPackageDeps flags))))),
           Field ("Conflicts", " " ++ "${haskell:Conflicts}"),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Provides", " " ++ "${haskell:Provides}"),
           Field ("Description", " " ++ libraryDescription typ)]

      docSpecsParagraph =
          Paragraph
          [Field ("Package", " " ++ show (pretty (debianDocPackageName' pkgDesc))),
           Field ("Architecture", " " ++ "all"),
           Field ("Section", " " ++ "doc"),
           Field ("Depends", " " ++ showDeps' "Depends:" (filterMissing (missingDependencies' flags)
                                                          ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                                                           extraDeps (debianDocPackageName' pkgDesc) (binaryPackageDeps flags)))),
           Field ("Conflicts", " " ++ "${haskell:Conflicts}"),
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
          (concat . map (buildDependencies (epochMap flags) (execMap flags) compiler) . filter (not . selfDependency pkgDesc) . allBuildDepends (extraLibMap flags) $ pkgDesc)
      debianBuildDepsIndep :: D.Relations
      debianBuildDepsIndep =
          nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies (epochMap flags) compiler) . filter (not . selfDependency pkgDesc) . allBuildDepends (extraLibMap flags) $ pkgDesc)
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
      libraryDescription Profiling =
          debianDescription ++ "\n" ++
          unlines [" .",
                   " This package provides a library for the Haskell programming language, compiled",
                   " for profiling.  See http:///www.haskell.org/ for more information on Haskell."]
      libraryDescription Development =
          debianDescription ++ "\n" ++
          unlines [" .",
                   " This package provides a library for the Haskell programming language.",
                   " See http:///www.haskell.org/ for more information on Haskell."]
      libraryDescription Documentation =
          debianDescription ++ "\n" ++
          unlines [" .",
                   " This package provides the documentation for a library for the Haskell",
                   " programming language.",
                   " See http:///www.haskell.org/ for more information on Haskell." ]
      libraryDescription x = error $ "Unexpected library package name suffix: " ++ show x

      list :: b -> ([a] -> b) -> [a] -> b
      list d f l = case l of [] -> d; _ -> f l

-- | Generate the control file sections and other debhelper atoms for
-- the executable and utiltity packages.
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
             Field ("Conflicts", " " ++ "${haskell:Conflicts}"),
             Field ("Description", " " ++ maybe debianDescription (const executableDescription) (library pkgDesc))] ++
            conflicts (filterMissing (missingDependencies' flags) (extraDeps (b p) (binaryPackageConflicts flags)))),
           [DebRules ("build" </> debName p ++ ":: build-ghc-stamp")])
      executableDescription = " An executable built from the " ++ display (pkgName (package pkgDesc)) ++ " package."
      makeUtilsPackage =
          case (bundledExecutables, dataFiles pkgDesc) of
            ([], []) ->
                []
            _ ->
                let p = debianUtilsPackageName' pkgDesc
                    p' = show (pretty p)
                    -- s = debianSourcePackageName' pkgDesc
                    -- s' = show (D.prettySrcPkgName s)
                    c = package pkgDesc
                    c' = display c in
                [(Paragraph
                  ([Field ("Package", " " ++ p'),
                    Field ("Architecture", " " ++ "any"),
                    Field ("Section", " " ++ "misc"),
                    Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps p (binaryPackageDeps flags)))),
                    Field ("Conflicts", " " ++ "${haskell:Conflicts}"),
                    Field ("Description", " " ++ maybe debianDescription (const utilsDescription) (library pkgDesc))] ++
                   conflicts (extraDeps p (binaryPackageConflicts flags))),
                  (map (\ e -> DHInstallCabalExec p (exeName e) "usr/bin") bundledExecutables ++
                   map (\ f -> DHInstall p f (takeDirectory ("usr/share" </> c' </> f))) (dataFiles pkgDesc) ++
                   [DebRules ("build" </> p' ++ ":: build-ghc-stamp")])
                 )]
      utilsDescription = " " ++ "Utility files associated with the " ++ display (package pkgDesc) ++ " package."
      bundledExecutables = filter nopackage (filter (buildable . buildInfo) (executables pkgDesc))
      nopackage p = not (elem (exeName p) (map execName (executablePackages flags)))

b :: Distribution.Debian.Server.Executable -> D.BinPkgName
b p = D.BinPkgName (debName p)

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

compileEnvironmentArgs :: HasAtoms atoms => atoms -> IO atoms
compileEnvironmentArgs atoms0 =
    getEnv "CABALDEBIAN" >>= return . compileArgs atoms0 . read

-- | Compile the command line arguments into the atoms value and pass
-- to the action.
withFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withFlags def action = getArgs >>= action . compileArgs def

-- | Read the value of $CABALDEBIAN as a list of command line
-- arguments, construct a ('Flags' -> 'Flags') function from them, and
-- compose it with a function that takes a 'Flags' record.
withEnvironmentFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withEnvironmentFlags atoms0 f =
    (getEnv "CABALDEBIAN" >>= f . compileArgs atoms0 . read) `catchIOError` (\ _ -> f atoms0)

compileArgs :: HasAtoms atoms => atoms -> [String] -> atoms
compileArgs atoms args =
    case getOpt' RequireOrder (flagOptions ++ atomOptions) args of
      (os, [], [], []) -> foldl (flip ($)) atoms os
      (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                    ", Unrecognized: " ++ show unk ++
                                    ", Non-Options: " ++ show non)

-- | Call a function with a list of strings read from the value of
-- $CABALDEBIAN.
withEnvironmentArgs :: ([String] -> IO a) -> IO a
withEnvironmentArgs f =
  (getEnv "CABALDEBIAN" >>= return . read) `catchIOError` handle >>= f
  where
    handle :: IOError -> IO [String]
    handle _ = return []
