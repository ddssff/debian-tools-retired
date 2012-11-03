{-# LANGUAGE CPP, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Distribution.Debian.Debianize
    ( debianize
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Arrow (second)
import Control.Exception (SomeException, catch)
import Control.Monad (mplus, unless)
import Data.Either (partitionEithers)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Version (showVersion)
import Debian.Control
import qualified Debian.Relation as D
import Debian.Release (parseReleaseName)
import Debian.Changes (ChangeLogEntry(..), prettyEntry, parseLog)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Debian.Version.String
import Distribution.Debian.Config (Flags(..), missingDependencies')
import Distribution.Debian.Dependencies (PackageType(..), debianExtraPackageName, debianUtilsPackageName, debianSourcePackageName,
                                         debianDocPackageName, debianDevPackageName, debianProfPackageName)
import Distribution.Debian.Relations (buildDependencies, docDependencies, allBuildDepends, versionSplits)
import Distribution.Debian.Utility
import Distribution.Text (display)
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.License (License(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (PackageDescription(..), exeName, Executable)
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.Class (pretty)

type Debianization = Map.Map FilePath String

debianization :: Flags		 -- ^ command line flags
              -> PackageDescription  -- ^ info from the .cabal file
              -> Compiler            -- ^ compiler details
              -> FilePath            -- ^ directory in which to create files
              -> IO Debianization
debianization flags pkgDesc compiler debian =
    do date <- getCurrentLocalRFC822Time
       copyright <- readFile' (licenseFile pkgDesc) `catch` (\ (_ :: SomeException) -> return . showLicense . license $ pkgDesc)
       debianMaintainer <- getDebianMaintainer flags >>= maybe (error "Missing value for --maintainer") return
       oldChangelog <- readFile (debian </> "changelog") `catch` (\ (_ :: SomeException) -> return "")
       let (errs, newLog) = updateChangelog flags debianMaintainer pkgDesc date oldChangelog
       mapM_ (hPutStrLn stderr) errs
       let (controlFile, rulesLines) = control flags compiler debianMaintainer pkgDesc
       return $ Map.fromList
                  (controlFile ++
                   [("changelog", newLog),
                    ("rules", cdbsRules pkgDesc ++ unlines rulesLines),
                    ("compat", "7\n"), -- should this be hardcoded, or automatically read from /var/lib/dpkg/status?
                    ("copyright", copyright),
                    ("source/format", sourceFormat flags ++ "\n"),
                    ("watch", watch pkgname)] ++
                   installFiles)
    where
        PackageName pkgname = pkgName . package $ pkgDesc
        installFiles = []

debianize :: Flags		 -- ^ command line flags
          -> PackageDescription  -- ^ info from the .cabal file
          -> Compiler            -- ^ compiler details
          -> FilePath            -- ^ directory in which to create files
          -> IO ()
debianize flags pkgDesc compiler debian =
    debianization flags pkgDesc compiler debian >>=
    mapM_ (uncurry doFile) . Map.toList >>
    unless (compareOnly flags) (getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True}))
    where
        doFile path text =
            let path' = debian </> path in
            if compareOnly flags
            then doesFileExist path' >>= \ exists ->
                 if exists
                 then diff path' text
                 else putStrLn ("New file: " ++ path') >> diff "/dev/null" text
            else createDirectoryIfMissing True (takeDirectory path') >>
                 replaceFile path' text
        diff path text =
            readProcessWithExitCode "diff" ["-ruw", path, "-"] text >>= \ (code, out, _err) ->
            case code of
              ExitSuccess -> putStr out
              ExitFailure 1 -> putStr out
              _ -> hPutStrLn stderr (showCommandForUser "diff" ["-r", "-u", path, "-"] ++ " < " ++ show text ++ " -> " ++ show code)

watch :: String -> String
watch pkgname =
    "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
    "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
    " \\\n    ([\\d\\.]*\\d)/\n"

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
getDebianMaintainer :: Flags -> IO (Maybe String)
getDebianMaintainer flags =
    case debMaintainer flags of
      Nothing -> envMaintainer
      maint -> return maint
    where
      envMaintainer :: IO (Maybe String)
      envMaintainer =
          do env <- map (second decodeString) `fmap` getEnvironment
             return $ do fullname <- lookup "DEBFULLNAME" env `mplus` lookup "NAME" env
                         email    <- lookup "DEBEMAIL" env `mplus` lookup "EMAIL" env
                         return (fullname ++ " <" ++ email ++ ">")

-- | Generate the debian/rules file.
cdbsRules :: PackageDescription -> String
cdbsRules pkgDesc =
    unlines $
          ["#!/usr/bin/make -f",
           "",
           -- This is marked obsolete by cdbs.  That is all.
           "DEB_CABAL_PACKAGE = " ++ debianExtraPackageName' pkgDesc,
           "",
           "include /usr/share/cdbs/1/rules/debhelper.mk",
           "include /usr/share/cdbs/1/class/hlibrary.mk",
          "",
           "# How to install an extra file into the documentation package",
           "#binary-fixup/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "::",
           "#\techo \"Some informative text\" > debian/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "/usr/share/doc/" ++ (show . D.prettyBinPkgName $ docDeb) ++ "/AnExtraDocFile",
          ""]
    where
      p = pkgName . package $ pkgDesc
      v = pkgVersion . package $ pkgDesc
      libDir = unPackageName p ++ "-" ++ showVersion v
      docDeb = debianDocPackageName versionSplits (pkgName (package pkgDesc)) (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))
      utilsDeb = debianUtilsPackageName versionSplits (pkgName (package pkgDesc)) (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))
      --exeDeb e = debianExtraPackageName (PackageName (exeName e)) Nothing

-- | Functions that apply the mapping from cabal names to debian names based on version numbers.
debianSourcePackageName' :: PackageDescription -> String
debianSourcePackageName' pkgDesc =
    show . D.prettySrcPkgName $
         debianSourcePackageName versionSplits (pkgName . package $ pkgDesc)
                                     (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianUtilsPackageName' :: PackageDescription -> String
debianUtilsPackageName' pkgDesc =
    show . D.prettyBinPkgName $
         debianUtilsPackageName versionSplits (pkgName (package pkgDesc))
                                    (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianDocPackageName' :: PackageDescription -> String
debianDocPackageName' pkgDesc =
    show . D.prettyBinPkgName $
         debianDocPackageName versionSplits (pkgName (package pkgDesc))
                                  (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianDevPackageName' :: PackageDescription -> String
debianDevPackageName' pkgDesc =
    show . D.prettyBinPkgName $
         debianDevPackageName versionSplits (pkgName (package pkgDesc))
                                  (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianProfPackageName' :: PackageDescription -> String
debianProfPackageName' pkgDesc =
    show . D.prettyBinPkgName $
         debianProfPackageName versionSplits (pkgName (package pkgDesc))
                                   (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianExtraPackageName' :: PackageDescription -> String
debianExtraPackageName' pkgDesc =
    show . D.prettyBinPkgName $ 
         debianExtraPackageName versionSplits (pkgName (package pkgDesc))
                                    (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

-- | The control file consists of a Source paragraph and one or more
-- Binary paragraphs, each one representing a binary package to be
-- produced.  If the package contains a library we usually want dev,
-- prof, and doc packages.
control :: Flags -> Compiler -> String -> PackageDescription -> ([(FilePath, String)], [String])
control flags compiler debianMaintainer pkgDesc =
    -- trace ("allBuildDepends " ++ show flags ++ " -> " ++ show (allBuildDepends flags pkgDesc)) $
    ([("control", show $ pretty $ Control {unControl = ([sourceSpec] ++ librarySpecs ++ controlSpecs)})] ++ installFiles, concat rulesLines)
    where
      librarySpecs = maybe [] (const (develLibrarySpec ++ profileLibrarySpec ++ docLibrarySpec)) (library pkgDesc)
      develLibrarySpec = [librarySpec "any" Development debianDevPackageName']
      profileLibrarySpec = if debLibProf flags then [librarySpec "any" Profiling debianProfPackageName'] else []
      docLibrarySpec = if haddock flags then [docSpecsParagraph] else []

      (controlSpecs, installFiles, rulesLines) = unzip3 $ execAndUtilSpecs flags pkgDesc debianDescription

      sourceSpec =
          Paragraph
          ([Field ("Source", " " ++ debianSourcePackageName' pkgDesc),
            Field ("Priority", " " ++ "extra"),
            Field ("Section", " " ++ "haskell"),
            Field ("Maintainer", " " ++ debianMaintainer),
            Field ("Build-Depends", " " ++ showDeps' "Build-Depends:" (filterMissing (missingDependencies' flags) (debianBuildDeps ++ map anyrel (buildDeps flags)))),
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
          [Field ("Package", " " ++ debianName pkgDesc),
           Field ("Architecture", " " ++ arch),
           Field ("Depends", " " ++ showDeps' "Depends:" (filterMissing (missingDependencies' flags) (
                     (if typ == Development
                      then [anyrel "${shlibs:Depends}"] ++ map anyrel (extraDevDeps flags)
                      else []) ++
                     ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                      extraDeps (debianName pkgDesc) (binaryPackageDeps flags))))),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Provides", " " ++ "${haskell:Provides}"),
           Field ("Description", " " ++ libraryDescription typ)]

      docSpecsParagraph =
          Paragraph
          [Field ("Package", " " ++ debianDocPackageName' pkgDesc),
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
          [[D.Rel (D.BinPkgName (D.PkgName "debhelper")) (Just (D.GRE (parseDebianVersion "7.0"))) Nothing],
           [D.Rel (D.BinPkgName (D.PkgName "haskell-devscripts")) (Just (D.GRE (parseDebianVersion "0.8"))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
          (if debLibProf flags then [anyrel "ghc-prof"] else []) ++
          (concat . map (buildDependencies (epochMap flags) (execMap flags) compiler) . allBuildDepends (depMap flags) $ pkgDesc)
      debianBuildDepsIndep :: D.Relations
      debianBuildDepsIndep =
          nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies (epochMap flags) compiler) . allBuildDepends (depMap flags) $ pkgDesc)
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

execAndUtilSpecs :: Flags -> PackageDescription -> String -> [(Paragraph' String, (FilePath, String), [String])]
execAndUtilSpecs flags pkgDesc debianDescription =
    map makeExecutablePackage (executablePackages flags) ++ makeUtilsPackage
    where
      makeExecutablePackage p =
          (Paragraph
           ([Field ("Package", " " ++ p),
             Field ("Architecture", " " ++ "any"),
             Field ("Section", " " ++ "misc"),
             Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps p (binaryPackageDeps flags)))),
             Field ("Description", " " ++ maybe debianDescription (const executableDescription) (library pkgDesc))] ++
            conflicts (filterMissing (missingDependencies' flags) (extraDeps p (binaryPackageConflicts flags)))),
           (p ++ ".install", "dist-ghc" </> "build" </> p </> p ++ " usr/bin\n"),
           ["build" </> p ++ ":: build-ghc-stamp"])
      executableDescription = " " ++ "An executable built from the " ++ display (package pkgDesc) ++ " package."
      makeUtilsPackage =
          case (bundledExecutables, dataFiles pkgDesc) of
            ([], []) ->
                []
            _ ->
                [(Paragraph
                  ([Field ("Package", " " ++ debianUtilsPackageName' pkgDesc),
                    Field ("Architecture", " " ++ "any"),
                    Field ("Section", " " ++ "misc"),
                    Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps (debianUtilsPackageName' pkgDesc) (binaryPackageDeps flags)))),
                    Field ("Description", " " ++ maybe debianDescription (const utilsDescription) (library pkgDesc))] ++
                   conflicts (extraDeps (debianUtilsPackageName' pkgDesc) (binaryPackageConflicts flags))),
                  (debianUtilsPackageName' pkgDesc ++ ".install",
                   unlines (map (\ p -> "dist-ghc" </> "build" </> exeName p </> exeName p ++ " " ++ "usr/bin") bundledExecutables ++
                            map (\ file -> file ++ " " ++ takeDirectory ("usr/share" </> debianSourcePackageName' pkgDesc ++ "-" ++ show (prettyDebianVersion (debianVersionNumber pkgDesc)) </> file)) (dataFiles pkgDesc))),
                  ["build" </> debianUtilsPackageName' pkgDesc ++ ":: build-ghc-stamp"])]
      utilsDescription = " " ++ "Utility files associated with the " ++ display (package pkgDesc) ++ " package."
      bundledExecutables = filter (\ p -> not (elem (exeName p) (executablePackages flags))) (executables pkgDesc)

conflicts :: [[D.Relation]] -> [Field' String]
conflicts [] = []
conflicts [[]] = [] -- I don't think this happens
conflicts rels = [Field ("Conflicts", " " ++ showDeps rels)]

extraDeps :: String -> [(String, String)] -> [[D.Relation]]
extraDeps p deps =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map (mkDep . snd) pairs
    where mkDep name = [D.Rel (D.BinPkgName (D.PkgName name)) Nothing Nothing]


anyrel :: String -> [D.Relation]
anyrel x = [D.Rel (D.BinPkgName (D.PkgName x)) Nothing Nothing]

-- | This is the only file I am confident I can merge with an existing
-- file, since the entries are dated and marked with well understood
-- version numbers.
updateChangelog :: Flags -> String -> PackageDescription -> String -> String -> ([String], String)
updateChangelog flags debianMaintainer pkgDesc date old =
    (concat errs, newLog)
    where
      newLog = concatMap (render . prettyEntry) $ entries
      entries = newEntry : dropWhile (\ entry -> logVersion entry >= logVersion newEntry) oldEntries
      (errs, oldEntries) = partitionEithers $ parseLog old
      newEntry = changelog flags debianMaintainer pkgDesc date

changelog :: Flags -> String -> PackageDescription -> String -> ChangeLogEntry
changelog flags debianMaintainer pkgDesc date =
    Entry { logPackage = debianSourcePackageName' pkgDesc
          , logVersion = updateOriginal f $ debianVersionNumber pkgDesc
          , logDists = [parseReleaseName "unstable"]
          , logUrgency = "low"
          , logComments = "  * Debianization generated by cabal-debian\n\n"
          , logWho = debianMaintainer
          , logDate = date }
    where
      f s = maybe (g s) (\ d -> if older d s then error ("Version from --deb-version (" ++ show d ++ ") is older than hackage version (" ++ show s ++ "), maybe you need to unpin this package?") else d) (debVersion flags)
      g s = maybe "" (\ n -> show n ++ ":") (Map.lookup (pkgName (package pkgDesc)) (epochMap flags)) ++ s ++ revision flags
      older debv cabv = parseDebianVersion debv < parseDebianVersion cabv

updateOriginal :: (String -> String) -> DebianVersion -> DebianVersion
updateOriginal f v = parseDebianVersion . f . show . prettyDebianVersion $ v

debianVersionNumber :: PackageDescription -> DebianVersion
debianVersionNumber pkgDesc = parseDebianVersion . showVersion . pkgVersion . package $ pkgDesc

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
