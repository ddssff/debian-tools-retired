{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Distribution.Debian.Debianize
    ( debianize
    , autobuilderDebianize
    , withEnvironmentArgs
    , withEnvironmentFlags
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (SomeException, catch, try)
import Control.Monad (mplus, when)
import Data.Either (partitionEithers)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
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
import Distribution.Debian.Options (compileArgs)
import Distribution.Debian.PackageDescription (withSimplePackageDescription)
import Distribution.Debian.Relations (buildDependencies, docDependencies, allBuildDepends, versionSplits)
import Distribution.Debian.Server (Executable(..))
import Distribution.Debian.Utility
import Distribution.Text (display)
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.License (License(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(buildable), Executable(exeName, buildInfo))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.Class (pretty)

data Debianization
    = Debianization
      { controlFile :: Control' String
      , changeLog :: [ChangeLogEntry]
      , otherFiles :: Map.Map FilePath String }

deriving instance Ord (Field' String)

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

-- | This function can be called from the postConf of a package's
-- Setup.hs file.  It parses the CABALDEBIAN environment variable and
-- applies the arguments it finds to the flags argument to produce a
-- new flags value.  This is then used to do the debianization.
autobuilderDebianize :: LocalBuildInfo -> Flags -> IO ()
autobuilderDebianize lbi flags =
  withEnvironmentFlags flags $ \ flags' ->
  case buildDir lbi of
    -- The autobuilder calls setup with --builddir=debian, so
    -- this case actually does the debianization.
    "debian/build" -> debianize flags'
    -- During dpkg-buildpackage Setup is run by haskell-devscripts,
    -- but we don't want to change things at that time or the build
    -- will fail.  So this just makes sure things are already properly
    -- debianized.
    "dist-ghc/build" -> debianize (flags' {validate = True})
    -- This is what gets called when you run Setup configure by hand.
    -- It just prints the changes debianization would make.
    _ -> debianize (flags' {dryRun = True})

-- | Generate a debianization for the cabal package in the current
-- directory using information from the .cabal file and from the
-- options in Flags.  This ignores any existing debianization except
-- for the debian/changelog file.  A new entry changelog is generated,
-- and any entries already there that look older than the new one are
-- preserved.
debianize :: Flags -> IO ()
debianize flags =
    withSimplePackageDescription flags $ \ pkgDesc compiler -> do
      old <- try readDebianization >>= return . either (\ (_ :: SomeException) -> Nothing) Just
      new <- debianization flags pkgDesc compiler old
      -- It is imperitive that during the time that dpkg-buildpackage
      -- runs the version number in the changelog and the source and
      -- package names in the control file do not change, or the bulid
      -- will fail.  To ensure this set the validate flag.  This means
      -- you probably need to run debianize before starting
      -- dpkg-buildpackage.  However, it is still good to be able to
      -- put the debianize parameters in the Setup file, rather than
      -- storing them apart from the package in the autobuilder
      -- configuration.
      when (validate flags)
          ( do {- versionsMatch <- catch (let oldVersion = logVersion (head (changeLog (fromJust old)))
                                           newVersion = logVersion (head (changeLog new)) in
                                       hPutStrLn stderr ("oldVersion: " ++ show (pretty oldVersion) ++ ", newVersion: " ++ show (pretty newVersion)) >>
                                       return (oldVersion == newVersion))
                                      (\ (_ :: SomeException) -> return False) -}
               sourcesMatch <- catch (let oldSource = fromJust (lookupP "Source" (head (unControl (controlFile (fromJust old)))))
                                          newSource = fromJust (lookupP "Source" (head (unControl (controlFile new)))) in
                                      hPutStrLn stderr ("oldSource: " ++ show (pretty oldSource) ++ "\nnewSource: " ++ show (pretty newSource)) >>
                                      return (oldSource == newSource))
                                     (\ (_ :: SomeException) -> return False)
               packagesMatch <- catch (let oldPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile (fromJust old)))))
                                           newPackages = catMaybes (map (lookupP "Package") (tail (unControl (controlFile new)))) in
                                       hPutStrLn stderr ("oldPackages: " ++ show (pretty oldPackages) ++ "\nnewPackages: " ++ show (pretty newPackages)) >>
                                       return (Set.fromList oldPackages == Set.fromList newPackages))
                                      (\ (_ :: SomeException) -> return False)
               when (not ({- versionsMatch && -} sourcesMatch && packagesMatch)) (describeDebianization new >>= \ text -> error $ "Debianization mismatch:\n" ++ text))
      if dryRun flags then putStrLn "Debianization (dry run):" >> describeDebianization new >>= putStr else writeDebianization new

readDebianization :: IO Debianization
readDebianization = do
  (_errs, oldLog) <- readFile "debian/changelog" >>= return . partitionEithers . parseLog
  oldControl <- parseControlFromFile "debian/control" >>= either (error . show) return
  return $ Debianization {controlFile = oldControl, changeLog = oldLog, otherFiles = Map.empty}

debianization :: Flags		 -- ^ command line flags
              -> PackageDescription  -- ^ info from the .cabal file
              -> Compiler            -- ^ compiler details
              -> Maybe Debianization
              -> IO Debianization
debianization flags pkgDesc compiler oldDeb =
    do date <- getCurrentLocalRFC822Time
       copyright <- readFile' (licenseFile pkgDesc) `catch` (\ (_ :: SomeException) -> return . showLicense . license $ pkgDesc)
       maint <- getMaintainer flags pkgDesc >>= maybe (error "Missing value for --maintainer") return
       let newLog = updateChangelog flags maint pkgDesc date (maybe [] changeLog oldDeb)
       let (controlFile, installFiles, rulesLines) = control flags compiler maint pkgDesc
       return $ Debianization
                  { controlFile = controlFile
                  , changeLog = newLog
                  , otherFiles =
                      Map.fromList
                          ([("debian/rules", cdbsRules pkgDesc ++ unlines rulesLines),
                            ("debian/compat", "7\n"), -- should this be hardcoded, or automatically read from /var/lib/dpkg/status?
                            ("debian/copyright", copyright),
                            ("debian/source/format", sourceFormat flags ++ "\n"),
                            ("debian/watch", watch pkgname)] ++
                           installFiles) }
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
      newEntry = Entry { logPackage = show (D.prettySrcPkgName (debianSourcePackageName' pkgDesc))
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

describeDebianization :: Debianization -> IO String
describeDebianization d =
    concat <$> mapM (uncurry doFile) (debianizationFiles d)
    where
      doFile path text =
          doesFileExist path >>= \ exists ->
              if exists
              then diffFile path text >>= return . maybe (path ++ ": Unchanged\n") (\ diff -> path ++ ": Modified\n" ++ indent " | " diff)
              else return $ path ++ ": Created\n" ++ indent " | " text

indent :: [Char] -> String -> String
indent prefix text = unlines (map (prefix ++) (lines text))

writeDebianization :: Debianization -> IO ()
writeDebianization d =
    mapM_ (uncurry doFile) (debianizationFiles d) >>
    getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
    where
      doFile path text =
          createDirectoryIfMissing True (takeDirectory path) >>
          replaceFile path text

debianizationFiles :: Debianization -> [(FilePath, String)]
debianizationFiles d =
    ("debian/changelog", concat (map (show . prettyEntry) (changeLog d))) :
    ("debian/control", show (pretty (controlFile d))) :
    Map.toList (otherFiles d)

watch :: String -> String
watch pkgname =
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
cdbsRules :: PackageDescription -> String
cdbsRules pkgDesc =
    unlines $
          ["#!/usr/bin/make -f",
           "",
           -- This is marked obsolete by cdbs.  That is all.
           "DEB_CABAL_PACKAGE = " ++ show (D.prettyBinPkgName (debianExtraPackageName' pkgDesc)),
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
control :: Flags -> Compiler -> String -> PackageDescription -> (Control' String, [(FilePath, String)], [String])
control flags compiler maint pkgDesc =
    -- trace ("allBuildDepends " ++ show flags ++ " -> " ++ show (allBuildDepends flags pkgDesc)) $
    (Control {unControl = ([sourceSpec] ++ librarySpecs ++ controlSpecs)}, installFiles, concat rulesLines)
    where
      librarySpecs = maybe [] (const (develLibrarySpec ++ profileLibrarySpec ++ docLibrarySpec)) (library pkgDesc)
      develLibrarySpec = [librarySpec "any" Development debianDevPackageName']
      profileLibrarySpec = if debLibProf flags then [librarySpec "any" Profiling debianProfPackageName'] else []
      docLibrarySpec = if haddock flags then [docSpecsParagraph] else []

      (controlSpecs, installFiles, rulesLines) = unzip3 $ execAndUtilSpecs flags pkgDesc debianDescription

      sourceSpec =
          Paragraph
          ([Field ("Source", " " ++ show (D.prettySrcPkgName (debianSourcePackageName' pkgDesc))),
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities
            Field ("Priority", " " ++ "optional"),
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-subsections
            Field ("Section", " " ++ "haskell"),
            Field ("Maintainer", " " ++ maint),
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
          [Field ("Package", " " ++ show (D.prettyBinPkgName (debianName pkgDesc))),
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
          [Field ("Package", " " ++ show (D.prettyBinPkgName (debianDocPackageName' pkgDesc))),
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

execAndUtilSpecs :: Flags -> PackageDescription -> String -> [(Paragraph' String, (FilePath, String), [String])]
execAndUtilSpecs flags pkgDesc debianDescription =
    map makeExecutablePackage (executablePackages flags) ++ makeUtilsPackage
    where
      makeExecutablePackage p =
          (Paragraph
           ([Field ("Package", " " ++ execName p),
             Field ("Architecture", " " ++ "any"),
             Field ("Section", " " ++ "misc"),
             Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps (D.BinPkgName (D.PkgName (execName p))) (binaryPackageDeps flags)))),
             Field ("Description", " " ++ maybe debianDescription (const executableDescription) (library pkgDesc))] ++
            conflicts (filterMissing (missingDependencies' flags) (extraDeps (D.BinPkgName (D.PkgName (execName p))) (binaryPackageConflicts flags)))),
           ("debian" </> execName p ++ ".install", "dist-ghc" </> "build" </> execName p </> execName p ++ " usr/bin\n"),
           ["build" </> execName p ++ ":: build-ghc-stamp"])
      executableDescription = " " ++ "An executable built from the " ++ display (pkgName (package pkgDesc)) ++ " package."
      makeUtilsPackage =
          case (bundledExecutables, dataFiles pkgDesc) of
            ([], []) ->
                []
            _ ->
                let p = debianUtilsPackageName' pkgDesc
                    p' = show (D.prettyBinPkgName p)
                    s = debianSourcePackageName' pkgDesc
                    s' = show (D.prettySrcPkgName s) in
                [(Paragraph
                  ([Field ("Package", " " ++ p'),
                    Field ("Architecture", " " ++ "any"),
                    Field ("Section", " " ++ "misc"),
                    Field ("Depends", " " ++ showDeps (filterMissing (missingDependencies' flags) ([anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps p (binaryPackageDeps flags)))),
                    Field ("Description", " " ++ maybe debianDescription (const utilsDescription) (library pkgDesc))] ++
                   conflicts (extraDeps p (binaryPackageConflicts flags))),
                  ("debian" </> p' ++ ".install",
                   unlines (map (\ p -> "dist-ghc" </> "build" </> exeName p </> exeName p ++ " " ++ "usr/bin") bundledExecutables ++
                            map (\ file -> file ++ " " ++ takeDirectory ("usr/share" </> s' ++ "-" ++ show (prettyDebianVersion (debianVersionNumber pkgDesc)) </> file)) (dataFiles pkgDesc))),
                  ["build" </> p' ++ ":: build-ghc-stamp"])]
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
anyrel x = [D.Rel (D.BinPkgName (D.PkgName x)) Nothing Nothing]

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
