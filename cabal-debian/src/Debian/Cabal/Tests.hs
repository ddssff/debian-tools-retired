{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Cabal.Tests
    ( tests
    ) where

import qualified CabalDebian.Flags as Flags (Flags(..), defaultFlags)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map
import Data.Monoid (mempty, mconcat, (<>))
import Data.Set as Set (fromList, singleton)
import qualified Data.Text as T
import Debian.Cabal.Debianize (debianizationWithIO)
import Debian.Cabal.PackageDescription (withSimplePackageDescription, dataDirectory)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.Combinators (tightDependencyFixup, buildDeps, setChangelog, control,
                                     setArchitecture, installExec, installServer, installWebsite)
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Input (inputDebianization, inputChangeLog)
import Debian.Debianize.Output (describeDebianization)
import Debian.Debianize.Paths (databaseDirectory)
import Debian.Debianize.Types.Atoms (compilerVersion, DebAtomKey(..), DebAtom(..), insertAtom, mapAtoms,
                                     dependencyHints, missingDependency, setRevision, putExecMap, putExtraDevDep, putBinaryPackageDep,
                                     doExecutable, doWebsite, doServer, buildDir, cabalFlagAssignments)
import Debian.Debianize.Types.Debianization (Debianization(..), newDebianization, SourceDebDescription(..), BinaryDebDescription(..),
                                             PackageRelations(..), VersionControlSpec(..))
import Debian.Debianize.Types.PackageHints (PackageHint(..), InstallFile(..), Server(..), Site(..))
import Debian.Policy (StandardsVersion(StandardsVersion), getDebhelperCompatLevel, getDebianStandardsVersion,
                      PackagePriority(Extra), PackageArchitectures(All, Any), SourceFormat(Native3), Section(..))
import Debian.Relation (Relation(..), VersionReq(..), SrcPkgName(..), BinPkgName(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (buildDebianVersion, parseDebianVersion)
import Distribution.License (License(BSD3))
import System.FilePath ((</>))
import Test.HUnit
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [test1, test2a, test2b, test3, test4, test5, test6, test7])

test1 :: Test
test1 =
    TestLabel "test1" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization testEntry (Left BSD3) level standards
                 assertEqual "test1" [] (diffDebianizations testDeb1 deb))

test2a :: Test
test2a =
    TestLabel "test2a" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization testEntry (Left BSD3) level standards
                 assertEqual "test2a" [] (diffDebianizations expect deb))
    where
      expect =
          Debianization
          { sourceDebDescription =
                SourceDebDescription
                { source = SrcPkgName {unSrcPkgName = "haskell-cabal-debian"},
                  maintainer = NameAddr {nameAddr_name = Just "David Fox", nameAddr_addr = "dsf@seereason.com"},
                  changedBy = Nothing,
                  uploaders = [],
                  dmUploadAllowed = False,
                  priority = Nothing,
                  section = Nothing,
                  buildDepends = [],
                  buildConflicts = [],
                  buildDependsIndep = [],
                  buildConflictsIndep = [],
                  standardsVersion = StandardsVersion 3 9 3 (Just 1),
                  homepage = Nothing,
                  vcsFields = Set.fromList [],
                  xFields = Set.fromList [],
                  binaryPackages = [] },
            changelog =
                ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                                  logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                                  logDists = [ReleaseName {relName = "unstable"}],
                                  logUrgency = "low",
                                  logComments = unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                         "    files that were supposed to be installed into packages."],
                                  logWho = "David Fox <dsf@seereason.com>",
                                  logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}],
            rulesHead =
                T.unlines ["#!/usr/bin/make -f",
                           "",
                           "DEB_CABAL_PACKAGE = haskell-cabal-debian",
                           "",
                           "include /usr/share/cdbs/1/rules/debhelper.mk",
                           "include /usr/share/cdbs/1/class/hlibrary.mk",
                           ""],
            compat = 9,
            copyright = Left BSD3,
            debAtoms = mempty}

test2b :: Test
test2b =
    TestLabel "test2b" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization testEntry (Left BSD3) level standards
                 assertEqual "test2b"
                  (Map.fromList 
                   [("debian/changelog", (T.unlines
                                          ["haskell-cabal-debian (2.6.2) unstable; urgency=low",
                                           "",
                                           "  * Fix a bug constructing the destination pathnames that was dropping",
                                           "    files that were supposed to be installed into packages.",
                                           "",
                                           " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800"])),
                    ("debian/compat","9\n"),
                    ("debian/control","Source: haskell-cabal-debian\nMaintainer: David Fox <dsf@seereason.com>\nStandards-Version: 3.9.3.1\n"),
                    ("debian/copyright","BSD3\n"),
                    ("debian/rules", "#!/usr/bin/make -f\n\nDEB_CABAL_PACKAGE = haskell-cabal-debian\n\ninclude /usr/share/cdbs/1/rules/debhelper.mk\ninclude /usr/share/cdbs/1/class/hlibrary.mk\n\n")])
                  (toFileMap "dist-ghc/build" "<datadir>" deb)
             )

test3 :: Test
test3 =
    TestLabel "test3" $
    TestCase (do deb <- inputDebianization "test-data/haskell-devscripts"
                 assertEqual "test3" [] (diffDebianizations testDeb2 deb))

test4 :: Test
test4 =
    TestLabel "test4" $
    TestCase (do oldlog <- inputChangeLog "test-data/clckwrks-dot-com/input/debian"
                 old <- inputDebianization "test-data/clckwrks-dot-com/output" >>= \ x -> return (x {changelog = oldlog})
                 (new, dataDir) <- debianizationWithIO "test-data/clckwrks-dot-com/input" (Flags.verbosity flags) (compilerVersion flags) (cabalFlagAssignments flags) (Flags.debAtoms flags) old
                 let new' = copyFirstLogEntry old (fixRules (tight new))
                 desc <- describeDebianization (buildDir "dist-ghc/build" flags) "test-data/clckwrks-dot-com/output" dataDir new'
                 -- assertEqual "test4" "" desc
                 -- assertEqual "test4" [] (gdiff old (finalizeDebianization "dist-ghc/build" dataDir new'))
                 -- assertEqual "test4" (toFileMap "dist-ghc/build" "<datadir>" old) (toFileMap "dist-ghc/build" "<datadir>" new')
                 assertEqual "test4" [] (diffDebianizations old new')
             )
    where
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      dropRulesAtoms deb =
          mapAtoms omitRulesAtom deb
          where
            omitRulesAtom Source (DebRulesFragment _) = mempty
            omitRulesAtom _ x = Set.singleton x
      flags = insertAtom Source (DebSourceFormat Native3) $
              missingDependency (BinPkgName "libghc-clckwrks-theme-clckwrks-doc") $
              setRevision "" $
              doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production")) $
              doExecutable (BinPkgName "clckwrks-dot-com-backups") backups $
{-            doServer (BinPkgName "clckwrks-dot-com-staging") (theServer (BinPkgName "clckwrks-dot-com-staging")) $
              doServer (BinPkgName "clckwrks-dot-com-development") (theServer (BinPkgName "clckwrks-dot-com-development")) $ -}
              Flags.defaultFlags
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules deb = deb {rulesHead = T.unlines $ concat $ map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                                          then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                                                          else [line] :: [T.Text]) (T.lines (rulesHead deb))}

      tight deb = foldr (tightDependencyFixup
                         -- For each pair (A, B) make sure that this package requires the
                         -- same exact version of package B as the version of A currently
                         -- installed during the build.
                         [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-bugs-dev", BinPkgName "haskell-clckwrks-plugin-bugs-utils"),
                          (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) deb serverNames

      theSite :: BinPkgName -> Site
      theSite deb =
          Site { domain = hostname'
               , serverAdmin = "logic@seereason.com"
               , server = theServer deb }
      theServer :: BinPkgName -> Server
      theServer deb =
          Server { hostname =
                       case deb of
                         BinPkgName "clckwrks-dot-com-production" -> hostname'
                         _ -> hostname'
                 , port = portNum deb
                 , headerMessage = "Generated by clckwrks-dot-com/Setup.hs"
                 , retry = "60"
                 , serverFlags =
                     [ "--http-port", show (portNum deb)
                     , "--hide-port"
                     , "--hostname", hostname'
                     , "--top", databaseDirectory deb
                     , "--enable-analytics"
                     , "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path
                     ]
                 , installFile =
                     InstallFile { execName   = "clckwrks-dot-com-server"
                                 , destName   = show (pretty deb)
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }
      hostname' = "clckwrks.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "clckwrks-dot-com-production"  -> 9029
            "clckwrks-dot-com-staging"     -> 9038
            "clckwrks-dot-com-development" -> 9039
            _ -> error $ "Unexpected package name: " ++ deb
      jstreePath = "/usr/share/clckwrks-0.13.2/jstree"
      json2Path = "/usr/share/clckwrks-0.13.2/json2"
      backups = InstallFile { execName   = "clckwrks-dot-com-backups"
                            , destName   = "clckwrks-dot-com-backups"
                            , sourceDir  = Nothing
                            , destDir    = Just "/etc/cron.hourly" }

test5 :: Test
test5 =
    TestLabel "test5" $
    TestCase (     do oldlog <- inputChangeLog "test-data/creativeprompts/input/debian"
                      old <- inputDebianization "test-data/creativeprompts/output" >>= \ x -> return (x {changelog = oldlog})
                      (new, dataDir) <- debianizationWithIO "test-data/creativeprompts/input" (Flags.verbosity flags) (compilerVersion flags) (cabalFlagAssignments flags) (Flags.debAtoms flags) old
                      let new' = setArchitecture (BinPkgName "creativeprompts-development") All $
                                 setArchitecture (BinPkgName "creativeprompts-production") All $
                                 insertAtom Source (UtilsPackageName (BinPkgName "creativeprompts-data")) $
                                 copyFirstLogEntry old $
                                 putBinaryPackageDep (BinPkgName "creativeprompts-backups") (BinPkgName "anacron") $
                                 putBinaryPackageDep (BinPkgName "creativeprompts-server") (BinPkgName "markdown") $
                                 new
                      desc <- describeDebianization (buildDir "dist-ghc/build" flags) "test-data/creativeprompts/output" dataDir new'
                      writeFile "/tmp/foo" desc
                      -- assertEqual "Convert creativeprompts" [] (gdiff (dropFirstLogEntry old) (addMarkdownDependency (dropFirstLogEntry (finalizeDebianization "dist-ghc/build" dataDir new))))
                      -- assertEqual "test5" "" desc
                      -- assertEqual "test5" (toFileMap "dist-ghc/build" "<datadir>" old) (toFileMap "dist-ghc/build" "<datadir>" new')
                      let -- Put the old values in fst, the new values in snd
                          new'' :: Map.Map FilePath T.Text
                          new'' = (toFileMap "dist-ghc/build" "<datadir>" new')
                          old' :: Map.Map FilePath (T.Text, T.Text)
                          old' = Map.map (\ x -> (x, mempty)) (toFileMap "dist-ghc/build" "<datadir>" old)
                      assertEqual "test5" [] (diffDebianizations old new')
             )
    where
      flags = putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $
              doExecutable (BinPkgName "creativeprompts-backups") (InstallFile "creativeprompts-backups" Nothing Nothing "creativeprompts-backups") $
              doExecutable (BinPkgName "creativeprompts-development") (InstallFile "creativeprompts-development" Nothing Nothing "creativeprompts-development") $
              doExecutable (BinPkgName "creativeprompts-production") (InstallFile "creativeprompts-production" Nothing Nothing "creativeprompts-production") $
              doExecutable (BinPkgName "creativeprompts-server") (InstallFile "creativeprompts-server" Nothing Nothing "creativeprompts-server") $
              Flags.defaultFlags
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      addMarkdownDependency :: Debianization -> Debianization
      addMarkdownDependency deb = updateBinaryPackage (BinPkgName "creativeprompts-server") addMarkdownDependency' deb
      addMarkdownDependency' :: BinaryDebDescription -> BinaryDebDescription
      addMarkdownDependency' deb = deb {relations = (relations deb) {depends = [[Rel (BinPkgName "markdown") Nothing Nothing]] ++ depends (relations deb)}}


dropFirstLogEntry :: Debianization -> Debianization
dropFirstLogEntry (deb@(Debianization {changelog = ChangeLog (_ : tl)})) = deb {changelog = ChangeLog tl}

copyFirstLogEntry :: Debianization -> Debianization -> Debianization
copyFirstLogEntry (Debianization {changelog = ChangeLog (hd1 : _)}) (deb2@(Debianization {changelog = ChangeLog (_ : tl2)})) = deb2 {changelog = ChangeLog (hd1 : tl2)}

test6 :: Test
test6 =
    TestLabel "test6" $
    TestCase ( do old@(Debianization {changelog = oldLog@(ChangeLog (entry : _))}) <- inputDebianization "test-data/creativeprompts/output"
                  withSimplePackageDescription "test-data/creativeprompts/input" 0 Nothing [] $ \ pkgDesc cmplr ->
                      do -- compat <- getDebhelperCompatLevel
                         let compat' = 7
                         -- standards <- getDebianStandardsVersion
                         let standards = StandardsVersion 3 8 1 Nothing
                         let new = control $
                                   insertAtom Source (UtilsPackageName (BinPkgName "creativeprompts-data")) $
                                   -- setSourcePackageName (SrcPkgName "haskell-creativeprompts") $
                                   -- setChangelog oldLog $
                                   buildDeps $
                                   insertAtom Source (DHPackageDescription pkgDesc) $
                                   insertAtom Source (DHCompiler cmplr) $
                                   putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $
                                   putBinaryPackageDep (BinPkgName "creativeprompts-backups") (BinPkgName "anacron") $
                                   putBinaryPackageDep (BinPkgName "creativeprompts-server") (BinPkgName "markdown") $
                                   setArchitecture (BinPkgName "creativeprompts-server") Any $
                                   setArchitecture (BinPkgName "creativeprompts-development") All $
                                   setArchitecture (BinPkgName "creativeprompts-production") All $
                                   setArchitecture (BinPkgName "creativeprompts-data") All $
                                   setArchitecture (BinPkgName "creativeprompts-backups") Any $
                                   doExecutable (BinPkgName "creativeprompts-server") (InstallFile "creativeprompts-server" Nothing Nothing "creativeprompts-server") $
                                   doExecutable (BinPkgName "creativeprompts-development") (InstallFile "creativeprompts-development" Nothing Nothing "creativeprompts-development") $
                                   doExecutable (BinPkgName "creativeprompts-production") (InstallFile "creativeprompts-production" Nothing Nothing "creativeprompts-production") $
                                   doExecutable (BinPkgName "creativeprompts-backups") (InstallFile "creativeprompts-backups" Nothing Nothing "creativeprompts-backups") $
                                   newDebianization entry (Left BSD3) compat' standards
                         desc <- describeDebianization "dist-ghc/build" "test-data/creativeprompts/output" (dataDirectory pkgDesc) new
                         writeFile "/tmp/bar" desc
                         assertEqual "test6" [] (diffDebianizations old new)
             )
    where
      -- hints = dependencyHints (putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $ putExtraDevDep (BinPkgName "markdown") $ Flags.defaultFlags)
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      -- dropFirstLogEntry (deb@(Debianization {changelog = ChangeLog (_ : tl)})) = deb {changelog = ChangeLog tl}
      addMarkdownDependency :: Debianization -> Debianization
      addMarkdownDependency deb = updateBinaryPackage (BinPkgName "creativeprompts-server") addMarkdownDependency' deb
      addMarkdownDependency' :: BinaryDebDescription -> BinaryDebDescription
      addMarkdownDependency' deb = deb {relations = (relations deb) {depends = [[Rel (BinPkgName "markdown") Nothing Nothing]] ++ depends (relations deb)}}

test7 :: Test
test7 =
    TestLabel "test7" $
    TestCase ( do old <- inputDebianization "."
                  (new, dataDir) <- debianizationWithIO "test-data/cabal-debian/input" (Flags.verbosity flags) (compilerVersion flags) (cabalFlagAssignments flags) (Flags.debAtoms flags) old
                  assertEqual "test7" [] (diffDebianizations old new)
             )
    where
      flags = Flags.defaultFlags

data Change k a
    = Created k a
    | Deleted k a
    | Modified k a a
    | Unchanged k a
    deriving (Eq, Show)

diffMaps :: (Ord k, Eq a, Show k, Show a) => Map.Map k a -> Map.Map k a -> [Change k a]
diffMaps old new =
    Map.elems (intersectionWithKey combine1 old new) ++
    map (uncurry Deleted) (Map.toList (differenceWithKey combine2 old new)) ++
    map (uncurry Created) (Map.toList (differenceWithKey combine2 new old))
    where
      combine1 k a b = if a == b then Unchanged k a else Modified k a b
      combine2 _ _ _ = Nothing

diffDebianizations :: Debianization -> Debianization -> String -- [Change FilePath T.Text]
diffDebianizations old new =
    show (mconcat (map prettyChange (filter (not . isUnchanged) (diffMaps old' new'))))
    where
      old' = toFileMap "dist-ghc/build" "<datadir>" old
      new' = toFileMap "dist-ghc/build" "<datadir>" new
      isUnchanged (Unchanged _ _) = True
      isUnchanged _ = False
      prettyChange (Unchanged path _) = text ("Unchanged: " <> path <> "\n")
      prettyChange (Deleted path _) = text ("Deleted: " <> path <> "\n")
      prettyChange (Created path _) = text ("Created: " <> path <> "\n")
      prettyChange (Modified path a b) =
          text ("Modified: " <> path <> "\n") <>
          prettyDiff ("old" </> path) ("new" </> path)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 (T.split (== '\n') a) (T.split (== '\n') b))

updateBinaryPackage :: BinPkgName -> (BinaryDebDescription -> BinaryDebDescription) -> Debianization -> Debianization
updateBinaryPackage name f deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = map update (binaryPackages (sourceDebDescription deb))}}
    where update p | package p == name = f p
          update p = p

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (unlines [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))

testDeb1 :: Debianization
testDeb1 =
    Debianization
        { sourceDebDescription =
            SourceDebDescription
                { source = SrcPkgName {unSrcPkgName = "haskell-cabal-debian"}
                , maintainer = NameAddr (Just "David Fox") "dsf@seereason.com"
                , changedBy = Nothing
                , uploaders = []
                , dmUploadAllowed = False
                , priority = Nothing
                , section = Nothing
                , buildDepends = []
                , buildConflicts = []
                , buildDependsIndep = []
                , buildConflictsIndep = []
                , standardsVersion = StandardsVersion 3 9 3 (Just 1) -- This will change as new versions of debian-policy are released
                , homepage = Nothing
                , vcsFields = Set.fromList []
                , xFields = Set.fromList []
                , binaryPackages = [] }
        , changelog =
            ChangeLog [Entry { logPackage = "haskell-cabal-debian"
                             , logVersion = buildDebianVersion Nothing "2.6.2" Nothing
                             , logDists = [ReleaseName {relName = "unstable"}]
                             , logUrgency = "low"
                             , logComments = "  * Fix a bug constructing the destination pathnames that was dropping\n    files that were supposed to be installed into packages.\n"
                             , logWho = "David Fox <dsf@seereason.com>"
                             , logDate = "Thu, 20 Dec 2012 06:49:25 -0800" }]
        , rulesHead = T.pack . unlines $
                              [ "#!/usr/bin/make -f"
                              , ""
                              , "DEB_CABAL_PACKAGE = haskell-cabal-debian"
                              , ""
                              , "include /usr/share/cdbs/1/rules/debhelper.mk"
                              , "include /usr/share/cdbs/1/class/hlibrary.mk"
                              , "" ]
        , compat = 9 -- This will change as new version of debhelper are released
        , copyright = Left BSD3
        , debAtoms = mempty }

testDeb2 :: Debianization
testDeb2 =
    insertAtom Source (DebSourceFormat Native3) $
    Debianization
    { sourceDebDescription =
          SourceDebDescription
          { source = SrcPkgName {unSrcPkgName = "haskell-devscripts"}
          , maintainer = NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"}
          , changedBy = Nothing
          , uploaders = [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
          , dmUploadAllowed = False
          , priority = Just Extra
          , section = Just (MainSection "haskell")
          , buildDepends = [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]]
          , buildConflicts = []
          , buildDependsIndep = [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
          , buildConflictsIndep = []
          , standardsVersion = StandardsVersion 3 9 4 Nothing
          , homepage = Nothing
          , vcsFields = Set.fromList [ VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                     , VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"]
          , xFields = Set.fromList []
          , binaryPackages = [BinaryDebDescription { package = BinPkgName {unBinPkgName = "haskell-devscripts"}
                                                   , architecture = All
                                                   , binarySection = Nothing
                                                   , binaryPriority = Nothing
                                                   , essential = False
                                                   , description = (T.intercalate "\n"
                                                                    ["Tools to help Debian developers build Haskell packages",
                                                                     " This package provides a collection of scripts to help build Haskell",
                                                                     " packages for Debian.  Unlike haskell-utils, this package is not",
                                                                     " expected to be installed on the machines of end users.",
                                                                     " .",
                                                                     " This package is designed to support Cabalized Haskell libraries.  It",
                                                                     " is designed to build a library for each supported Debian compiler or",
                                                                     " interpreter, generate appropriate postinst/prerm files for each one,",
                                                                     " generate appropriate substvars entries for each one, and install the",
                                                                     " package in the Debian temporary area as part of the build process."])
                                                   , relations =
                                                       PackageRelations
                                                       { depends = [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "debhelper"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "dh-buildinfo"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "ghc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.6" :: String)))) Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "html-xml-utils"}) Nothing Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "hscolour"}) (Just (GRE (Debian.Version.parseDebianVersion ("1.8" :: String)))) Nothing]
                                                                   , [Rel (BinPkgName {unBinPkgName = "ghc-haddock"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.4" :: String)))) Nothing] ]
                                                       , recommends = []
                                                       , suggests = []
                                                       , preDepends = []
                                                       , breaks = []
                                                       , conflicts = []
                                                       , provides = []
                                                       , replaces = []
                                                       , builtUsing = [] }}]}
    , changelog =
        ChangeLog [Entry { logPackage = "haskell-devscripts"
                         , logVersion = Debian.Version.parseDebianVersion ("0.8.13" :: String)
                         , logDists = [ReleaseName {relName = "experimental"}]
                         , logUrgency = "low"
                         , logComments = "  [ Joachim Breitner ]\n  * Improve parsing of \"Setup register\" output, patch by David Fox\n  * Enable creation of hoogle files, thanks to Kiwamu Okabe for the\n    suggestion. \n\n  [ Kiwamu Okabe ]\n  * Need --html option to fix bug that --hoogle option don't output html file.\n  * Support to create /usr/lib/ghc-doc/hoogle/*.txt for hoogle package.\n\n  [ Joachim Breitner ]\n  * Symlink hoogle\8217s txt files to /usr/lib/ghc-doc/hoogle/\n  * Bump ghc dependency to 7.6 \n  * Bump standards version\n"
                         , logWho = "Joachim Breitner <nomeata@debian.org>"
                         , logDate = "Mon, 08 Oct 2012 21:14:50 +0200" },
                   Entry { logPackage = "haskell-devscripts"
                         , logVersion = Debian.Version.parseDebianVersion ("0.8.12" :: String)
                         , logDists = [ReleaseName {relName = "unstable"}]
                         , logUrgency = "low"
                         , logComments = "  * Depend on ghc >= 7.4, adjusting to its haddock --interface-version\n    behaviour.\n"
                         , logWho = "Joachim Breitner <nomeata@debian.org>"
                         , logDate = "Sat, 04 Feb 2012 10:50:33 +0100"}]
    , rulesHead = "#!/usr/bin/make -f\n# -*- makefile -*-\n\n# Uncomment this to turn on verbose mode.\n#export DH_VERBOSE=1\n\nDEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')\n\nmanpages = $(shell cat debian/manpages)\n\n%.1: %.pod\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n%.1: %\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n.PHONY: build\nbuild: $(manpages)\n\ninstall-stamp:\n\tdh install\n\n.PHONY: install\ninstall: install-stamp\n\nbinary-indep-stamp: install-stamp\n\tdh binary-indep\n\ttouch $@\n\n.PHONY: binary-indep\nbinary-indep: binary-indep-stamp\n\n.PHONY: binary-arch\nbinary-arch: install-stamp\n\n.PHONY: binary\nbinary: binary-indep-stamp\n\n.PHONY: clean\nclean:\n\tdh clean\n\trm -f $(manpages)\n\n\n"
    , compat = 7
    , copyright = Right "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n\n"
    , debAtoms = mempty }
