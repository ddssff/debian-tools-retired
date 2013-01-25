{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Tests
    ( tests
    ) where

import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map
import Data.Monoid (mconcat, (<>), mempty)
import Data.Set as Set (Set, fromList, singleton)
import qualified Data.Text as T
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.Debianize (cabalToDebianization, newDebianization)
import Debian.Debianize.Atoms as Atom (tightDependencyFixup, missingDependency, setRevision, putExecMap, sourceFormat,
                                       depends, conflicts, doExecutable, doWebsite, doServer, doBackups, setArchitecture, setSourcePackageName,
                                       setChangeLog, changeLog, setChangeLog', setRulesHead)
import Debian.Debianize.Files (finalizeDebianization, toFileMap)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Output (writeDebianization)
import Debian.Debianize.Types.Atoms (DebAtomKey(..), DebAtom(..), insertAtom, defaultAtoms, mapAtoms)
import Debian.Debianize.Types.Debianization as Deb (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..),
                                                    PackageRelations(..), VersionControlSpec(..))
import Debian.Debianize.Types.PackageHints (InstallFile(..), Server(..), Site(..))
import Debian.Debianize.Utility (withCurrentDirectory)
import Debian.Policy (databaseDirectory, StandardsVersion(StandardsVersion), getDebhelperCompatLevel,
                      getDebianStandardsVersion, PackagePriority(Extra), PackageArchitectures(All),
                      SourceFormat(Native3), Section(..))
import Debian.Relation (Relation(..), VersionReq(..), SrcPkgName(..), BinPkgName(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (buildDebianVersion, parseDebianVersion)
import Distribution.License (License(BSD3))
import Prelude hiding (log)
import System.FilePath ((</>))
import Test.HUnit
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [test1, test2, test3, test4, test5, test6, test7])

test1 :: Test
test1 =
    TestLabel "test1" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization (ChangeLog [testEntry]) (Left BSD3) level standards
                 assertEqual "test1" [] (diffDebianizations testDeb1 deb))
    where
      testDeb1 :: Debianization
      testDeb1 =
          setRulesHead (T.pack . unlines $ [ "#!/usr/bin/make -f"
                                           , ""
                                           , "DEB_CABAL_PACKAGE = haskell-cabal-debian"
                                           , ""
                                           , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                           , "include /usr/share/cdbs/1/class/hlibrary.mk"
                                           , "" ]) $
          setChangeLog
                 (ChangeLog [Entry { logPackage = "haskell-cabal-debian"
                                   , logVersion = buildDebianVersion Nothing "2.6.2" Nothing
                                   , logDists = [ReleaseName {relName = "unstable"}]
                                   , logUrgency = "low"
                                   , logComments = "  * Fix a bug constructing the destination pathnames that was dropping\n    files that were supposed to be installed into packages.\n"
                                   , logWho = "David Fox <dsf@seereason.com>"
                                   , logDate = "Thu, 20 Dec 2012 06:49:25 -0800" }]) $
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
              , compat = 9 -- This will change as new version of debhelper are released
              , copyright = Left BSD3
              , debAtoms = defaultAtoms }

test2 :: Test
test2 =
    TestLabel "test2" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization (ChangeLog [testEntry]) (Left BSD3) level standards
                 assertEqual "test2" [] (diffDebianizations expect deb))
    where
      expect =
          setRulesHead (T.unlines ["#!/usr/bin/make -f",
                                   "",
                                   "DEB_CABAL_PACKAGE = haskell-cabal-debian",
                                   "",
                                   "include /usr/share/cdbs/1/rules/debhelper.mk",
                                   "include /usr/share/cdbs/1/class/hlibrary.mk",
                                   ""]) $
          setChangeLog
               (ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                                  logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                                  logDists = [ReleaseName {relName = "unstable"}],
                                  logUrgency = "low",
                                  logComments = unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                         "    files that were supposed to be installed into packages."],
                                  logWho = "David Fox <dsf@seereason.com>",
                                  logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}]) $
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
            compat = 9,
            copyright = Left BSD3,
            debAtoms = defaultAtoms }

test3 :: Test
test3 =
    TestLabel "test3" $
    TestCase (do deb <- inputDebianization "test-data/haskell-devscripts"
                 assertEqual "test3" [] (diffDebianizations testDeb2 deb))
    where
      testDeb2 :: Debianization
      testDeb2 =
          sourceFormat Native3 $
          setRulesHead "#!/usr/bin/make -f\n# -*- makefile -*-\n\n# Uncomment this to turn on verbose mode.\n#export DH_VERBOSE=1\n\nDEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')\n\nmanpages = $(shell cat debian/manpages)\n\n%.1: %.pod\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n%.1: %\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n.PHONY: build\nbuild: $(manpages)\n\ninstall-stamp:\n\tdh install\n\n.PHONY: install\ninstall: install-stamp\n\nbinary-indep-stamp: install-stamp\n\tdh binary-indep\n\ttouch $@\n\n.PHONY: binary-indep\nbinary-indep: binary-indep-stamp\n\n.PHONY: binary-arch\nbinary-arch: install-stamp\n\n.PHONY: binary\nbinary: binary-indep-stamp\n\n.PHONY: clean\nclean:\n\tdh clean\n\trm -f $(manpages)\n\n\n" $
          setChangeLog
             (ChangeLog [Entry { logPackage = "haskell-devscripts"
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
                               , logDate = "Sat, 04 Feb 2012 10:50:33 +0100"}]) $
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
                , buildDependsIndep =
                    [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
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
                                                             { Deb.depends =
                                                                   [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
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
                                                             , Deb.conflicts = []
                                                             , provides = []
                                                             , replaces = []
                                                             , builtUsing = [] }}]}
          , compat = 7
          , copyright = Right "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n\n"
          , debAtoms = defaultAtoms }

test4 :: Test
test4 =
    TestLabel "test4" $
    TestCase (do old <- inputDebianization "test-data/clckwrks-dot-com/output" -- >>= \ x -> return (x {changelog = oldlog})
                 new <- cabalToDebianization "test-data/clckwrks-dot-com/input" (newDebianization (changeLog old) (Left BSD3) 7 (StandardsVersion 3 9 4 Nothing))
                 let new' =
                         finalizeDebianization $
                         (\ x -> x {sourceDebDescription = (sourceDebDescription x) {homepage = Just "http://www.clckwrks.com/"}}) $
                         sourceFormat Native3 $
                         missingDependency (BinPkgName "libghc-clckwrks-theme-clckwrks-doc") $
                         setRevision "" $
                         doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production")) $
                         doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups" $
                         {- doServer (BinPkgName "clckwrks-dot-com-staging") (theServer (BinPkgName "clckwrks-dot-com-staging")) $
                            doServer (BinPkgName "clckwrks-dot-com-development") (theServer (BinPkgName "clckwrks-dot-com-development")) $ -}
                         copyFirstLogEntry old $
                         fixRules $
                         tight $
                         new
                 -- desc <- describeDebianization (buildDir "dist-ghc/build" atoms) "test-data/clckwrks-dot-com/output" dataDir new'
                 -- assertEqual "test4" "" desc
                 -- assertEqual "test4" [] (gdiff old (finalizeDebianization "dist-ghc/build" dataDir new'))
                 -- assertEqual "test4" (toFileMap "dist-ghc/build" "<datadir>" old) (toFileMap "dist-ghc/build" "<datadir>" new')
                 assertEqual "test4" [] (diffDebianizations old new')
             )
    where
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
{-
      dropRulesAtoms deb =
          mapAtoms omitRulesAtom deb
          where
            omitRulesAtom Source (DebRulesFragment _) = mempty
            omitRulesAtom _ x = Set.singleton x
-}
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules deb =
          mapAtoms f deb
          where
            f :: DebAtomKey -> DebAtom -> Set (DebAtomKey, DebAtom)
            f Source (DebRulesHead t) =
                singleton (Source, DebRulesHead (T.unlines $ concat $
                                                 map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                                                else [line] :: [T.Text]) (T.lines t)))
            f k a = singleton (k, a)
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

anyrel :: BinPkgName -> Relation
anyrel b = Rel b Nothing Nothing

test5 :: Test
test5 =
    TestLabel "test5" $
    TestCase (     do -- oldlog <- inputChangeLog "test-data/creativeprompts/input/debian"
                      old <- inputDebianization "test-data/creativeprompts/output" -- >>= \ x -> return (x {changelog = oldlog})
                      new <- cabalToDebianization "test-data/creativeprompts/input"
                               (newDebianization (changeLog old) (copyright old) (compat old) (standardsVersion (sourceDebDescription old)))
                      let new' = finalizeDebianization $
                                 sourceFormat Native3 $
                                 -- setArchitecture (Binary (BinPkgName "creativeprompts-server")) Any $
                                 setArchitecture (Binary (BinPkgName "creativeprompts-data")) All $
                                 setArchitecture (Binary (BinPkgName "creativeprompts-development")) All $
                                 setArchitecture (Binary (BinPkgName "creativeprompts-production")) All $
                                 insertAtom Source (UtilsPackageName (BinPkgName "creativeprompts-data")) $
                                 copyFirstLogEntry old $ -- Get rid of the "debianization generated by..." message so we match
                                 Atom.depends (BinPkgName "creativeprompts-server") (anyrel (BinPkgName "markdown")) $
                                 putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $
                                 -- doExecutable (BinPkgName "creativeprompts-development") (InstallFile "creativeprompts-development" Nothing Nothing "creativeprompts-development") $
                                 doBackups (BinPkgName "creativeprompts-backups") "creativeprompts-backups" $
                                 doServer (BinPkgName "creativeprompts-development") (theServer (BinPkgName "creativeprompts-development")) $
                                 doWebsite (BinPkgName "creativeprompts-production") (theSite (BinPkgName "creativeprompts-production")) $
                                 new
                      -- desc <- describeDebianization "test-data/creativeprompts/output" new'
                      -- writeFile "/tmp/foo" desc
                      -- assertEqual "Convert creativeprompts" [] (gdiff (dropFirstLogEntry old) (addMarkdownDependency (dropFirstLogEntry (finalizeDebianization "dist-ghc/build" new))))
                      -- assertEqual "test5" "" desc
                      -- assertEqual "test5" (toFileMap "dist-ghc/build" "<datadir>" old) (toFileMap "dist-ghc/build" "<datadir>" new')
{-
                      let -- Put the old values in fst, the new values in snd
                          new'' :: Map.Map FilePath T.Text
                          new'' = (toFileMap new')
                          old' :: Map.Map FilePath (T.Text, T.Text)
                          old' = Map.map (\ x -> (x, mempty)) (toFileMap old)
-}
                      assertEqual "test5" [] (diffDebianizations old new')
             )
    where
{-
      addMarkdownDependency :: Debianization -> Debianization
      addMarkdownDependency deb = updateBinaryPackage (BinPkgName "creativeprompts-server") addMarkdownDependency' deb
      addMarkdownDependency' :: BinaryDebDescription -> BinaryDebDescription
      addMarkdownDependency' deb = deb {relations = (relations deb) {depends = [[Rel (BinPkgName "markdown") Nothing Nothing]] ++ depends (relations deb)}}
-}
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
                 , headerMessage = "Generated by creativeprompts-dot-com/debian/Debianize.hs"
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
                     InstallFile { execName   = "creativeprompts-server"
                                 , destName   = show (pretty deb)
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }
      hostname' = "creativeprompts.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "creativeprompts-production"  -> 9022
            "creativeprompts-staging"     -> 9033
            "creativeprompts-development" -> 9034
            _ -> error $ "Unexpected package name: " ++ deb
      jstreePath = "/usr/share/clckwrks-0.13.2/jstree"
      json2Path = "/usr/share/clckwrks-0.13.2/json2"


{-
dropFirstLogEntry :: Debianization -> Debianization
dropFirstLogEntry (deb@(Debianization {changelog = ChangeLog (_ : tl)})) = deb {changelog = ChangeLog tl}
-}

copyFirstLogEntry :: Debianization -> Debianization -> Debianization
copyFirstLogEntry deb1 deb2 =
    setChangeLog' (ChangeLog (hd1 : tl2)) deb2
    where
      ChangeLog (hd1 : _) = changeLog deb1
      ChangeLog (_ : tl2) = changeLog deb2

copyChangelog :: Debianization -> Debianization -> Debianization
copyChangelog deb1 deb2 = setChangeLog' (changeLog deb1) deb2

copyCopyright :: Debianization -> Debianization -> Debianization
copyCopyright (Debianization {copyright = x}) deb = deb {copyright = x}

test6 :: Test
test6 =
    TestLabel "test6" $
    TestCase ( do old <- inputDebianization "test-data/artvaluereport2/output"
                  let log = changeLog old
                      standards = StandardsVersion 3 9 1 Nothing
                      compat' = 7
                  new <- cabalToDebianization "test-data/artvaluereport2/input" (newDebianization log (Left BSD3) compat' standards)
                  let new' = finalizeDebianization $
                             (\ x -> x {sourceDebDescription = (sourceDebDescription x) {homepage = Just "http://appraisalreportonline.com"}}) $
                             setSourcePackageName (SrcPkgName "haskell-artvaluereport2") $
                             copyFirstLogEntry old $ -- Get rid of the "debianization generated by..." message so we match
                             insertAtom Source (UtilsPackageName (BinPkgName "artvaluereport2-server")) $
                             setArchitecture (Binary (BinPkgName "artvaluereport2-development")) All $
                             setArchitecture (Binary (BinPkgName "artvaluereport2-production")) All $
                             setArchitecture (Binary (BinPkgName "artvaluereport2-staging")) All $
                             insertAtom Source (BuildDepIndep (BinPkgName "libjs-jcrop")) $
                             insertAtom Source (BuildDepIndep (BinPkgName "libjs-jquery")) $
                             insertAtom Source (BuildDepIndep (BinPkgName "libjs-jquery-u")) $

                             Atom.depends (BinPkgName "artvaluereport2-development") (anyrel (BinPkgName "artvaluereport2-server")) $
                             Atom.depends (BinPkgName "artvaluereport2-production") (anyrel (BinPkgName "artvaluereport2-server")) $
                             Atom.depends (BinPkgName "artvaluereport2-production") (anyrel (BinPkgName "apache2")) $
                             Atom.depends (BinPkgName "artvaluereport2-staging") (anyrel (BinPkgName "artvaluereport2-server")) $
                             -- This should go into the "real" data directory.  And maybe a different icon for each server?
                             insertAtom (Binary (BinPkgName "artvaluereport2-server")) (DHInstall "theme/ArtValueReport_SunsetSpectrum.ico" "usr/share/artvaluereport2-data") $
{-
                                 -- setSourcePackageName (SrcPkgName "haskell-creativeprompts") $
                                 -- setChangelog oldLog $
                                 putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $
                                 setArchitecture (Binary (BinPkgName "artvaluereport2-server")) Any $
                                 setArchitecture (Binary (BinPkgName "artvaluereport2-development")) All $
                                 setArchitecture (Binary (BinPkgName "artvaluereport2-production")) All $
                                 setArchitecture (Binary (BinPkgName "artvaluereport2-data")) All $
-}
                             doBackups (BinPkgName "artvaluereport2-backups") "artvaluereport2-backups" $
                             doWebsite (BinPkgName "artvaluereport2-production") (theSite (BinPkgName "artvaluereport2-production")) $
                             doServer (BinPkgName "artvaluereport2-staging") (theServer (BinPkgName "artvaluereport2-staging")) $
                             doServer (BinPkgName "artvaluereport2-development") (theServer (BinPkgName "artvaluereport2-development")) $
                             flip (foldr (\ s deb -> Atom.depends (BinPkgName "artvaluereport2-server") (anyrel (BinPkgName s)) deb))
                                  ["libjs-jquery", "libjs-jquery-ui", "libjs-jcrop", "libjpeg-progs", "netpbm",
                                   "texlive-latex-recommended", "texlive-latex-extra", "texlive-fonts-recommended", "texlive-fonts-extra"] $
                             doExecutable (BinPkgName "artvaluereport2-server") (InstallFile "artvaluereport2-server" Nothing Nothing "artvaluereport2-server") $
                             new
                  withCurrentDirectory "/tmp" (writeDebianization new')
                  assertEqual "test6" [] (diffDebianizations old new')
             )
    where
      -- hints = dependencyHints (putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $ putExtraDevDep (BinPkgName "markdown") $ Flags.defaultFlags)
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      -- dropFirstLogEntry (deb@(Debianization {changelog = ChangeLog (_ : tl)})) = deb {changelog = ChangeLog tl}
{-
      addMarkdownDependency :: Debianization -> Debianization
      addMarkdownDependency deb = updateBinaryPackage (BinPkgName "creativeprompts-server") addMarkdownDependency' deb
      addMarkdownDependency' :: BinaryDebDescription -> BinaryDebDescription
      addMarkdownDependency' deb = deb {relations = (relations deb) {depends = [[Rel (BinPkgName "markdown") Nothing Nothing]] ++ depends (relations deb)}}
-}
      theSite :: BinPkgName -> Site
      theSite deb =
          Site { domain = hostname'
               , serverAdmin = "logic@seereason.com"
               , server = theServer deb }
      theServer :: BinPkgName -> Server
      theServer deb =
          Server { hostname =
                       case deb of
                         BinPkgName "artvaluereport2-production" -> hostname'
                         _ -> hostname'
                 , port = portNum deb
                 , headerMessage = "Generated by artvaluereport2/Setup.hs"
                 , retry = "60"
                 , serverFlags =
                    ([ "--http-port", show (portNum deb)
                     , "--base-uri", case deb of
                                       BinPkgName "artvaluereport2-production" -> "http://" ++ hostname' ++ "/"
                                       _ -> "http://seereason.com:" ++ show (portNum deb) ++ "/"
                     , "--top", databaseDirectory deb
                     , "--logs", "/var/log/" ++ show (pretty deb)
                     , "--log-mode", case deb of
                                       BinPkgName "artvaluereport2-production" -> "Production"
                                       _ -> "Development"
                     , "--static", "/usr/share/artvaluereport2-data"
                     , "--no-validate" ] ++
                     (case deb of
                        BinPkgName "artvaluereport2-production" -> [{-"--enable-analytics"-}]
                        _ -> []) {- ++
                     [ "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path ] -})
                 , installFile =
                     InstallFile { execName   = "artvaluereport2-server"
                                 , destName   = show (pretty deb)
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }
      hostname' = "my.appraisalreportonline.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "artvaluereport2-production"  -> 9027
            "artvaluereport2-staging"     -> 9031
            "artvaluereport2-development" -> 9032
            _ -> error $ "Unexpected package name: " ++ deb
{-
      jstreePath = "/usr/share/clckwrks-0.13.2/jstree"
      json2Path = "/usr/share/clckwrks-0.13.2/json2"
-}

test7 :: Test
test7 =
    TestLabel "test7" $
    TestCase ( do old <- inputDebianization "."
                  let log = changeLog old
                      standards = StandardsVersion 3 9 3 Nothing
                      compat = 7
                  new <- cabalToDebianization "test-data/cabal-debian/input" (newDebianization log (Left BSD3) compat standards)
                  let new' = finalizeDebianization $
                             (\ x -> x {sourceDebDescription = (sourceDebDescription x) {homepage = Just "http://src.seereason.com/cabal-debian"}}) $
                             sourceFormat Native3 $
                             insertAtom (Binary (BinPkgName "libghc-cabal-debian-doc")) (DHLink "//usr/share/doc/libghc-cabal-debian-doc/html/cabal-debian.txt" "/usr/lib/ghc-doc/hoogle//cabal-debian.txt") $
                             insertAtom Source (UtilsPackageName (BinPkgName "cabal-debian")) $
                             Atom.depends (BinPkgName "cabal-debian") (anyrel (BinPkgName "apt-file")) $
                             Atom.conflicts (BinPkgName "cabal-debian")
                                     (Rel (BinPkgName "haskell-debian-utils") (Just (SLT (parseDebianVersion ("3.59" :: String)))) Nothing) $
                             copyChangelog old $
                             copyCopyright old $
                             new
                  assertEqual "test7" [] (diffDebianizations old new')
             )

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
      old' = toFileMap (sortBinaryDebs old)
      new' = toFileMap (sortBinaryDebs new)
      isUnchanged (Unchanged _ _) = True
      isUnchanged _ = False
      prettyChange (Unchanged p _) = text ("Unchanged: " <> p <> "\n")
      prettyChange (Deleted p _) = text ("Deleted: " <> p <> "\n")
      prettyChange (Created p b) =
          text ("Created: " <> p <> "\n") <>
          prettyDiff ("old" </> p) ("new" </> p)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 mempty (T.split (== '\n') b))
      prettyChange (Modified p a b) =
          text ("Modified: " <> p<> "\n") <>
          prettyDiff ("old" </> p) ("new" </> p)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 (T.split (== '\n') a) (T.split (== '\n') b))
      sortBinaryDebs deb = deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = sortBy (compare `on` package) (binaryPackages (sourceDebDescription deb))}}
{-
updateBinaryPackage :: BinPkgName -> (BinaryDebDescription -> BinaryDebDescription) -> Debianization -> Debianization
updateBinaryPackage name f deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = map update (binaryPackages (sourceDebDescription deb))}}
    where update p | package p == name = f p
          update p = p
-}

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (unlines [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))
