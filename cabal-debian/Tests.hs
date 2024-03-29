{-# LANGUAGE CPP, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Main
    ( tests
    , main
    ) where

import Control.Applicative ((<$>))
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Function (on)
import Data.Lens.Lazy (access, getL)
import Data.List (sortBy)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map (elems, Map, toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Set as Set (fromList, singleton, union)
import Data.Text as Text (intercalate, lines, split, Text, unlines)
import Data.Version (Version(Version, versionBranch))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.Finalize (debianization, finalizeDebianization')
import Debian.Debianize.Goodies (doBackups, doExecutable, doServer, doWebsite, makeRulesHead, tightDependencyFixup)
import Debian.Debianize.Input (inputChangeLog, inputDebianization)
import Debian.Debianize.Monad (DebT, evalDebT, execDebM, execDebT)
import Debian.Debianize.Prelude ((%=), (+++=), (++=), (+=), (~=), withCurrentDirectory)
import Debian.Debianize.Types as T
import Debian.Debianize.Types.Atoms as T
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Policy (databaseDirectory, PackageArchitectures(All), PackagePriority(Extra), parseMaintainer, Section(MainSection), SourceFormat(Native3), StandardsVersion(..), getDebhelperCompatLevel, getDebianStandardsVersion)
import Debian.Pretty (pretty, text, Doc)
import Debian.Relation (BinPkgName(..), Relation(..), SrcPkgName(..), VersionReq(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (parseDebianVersion, buildDebianVersion)
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.License (License(..))
import Distribution.Package (PackageName(PackageName))
import Prelude hiding (log)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit hiding ((~?=))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))

-- | A suitable defaultAtoms value for the debian repository.
defaultAtoms :: Monad m => DebT m ()
defaultAtoms =
    do T.epochMap ++= (PackageName "HaXml", 1)
       T.epochMap ++= (PackageName "HTTP", 1)
       mapCabal (PackageName "parsec") (DebBase "parsec3")
       splitCabal (PackageName "parsec") (DebBase "parsec2") (Version [3] [])
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (PackageName "QuickCheck") (DebBase "quickcheck1") (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")

-- | Force the compiler version to 7.6 to get predictable outputs
testAtoms :: IO Atoms
testAtoms = ghc763 <$> T.newAtoms GHC
    where
      ghc763 :: Atoms -> Atoms
      ghc763 atoms = atoms
{-
#if MIN_VERSION_Cabal(1,21,0)
          let CompilerId flavor version _ = getL ghcVersion_ atoms in
          atoms {ghcVersion_ = CompilerId flavor (version {versionBranch = [7, 6, 3]}) Nothing}
#else
          let CompilerId flavor version = ghcVersion_ atoms in
          atoms {ghcVersion_ = CompilerId flavor (version {versionBranch = [7, 6, 3]})}
#endif
-}

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: Monad m => ChangeLog -> Maybe Int -> Maybe StandardsVersion -> DebT m ()
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    do T.changelog ~= Just log
       T.compat ~= level
       T.source ~= Just (SrcPkgName (logPackage entry))
       T.maintainer ~= either error Just (parseMaintainer (logWho entry))
       T.standardsVersion ~= standards
newDebianization _ _ _ = error "Invalid changelog"

newDebianization' :: Monad m => Maybe Int -> Maybe StandardsVersion -> DebT m ()
newDebianization' level standards =
    do T.compat ~= level
       T.standardsVersion ~= standards

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [-- 1 and 2 do not input a cabal package - we're not ready to
                                                   -- debianize without a cabal package.
                                                   {- test1 "test1",
                                                   test2 "test2", -}
                                                   test3 "test3",
                                                   test4 "test4 - test-data/clckwrks-dot-com",
                                                   test5 "test5 - test-data/creativeprompts",
                                                   test6 "test6 - test-data/artvaluereport2",
                                                   test7 "test7 - debian/Debianize.hs",
                                                   test8 "test8 - test-data/artvaluereport-data",
                                                   test9 "test9 - test-data/alex",
                                                   test10 "test10 - test-data/archive"])

test1 :: String -> Test
test1 label =
    TestLabel label $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion :: IO (Maybe StandardsVersion)
                 atoms <- testAtoms
                 deb <- execDebT
                          (do -- let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              license ~= Just BSD3
                              -- inputCabalization top
                              finalizeDebianization')
                          atoms
                 diff <- diffDebianizations (testDeb1 atoms) deb
                 assertEqual label [] diff)
    where
      testDeb1 :: Atoms -> Atoms
      testDeb1 atoms =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                rulesHead %= (const (Just (Text.unlines $
                                                [ "#!/usr/bin/make -f"
                                                , ""
                                                , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                , "include /usr/share/cdbs/1/class/hlibrary.mk" ])))
                compat ~= Just 9 -- This will change as new version of debhelper are released
                license ~= Just BSD3
                T.source ~= Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                T.maintainer ~= Just (NameAddr (Just "David Fox") "dsf@seereason.com")
                T.standardsVersion ~= Just (StandardsVersion 3 9 3 (Just 1)) -- This will change as new versions of debian-policy are released
                T.buildDepends %= (++ [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                       [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                       [Rel (BinPkgName "cdbs") Nothing Nothing],
                                       [Rel (BinPkgName "ghc") Nothing Nothing],
                                       [Rel (BinPkgName "ghc-prof") Nothing Nothing]])
                T.buildDependsIndep %= (++ [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]))
            atoms
      log = ChangeLog [Entry { logPackage = "haskell-cabal-debian"
                             , logVersion = buildDebianVersion Nothing "2.6.2" Nothing
                             , logDists = [ReleaseName {relName = "unstable"}]
                             , logUrgency = "low"
                             , logComments = "  * Fix a bug constructing the destination pathnames that was dropping\n    files that were supposed to be installed into packages.\n"
                             , logWho = "David Fox <dsf@seereason.com>"
                             , logDate = "Thu, 20 Dec 2012 06:49:25 -0800" }]

test2 :: String -> Test
test2 label =
    TestLabel label $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 atoms <- testAtoms
                 deb <- execDebT
                          (do -- let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              license ~= Just BSD3
                              -- inputCabalization top
                              finalizeDebianization')
                          atoms
                 diff <- diffDebianizations (expect atoms) deb
                 assertEqual label [] diff)
    where
      expect atoms =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                rulesHead %= (const (Just (Text.unlines $
                                                ["#!/usr/bin/make -f",
                                                 "",
                                                 "include /usr/share/cdbs/1/rules/debhelper.mk",
                                                 "include /usr/share/cdbs/1/class/hlibrary.mk"])))
                compat ~= Just 9
                license ~= Just BSD3
                T.source ~= Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                T.maintainer ~= Just (NameAddr {nameAddr_name = Just "David Fox", nameAddr_addr = "dsf@seereason.com"})
                T.standardsVersion ~= Just (StandardsVersion 3 9 3 (Just 1))
                T.buildDepends %= (++ [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                       [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                       [Rel (BinPkgName "cdbs") Nothing Nothing],
                                       [Rel (BinPkgName "ghc") Nothing Nothing],
                                       [Rel (BinPkgName "ghc-prof") Nothing Nothing]])
                T.buildDependsIndep %= (++ [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]))
            atoms
      log = ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                              logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                              logDists = [ReleaseName {relName = "unstable"}],
                              logUrgency = "low",
                              logComments = Prelude.unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                             "    files that were supposed to be installed into packages."],
                              logWho = "David Fox <dsf@seereason.com>",
                              logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}]

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (Prelude.unlines
                                [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))

test3 :: String -> Test
test3 label =
    TestLabel label $
    TestCase (do let top = "test-data/haskell-devscripts"
                     envset = EnvSet "/" "/" "/"
                 atoms <- testAtoms
                 deb <- withCurrentDirectory top (execDebT (inputDebianization envset) atoms)
                 diff <- diffDebianizations (testDeb2 atoms) deb
                 assertEqual label [] diff)
    where
      testDeb2 :: Atoms -> Atoms
      testDeb2 atoms =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
                T.sourceFormat ~= Just Native3
                T.rulesHead ~= Just (Text.unlines  ["#!/usr/bin/make -f",
                                                    "# -*- makefile -*-",
                                                    "",
                                                    "# Uncomment this to turn on verbose mode.",
                                                    "#export DH_VERBOSE=1",
                                                    "",
                                                    "DEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')",
                                                    "",
                                                    "manpages = $(shell cat debian/manpages)",
                                                    "",
                                                    "%.1: %.pod",
                                                    "\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@",
                                                    "",
                                                    "%.1: %",
                                                    "\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@",
                                                    "",
                                                    ".PHONY: build",
                                                    "build: $(manpages)",
                                                    "",
                                                    "install-stamp:",
                                                    "\tdh install",
                                                    "",
                                                    ".PHONY: install",
                                                    "install: install-stamp",
                                                    "",
                                                    "binary-indep-stamp: install-stamp",
                                                    "\tdh binary-indep",
                                                    "\ttouch $@",
                                                    "",
                                                    ".PHONY: binary-indep",
                                                    "binary-indep: binary-indep-stamp",
                                                    "",
                                                    ".PHONY: binary-arch",
                                                    "binary-arch: install-stamp",
                                                    "",
                                                    ".PHONY: binary",
                                                    "binary: binary-indep-stamp",
                                                    "",
                                                    ".PHONY: clean",
                                                    "clean:",
                                                    "\tdh clean",
                                                    "\trm -f $(manpages)",
                                                    "",
                                                    ""])
                T.compat ~= Just 7
                T.copyright ~= Just "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n\n"
                T.source ~= Just (SrcPkgName {unSrcPkgName = "haskell-devscripts"})
                T.maintainer ~= Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                T.uploaders ~= [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                T.sourcePriority ~= Just Extra
                T.sourceSection ~= Just (MainSection "haskell")
                T.buildDepends %= (++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]])
                T.buildDependsIndep %=  (++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]])
                T.standardsVersion ~= Just (StandardsVersion 3 9 4 Nothing)
                T.vcsFields %= Set.union (Set.fromList [ S.VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                       , S.VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                T.binaryArchitectures (BinPkgName "haskell-devscripts") ~= Just All
                T.debianDescription (BinPkgName "haskell-devscripts") ~=
                   Just
                     (intercalate "\n"   ["Tools to help Debian developers build Haskell packages",
                                          " This package provides a collection of scripts to help build Haskell",
                                          " packages for Debian.  Unlike haskell-utils, this package is not",
                                          " expected to be installed on the machines of end users.",
                                          " .",
                                          " This package is designed to support Cabalized Haskell libraries.  It",
                                          " is designed to build a library for each supported Debian compiler or",
                                          " interpreter, generate appropriate postinst/prerm files for each one,",
                                          " generate appropriate substvars entries for each one, and install the",
                                          " package in the Debian temporary area as part of the build process."])
                T.depends (BinPkgName "haskell-devscripts") ~=
                     [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "debhelper"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "dh-buildinfo"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "ghc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.6" :: String)))) Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "html-xml-utils"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "hscolour"}) (Just (GRE (Debian.Version.parseDebianVersion ("1.8" :: String)))) Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "ghc-haddock"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.4" :: String)))) Nothing] ]
{-
                control %= (\ y -> y { S.source = 
                                     , S.maintainer = Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                                     , S.uploaders = [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                                     , S.priority = Just Extra
                                     , S.section = Just (MainSection "haskell")
                                     , S.buildDepends = (S.buildDepends y) ++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]]
                                     , S.buildDependsIndep = (S.buildDependsIndep y) ++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
                                     , S.standardsVersion = Just (StandardsVersion 3 9 4 Nothing)
                                     , S.vcsFields = Set.union (S.vcsFields y) (Set.fromList [ S.VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                                                                 , S.VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                                     , S.binaryPackages = [S.BinaryDebDescription { B.package = BinPkgName {unBinPkgName = "haskell-devscripts"}
                                                                                      , B.architecture = All
                                                                                      , B.binarySection = Nothing
                                                                                      , B.binaryPriority = Nothing
                                                                                      , B.essential = False
                                                                                      , B.description = Just $
                                                                                          (T.intercalate "\n"
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
                                                                                      , B.relations =
                                                                                          B.PackageRelations
                                                                                            { B.depends =
                                                                                              [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "debhelper"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "dh-buildinfo"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "ghc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.6" :: String)))) Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "html-xml-utils"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "hscolour"}) (Just (GRE (Debian.Version.parseDebianVersion ("1.8" :: String)))) Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "ghc-haddock"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.4" :: String)))) Nothing] ]
                                                                                            , B.recommends = []
                                                                                            , B.suggests = []
                                                                                            , B.preDepends = []
                                                                                            , B.breaks = []
                                                                                            , B.conflicts = []
                                                                                            , B.provides_ = []
                                                                                            , B.replaces_ = []
                                                                                            , B.builtUsing = [] }}]})
-}
                                                                                            )
            atoms
      log = ChangeLog [Entry { logPackage = "haskell-devscripts"
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

test4 :: String -> Test
test4 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/clckwrks-dot-com/input"
                     outTop = "test-data/clckwrks-dot-com/output"
                     envset = EnvSet "/" "/" "/"
                 atoms <- testAtoms
                 old <- withCurrentDirectory outTop (execDebT (inputDebianization envset) atoms)
                 let log = getL T.changelog old
                 new <- withCurrentDirectory inTop (execDebT (debianization defaultAtoms (customize log)) atoms)
                 diff <- diffDebianizations old ({-copyFirstLogEntry old-} new)
                 assertEqual label [] diff)
    where
      customize :: Maybe ChangeLog -> DebT IO ()
      customize log =
          do T.changelog ~= log
             tight
             fixRules
             doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups"
             doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))
             T.revision ~= Nothing
             T.missingDependencies += (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")
             T.sourceFormat ~= Just Native3
             T.homepage ~= Just "http://www.clckwrks.com/"
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
{-
      customize log = modifyM (lift . customize' log)
      customize' :: Maybe ChangeLog -> Atoms -> IO Atoms
      customize' log atoms =
          execDebT (newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))) .
          modL T.control (\ y -> y {T.homepage = Just "http://www.clckwrks.com/"}) .
          setL T.sourceFormat (Just Native3) .
          modL T.missingDependencies (insert (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")) .
          setL T.revision Nothing .
          execDebM (doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))) .
          execDebM (doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups") .
          fixRules .
          execDebM tight .
          setL T.changelog log
-}
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules =
          makeRulesHead >>= \ rh -> T.rulesHead %= (\ mt -> (Just . f) (fromMaybe rh mt))
          where
            f t = Text.unlines $ concat $
                  map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                 then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [Text]
                                 else [line] :: [Text]) (Text.lines t)
{-
          mapAtoms f deb
          where
            f :: DebAtomKey -> DebAtom -> Set (DebAtomKey, DebAtom)
            f Source (DebRulesHead t) =
                singleton (Source, DebRulesHead (T.unlines $ concat $
                                                 map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                                                else [line] :: [T.Text]) (T.lines t)))
            f k a = singleton (k, a)
-}
      tight = mapM_ (tightDependencyFixup [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-bugs-dev", BinPkgName "haskell-clckwrks-plugin-bugs-utils"),
                                           (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) serverNames

      theSite :: BinPkgName -> T.Site
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

test5 :: String -> Test
test5 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/creativeprompts/input"
                     outTop = "test-data/creativeprompts/output"
                     envset = EnvSet "/" "/" "/"
                 atoms <- testAtoms
                 old <- withCurrentDirectory outTop (execDebT (inputDebianization envset) atoms)
                 let standards = getL T.standardsVersion old
                     level = getL T.compat old
                 new <- withCurrentDirectory inTop (execDebT (debianization defaultAtoms (customize old level standards)) atoms)
                 diff <- diffDebianizations old new
                 assertEqual label [] diff)
    where
      customize old level standards =
          do T.utilsPackageNames ~= singleton (BinPkgName "creativeprompts-data")
             newDebianization' level standards
             T.changelog ~= (getL T.changelog old)
             doWebsite (BinPkgName "creativeprompts-production") (theSite (BinPkgName "creativeprompts-production"))
             doServer (BinPkgName "creativeprompts-development") (theServer (BinPkgName "creativeprompts-development"))
             doBackups (BinPkgName "creativeprompts-backups") "creativeprompts-backups"
             T.execMap ++= ("trhsx", [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]])
             mapM_ (\ b -> T.depends b %= \ deps -> deps ++ [[anyrel (BinPkgName "markdown")]])
                   [(BinPkgName "creativeprompts-production"), (BinPkgName "creativeprompts-development")]
             T.debianDescription (BinPkgName "creativeprompts-development") ~=
                   Just (intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                            , "  Testing version of the blog server, runs on port"
                                            , "  8000 with HTML validation turned on." ])
             T.debianDescription (BinPkgName "creativeprompts-data") ~=
                   Just (intercalate "\n" [ "creativeprompts.com data files"
                                            , "  Static data files for creativeprompts.com"])
             T.debianDescription (BinPkgName "creativeprompts-production") ~=
                   Just (intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                            , "  Production version of the blog server, runs on port"
                                            , "  9021 with HTML validation turned off." ])
             T.debianDescription (BinPkgName "creativeprompts-backups") ~=
                   Just (intercalate "\n" [ "backup program for creativeprompts.com"
                                            , "  Install this somewhere other than creativeprompts.com to run automated"
                                            , "  backups of the database."])
             T.binaryArchitectures (BinPkgName "creativeprompts-production") ~= Just All
             T.binaryArchitectures (BinPkgName "creativeprompts-data") ~= Just All
             T.binaryArchitectures (BinPkgName "creativeprompts-development") ~= Just All
             T.sourceFormat ~= Just Native3

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

test6 :: String -> Test
test6 label =
    TestLabel label $
    TestCase (do result <- readProcessWithExitCode "runhaskell" ["-isrc", "-DMIN_VERSION_Cabal(1,18,0)", "test-data/artvaluereport2/input/debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "") result)

test7 :: String -> Test
test7 label =
    TestLabel label $
    TestCase (do new <- readProcessWithExitCode "runhaskell" ["-isrc", "debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "Ignored: ./debian/cabal-debian.1\nIgnored: ./debian/cabal-debian.manpages\n") new)

test8 :: String -> Test
test8 label =
    TestLabel label $
    TestCase ( do let inTop = "test-data/artvaluereport-data/input"
                      outTop = "test-data/artvaluereport-data/output"
                      envset = EnvSet "/" "/" "/"
                  atoms <- testAtoms
                  old <- withCurrentDirectory outTop (execDebT (inputDebianization envset) atoms)
                  log <- withCurrentDirectory inTop (evalDebT (inputChangeLog >> access T.changelog) atoms)
                  new <- withCurrentDirectory inTop (execDebT (debianization defaultAtoms (customize log)) atoms)
                  diff <- diffDebianizations old new
                  assertEqual label [] diff
             )
    where
      customize Nothing = error "Missing changelog"
      customize (Just log) =
          do T.buildDepends %= (++ [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]])
             T.homepage ~= Just "http://artvaluereportonline.com"
             T.sourceFormat ~= Just Native3
             T.changelog ~= Just log
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))

test9 :: String -> Test
test9 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/alex/input"
                     outTop = "test-data/alex/output"
                     envset = EnvSet "/" "/" "/"
                 atoms <- testAtoms
                 old <- withCurrentDirectory outTop (execDebT (inputDebianization envset) atoms)
                 let Just (ChangeLog (entry : _)) = getL T.changelog old
                 new <- withCurrentDirectory inTop (execDebT (debianization defaultAtoms customize >> copyChangelogDate (logDate entry)) atoms)
                 diff <- diffDebianizations old new
                 assertEqual label [] diff)
    where
      customize =
          do newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))
             mapM_ (\ name -> T.installData +++= (BinPkgName "alex", singleton (name, name)))
                   [ "AlexTemplate"
                   , "AlexTemplate-debug"
                   , "AlexTemplate-ghc"
                   , "AlexTemplate-ghc-debug"
                   , "AlexWrapper-basic"
                   , "AlexWrapper-basic-bytestring"
                   , "AlexWrapper-gscan"
                   , "AlexWrapper-monad"
                   , "AlexWrapper-monad-bytestring"
                   , "AlexWrapper-monadUserState"
                   , "AlexWrapper-monadUserState-bytestring"
                   , "AlexWrapper-posn"
                   , "AlexWrapper-posn-bytestring"
                   , "AlexWrapper-strict-bytestring"]
             T.homepage ~= Just "http://www.haskell.org/alex/"
             T.sourceFormat ~= Just Native3
             T.debVersion ~= Just (parseDebianVersion ("3.0.2-1~hackage1" :: String))
             doExecutable (BinPkgName "alex")
                          (InstallFile {execName = "alex", destName = "alex", sourceDir = Nothing, destDir = Nothing})
             -- Bootstrap dependency
             T.buildDepends %= (++ [[Rel (BinPkgName "alex") Nothing Nothing]])

test10 :: String -> Test
test10 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/archive/input"
                     outTop = "test-data/archive/output"
                     envset = EnvSet "/" "/" "/"
                 atoms <- testAtoms
                 old <- withCurrentDirectory outTop (execDebT (inputDebianization envset) atoms)
                 let Just (ChangeLog (entry : _)) = getL T.changelog old
                 new <- withCurrentDirectory inTop (execDebT (debianization defaultAtoms customize >> copyChangelogDate (logDate entry)) atoms)
                 diff <- diffDebianizations old new
                 assertEqual label [] diff)
    where
      customize :: DebT IO ()
      customize =
          do T.sourcePackageName ~= Just (SrcPkgName "seereason-darcs-backups")
             T.compat ~= Just 5
             T.standardsVersion ~= Just (StandardsVersion 3 8 1 Nothing)
             T.maintainer ~= either (const Nothing) Just (parseMaintainer "David Fox <dsf@seereason.com>")
             T.depends (BinPkgName "seereason-darcs-backups") %= (++ [[Rel (BinPkgName "anacron") Nothing Nothing]])
             T.sourceSection ~= Just (MainSection "haskell")
             T.utilsPackageNames += utils
             T.installCabalExec +++= (utils, singleton ("seereason-darcs-backups", "/etc/cron.hourly"))
      utils = BinPkgName "seereason-darcs-backups"

copyChangelogDate :: Monad m => String -> DebT m ()
copyChangelogDate date =
    T.changelog %= (\ (Just (ChangeLog (entry : older))) -> Just (ChangeLog (entry {logDate = date} : older)))

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

diffDebianizations :: Atoms -> Atoms -> IO String -- [Change FilePath T.Text]
diffDebianizations old new =
    do old' <- evalDebT (sortBinaryDebs >> debianizationFileMap) old
       new' <- evalDebT (sortBinaryDebs >> debianizationFileMap) new
       return $ show $ mconcat $ map prettyChange $ filter (not . isUnchanged) $ diffMaps old' new'
    where
      isUnchanged (Unchanged _ _) = True
      isUnchanged _ = False
      prettyChange :: Change FilePath Text -> Doc
      prettyChange (Unchanged p _) = text "Unchanged: " <> pretty p <> text "\n"
      prettyChange (Deleted p _) = text "Deleted: " <> pretty p <> text "\n"
      prettyChange (Created p b) =
          text "Created: " <> pretty p <> text "\n" <>
          prettyDiff ("old" </> p) ("new" </> p)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 mempty (split (== '\n') b))
      prettyChange (Modified p a b) =
          text "Modified: " <> pretty p <> text "\n" <>
          prettyDiff ("old" </> p) ("new" </> p)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 (split (== '\n') a) (split (== '\n') b))

sortBinaryDebs :: DebT IO ()
sortBinaryDebs = T.binaryPackages %= sortBy (compare `on` getL B.package)

main :: IO ()
main = runTestTT tests >>= putStrLn . show

