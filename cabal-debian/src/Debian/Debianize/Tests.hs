{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Tests
    ( tests
    ) where

import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Function (on)
import Data.Lens.Lazy (setL, getL, modL)
import Data.List (sortBy)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, (<>), mempty)
import Data.Set as Set (fromList, union, insert, singleton)
import qualified Data.Text as T
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.Debianize (debianization)
import Debian.Debianize.Atoms as Atoms
    (Atoms, rulesHead, compat, sourceFormat, changelog, control, missingDependencies, revision,
     binaryArchitectures, copyright, debVersion, execMap, buildDeps, utilsPackageName, description,
     depends, installData {-, sourcePackageName, install, buildDepsIndep-})
import Debian.Debianize.ControlFile as Deb (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), VersionControlSpec(..))
-- import Debian.Debianize.Debianize (writeDebianization)
import Debian.Debianize.Dependencies (getRulesHead)
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Finalize (finalizeDebianization)
import Debian.Debianize.Goodies (defaultAtoms, tightDependencyFixup, doExecutable, doWebsite, doServer, doBackups)
import Debian.Debianize.Input (inputChangeLog, inputDebianization, inputCabalization)
import Debian.Debianize.Types (InstallFile(..), Server(..), Site(..), Top(Top))
import Debian.Policy (databaseDirectory, StandardsVersion(StandardsVersion), getDebhelperCompatLevel,
                      getDebianStandardsVersion, PackagePriority(Extra), PackageArchitectures(All),
                      SourceFormat(Native3), Section(..), parseMaintainer)
import Debian.Relation (Relation(..), VersionReq(..), SrcPkgName(..), BinPkgName(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (buildDebianVersion, parseDebianVersion)
import Distribution.License (License(BSD3))
import Prelude hiding (log)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLog -> Int -> StandardsVersion -> Atoms
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    setL changelog (Just log) $
    setL compat (Just level) $
    modL control (\ x -> x { source = Just (SrcPkgName (logPackage entry))
                           , maintainer = (either error Just (parseMaintainer (logWho entry)))
                           , standardsVersion = Just standards }) $
    defaultAtoms
newDebianization _ _ _ = error "Invalid changelog"

newDebianization' :: Int -> StandardsVersion -> Atoms
newDebianization' level standards =
    setL compat (Just level) $
    modL control (\ x -> x { standardsVersion = Just standards }) $
    defaultAtoms

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9])

test1 :: Test
test1 =
    TestLabel "test1" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = finalizeDebianization $ setL copyright (Just (Left BSD3)) $
                           newDebianization (ChangeLog [testEntry]) level standards
                 assertEqual "test1" [] (diffDebianizations testDeb1 deb))
    where
      testDeb1 :: Atoms
      testDeb1 =
          setL rulesHead (Just . T.unlines $
                          [ "#!/usr/bin/make -f"
                          , ""
                          , "include /usr/share/cdbs/1/rules/debhelper.mk"
                          , "include /usr/share/cdbs/1/class/hlibrary.mk" ]) $
          setL compat (Just 9) $ -- This will change as new version of debhelper are released
          setL copyright (Just (Left BSD3)) $
          modL control
              (\ y -> y { source = Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                        , maintainer = Just (NameAddr (Just "David Fox") "dsf@seereason.com")
                        , standardsVersion = Just (StandardsVersion 3 9 3 (Just 1)) -- This will change as new versions of debian-policy are released
                        , buildDepends = [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                                  [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                                  [Rel (BinPkgName "cdbs") Nothing Nothing],
                                                  [Rel (BinPkgName "ghc") Nothing Nothing],
                                                  [Rel (BinPkgName "ghc-prof") Nothing Nothing]]
                        , buildDependsIndep = [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]
                        }) $
          (newDebianization log 9 (StandardsVersion 3 9 3 (Just 1)))
      log = ChangeLog [Entry { logPackage = "haskell-cabal-debian"
                             , logVersion = buildDebianVersion Nothing "2.6.2" Nothing
                             , logDists = [ReleaseName {relName = "unstable"}]
                             , logUrgency = "low"
                             , logComments = "  * Fix a bug constructing the destination pathnames that was dropping\n    files that were supposed to be installed into packages.\n"
                             , logWho = "David Fox <dsf@seereason.com>"
                             , logDate = "Thu, 20 Dec 2012 06:49:25 -0800" }]

test2 :: Test
test2 =
    TestLabel "test2" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = finalizeDebianization $ setL copyright (Just (Left BSD3)) $ newDebianization (ChangeLog [testEntry]) level standards
                 assertEqual "test2" [] (diffDebianizations expect deb))
    where
      expect =
          setL rulesHead (Just . T.unlines $
                          ["#!/usr/bin/make -f",
                           "",
                           "include /usr/share/cdbs/1/rules/debhelper.mk",
                           "include /usr/share/cdbs/1/class/hlibrary.mk"]) $
          setL compat (Just 9) $
          setL copyright (Just (Left BSD3)) $
          modL control
              (\ y -> y
                { source = Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"}),
                  maintainer = Just (NameAddr {nameAddr_name = Just "David Fox", nameAddr_addr = "dsf@seereason.com"}),
                  standardsVersion = Just (StandardsVersion 3 9 3 (Just 1)),
                  buildDepends = [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                  [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                  [Rel (BinPkgName "cdbs") Nothing Nothing],
                                  [Rel (BinPkgName "ghc") Nothing Nothing],
                                  [Rel (BinPkgName "ghc-prof") Nothing Nothing]],
                  buildDependsIndep = [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]
                }) $
          (newDebianization log 9 (StandardsVersion 3 9 3 (Just 1)))
      log = ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                              logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                              logDists = [ReleaseName {relName = "unstable"}],
                              logUrgency = "low",
                              logComments = unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                     "    files that were supposed to be installed into packages."],
                              logWho = "David Fox <dsf@seereason.com>",
                              logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}]

test3 :: Test
test3 =
    TestLabel "test3" $
    TestCase (do deb <- inputDebianization (Top "test-data/haskell-devscripts")
                 assertEqual "test3" [] (diffDebianizations testDeb2 deb))
    where
      testDeb2 :: Atoms
      testDeb2 =
          setL sourceFormat (Just Native3) $
          setL rulesHead (Just "#!/usr/bin/make -f\n# -*- makefile -*-\n\n# Uncomment this to turn on verbose mode.\n#export DH_VERBOSE=1\n\nDEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')\n\nmanpages = $(shell cat debian/manpages)\n\n%.1: %.pod\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n%.1: %\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n.PHONY: build\nbuild: $(manpages)\n\ninstall-stamp:\n\tdh install\n\n.PHONY: install\ninstall: install-stamp\n\nbinary-indep-stamp: install-stamp\n\tdh binary-indep\n\ttouch $@\n\n.PHONY: binary-indep\nbinary-indep: binary-indep-stamp\n\n.PHONY: binary-arch\nbinary-arch: install-stamp\n\n.PHONY: binary\nbinary: binary-indep-stamp\n\n.PHONY: clean\nclean:\n\tdh clean\n\trm -f $(manpages)\n\n\n") $
          setL compat (Just 7) $
          setL copyright (Just (Right "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n\n")) $
          modL control
              (\ y -> y
                { source = Just (SrcPkgName {unSrcPkgName = "haskell-devscripts"})
                , maintainer = Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                , uploaders = [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                , priority = Just Extra
                , section = Just (MainSection "haskell")
                , buildDepends = (buildDepends y) ++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]]
                , buildDependsIndep = (buildDependsIndep y) ++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
                , standardsVersion = Just (StandardsVersion 3 9 4 Nothing)
                , vcsFields = Set.union (vcsFields y) (Set.fromList [ VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                                    , VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                , binaryPackages = [BinaryDebDescription { package = BinPkgName {unBinPkgName = "haskell-devscripts"}
                                                         , architecture = All
                                                         , binarySection = Nothing
                                                         , binaryPriority = Nothing
                                                         , essential = False
                                                         , Deb.description =
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
                                                             , builtUsing = [] }}]}) $
          (newDebianization log 7 (StandardsVersion 3 9 4 Nothing))
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

test4 :: Test
test4 =
    TestLabel "test4" $
    TestCase (do old <- inputDebianization (Top "test-data/clckwrks-dot-com/output")
                 new <- inputCabalization (Top "test-data/clckwrks-dot-com/input") (newDebianization' 7 (StandardsVersion 3 9 4 Nothing)) >>=
                        debianization (Top "test-data/clckwrks-dot-com/input") .
                          (modL control (\ y -> y {homepage = Just "http://www.clckwrks.com/"}) .
                           setL sourceFormat (Just Native3) .
                           modL missingDependencies (insert (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")) .
                           setL revision Nothing .
                           doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production")) .
                           doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups" .
                           fixRules .
                           tight .
                           setL changelog (getL changelog old))
                 assertEqual "test4" [] (diffDebianizations old (copyFirstLogEntry old new)))
    where
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules deb =
          modL rulesHead (\ mt -> (Just . f) (fromMaybe (getRulesHead deb) mt)) deb
          where
            f t = T.unlines $ concat $
                  map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                 then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                 else [line] :: [T.Text]) (T.lines t)
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
    TestCase (do old <- inputDebianization (Top "test-data/creativeprompts/output")
                 let standards = fromMaybe (error "test5") (standardsVersion (getL control old))
                     level = fromMaybe (error "test5") (getL compat old)
                 new <- debianization (Top "test-data/creativeprompts/input")
                          (setL sourceFormat (Just Native3) $
                           modL binaryArchitectures (Map.insert (BinPkgName "creativeprompts-data") All) $
                           modL binaryArchitectures (Map.insert (BinPkgName "creativeprompts-development") All) $
                           modL binaryArchitectures (Map.insert (BinPkgName "creativeprompts-production") All) $
                           setL utilsPackageName (Just (BinPkgName "creativeprompts-data")) $
                           modL Atoms.description (Map.insertWith (error "test5") (BinPkgName "creativeprompts-data")
                                                    (T.intercalate "\n" [ "creativeprompts.com data files"
                                                               , "  Static data files for creativeprompts.com"])) $
                           modL Atoms.description (Map.insertWith (error "test5") (BinPkgName "creativeprompts-production")
                                                    (T.intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                                               , "  Production version of the blog server, runs on port"
                                                               , "  9021 with HTML validation turned off." ])) $
                           modL Atoms.description (Map.insertWith (error "test5") (BinPkgName "creativeprompts-development")
                                                    (T.intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                                               , "  Testing version of the blog server, runs on port"
                                                               , "  8000 with HTML validation turned on." ])) $
                           modL Atoms.description (Map.insertWith (error "test5") (BinPkgName "creativeprompts-backups")
                                                    (T.intercalate "\n" [ "backup program for creativeprompts.com"
                                                               , "  Install this somewhere other than creativeprompts.com to run automated"
                                                               , "  backups of the database."])) $
                           modL Atoms.depends (Map.insertWith union (BinPkgName "creativeprompts-server") (singleton (anyrel (BinPkgName "markdown")))) $
                           modL execMap (Map.insertWith (error "Conflict in execMap") "trhsx" (BinPkgName "haskell-hsx-utils")) $
                           doBackups (BinPkgName "creativeprompts-backups") "creativeprompts-backups" $
                           doServer (BinPkgName "creativeprompts-development") (theServer (BinPkgName "creativeprompts-development")) $
                           doWebsite (BinPkgName "creativeprompts-production") (theSite (BinPkgName "creativeprompts-production")) $
                           setL changelog (getL changelog old) $
                           (newDebianization' level standards))
                 assertEqual "test5" [] (diffDebianizations old (copyFirstLogEntry old new)))
    where
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


copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)

copyChangelog :: Atoms -> Atoms -> Atoms
copyChangelog deb1 deb2 = modL changelog (const (getL changelog deb1)) deb2

test6 :: Test
test6 =
    TestLabel "test6" $
    TestCase (do result <- readProcessWithExitCode "runhaskell" ["-isrc", "test-data/artvaluereport2/input/debian/Debianize.hs"] ""
                 assertEqual "test6" (ExitSuccess, "", "") result)

test7 :: Test
test7 =
    TestLabel "test7" $
    TestCase (do new <- readProcessWithExitCode "runhaskell" ["-isrc", "debian/Debianize.hs"] ""
                 assertEqual "test7" (ExitSuccess, "", "Ignored: ./debian/cabal-debian.manpages\nIgnored: ./debian/cabal-debian.1\n") new)

test8 :: Test
test8 =
    TestLabel "test8" $
    TestCase ( do old <- inputDebianization (Top "test-data/artvaluereport-data/output")
                  log <- inputChangeLog (Top "test-data/artvaluereport-data/input")
                  new <- debianization (Top "test-data/artvaluereport-data/input")
                           (modL buildDeps (Set.insert (BinPkgName "haskell-hsx-utils")) $
                            modL control (\ y -> y {homepage = Just "http://artvaluereportonline.com"}) $
                            setL sourceFormat (Just Native3) $
                            setL changelog (Just log) $
                            (newDebianization' 7 (StandardsVersion 3 9 3 Nothing)))
                  assertEqual "test8" [] (diffDebianizations old (copyChangelog old new))
             )

test9 :: Test
test9 =
    TestLabel "test9" $
    TestCase ( do old <- inputDebianization (Top "test-data/alex/output")
                  new <- debianization (Top "test-data/alex/input")
                           (modL buildDeps (Set.insert (BinPkgName "alex")) $
                            doExecutable (BinPkgName "alex") (InstallFile {execName = "alex", destName = "alex", sourceDir = Nothing, destDir = Nothing}) $
                            setL debVersion (Just (parseDebianVersion ("3.0.2-1~hackage1" :: String))) $
                            setL sourceFormat (Just Native3) $
                            modL control (\ y -> y {homepage = Just "http://www.haskell.org/alex/"}) $
                            (\ atoms -> foldr (\ name atoms' -> modL installData (Map.insertWith union (BinPkgName "alex") (singleton (name, name))) atoms')
                                              atoms
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
                                              , "AlexWrapper-strict-bytestring"]) $
                            newDebianization' 7 (StandardsVersion 3 9 3 Nothing))
                  assertEqual "test9" [] (diffDebianizations old (copyFirstLogEntry old new)))

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

diffDebianizations :: Atoms -> Atoms -> String -- [Change FilePath T.Text]
diffDebianizations old new =
    show (mconcat (map prettyChange (filter (not . isUnchanged) (diffMaps old' new'))))
    where
      old' = toFileMap (sortBinaryDebs old) -- (sortBinaryDebs (fromMaybe newSourceDebDescription . getL control $ old))
      new' = toFileMap (sortBinaryDebs new) -- (sortBinaryDebs (fromMaybe newSourceDebDescription . getL control $ new))
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
      sortBinaryDebs atoms = modL control (\ deb -> deb {binaryPackages = sortBy (compare `on` package) (binaryPackages deb)}) atoms

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (unlines [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))
