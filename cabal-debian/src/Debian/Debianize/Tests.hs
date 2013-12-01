{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Main
    ( tests
    , main
    ) where

-- import Control.Monad.State (get, put)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Function (on)
import Data.Lens.Lazy (getL, modL, access)
import Data.List (sortBy)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, (<>), mempty)
import Data.Set as Set (fromList, union, insert)
import qualified Data.Text as T
import Data.Version (Version(Version))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.Facts.Lenses as Lenses
    (changelog, compat, control, copyright, rulesHead, sourceFormat, installData, debVersion, buildDeps,
     execMap, utilsPackageNames, binaryArchitectures, depends, description, revision, missingDependencies,
     installCabalExec, rulesHead, compat, sourceFormat, changelog, control, buildDeps, epochMap)
import Debian.Debianize.Facts.Monad
    (Atoms, DebT, evalDebT, execDebM, execDebT, mapCabal, splitCabal)
import Debian.Debianize.Facts.Types as Deb
    (Top(..), InstallFile(..), Server(..), Site(..), {-Atoms(top),-} newAtoms,
     SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), VersionControlSpec(..))
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.Finalize (debianization, finalizeDebianization')
import Debian.Debianize.Goodies (tightDependencyFixup, doExecutable, doWebsite, doServer, doBackups, makeRulesHead)
import Debian.Debianize.Input (inputChangeLog, inputDebianization, inputCabalization)
import Debian.Debianize.Utility ((~=), (%=), (+=), (++=), (+++=))
import Debian.Policy (databaseDirectory, StandardsVersion(StandardsVersion), getDebhelperCompatLevel,
                      getDebianStandardsVersion, PackagePriority(Extra), PackageArchitectures(All),
                      SourceFormat(Native3), Section(..), parseMaintainer)
import Debian.Relation (Relation(..), VersionReq(..), SrcPkgName(..), BinPkgName(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (buildDebianVersion, parseDebianVersion)
import Distribution.License (License(BSD3))
import Distribution.Package (PackageName(PackageName))
import Prelude hiding (log)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit hiding ((~?=))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

-- | A suitable defaultAtoms value for the debian repository.
defaultAtoms :: Monad m => DebT m ()
defaultAtoms =
    do epochMap ++= (PackageName "HaXml", 1)
       epochMap ++= (PackageName "HTTP", 1)
       mapCabal (PackageName "parsec") "parsec3"
       splitCabal (PackageName "parsec") "parsec2" (Version [3] [])
       mapCabal (PackageName "QuickCheck") "quickcheck2"
       splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools"

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: Monad m => ChangeLog -> Maybe Int -> Maybe StandardsVersion -> DebT m ()
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    do changelog ~= Just log
       compat ~= level
       control %= (\ x -> x { Deb.source = Just (SrcPkgName (logPackage entry))
                            , Deb.maintainer = (either error Just (parseMaintainer (logWho entry)))
                            , Deb.standardsVersion = standards })
newDebianization _ _ _ = error "Invalid changelog"

newDebianization' :: Monad m => Maybe Int -> Maybe StandardsVersion -> DebT m ()
newDebianization' level standards =
    do compat ~= level
       control %= (\ x -> x { Deb.standardsVersion = standards })

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [test1 "test1",
                                                   test2 "test2",
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
                 deb <- execDebT
                          (do let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              copyright ~= Just (Left BSD3)
                              -- inputCabalization top
                              finalizeDebianization')
                          newAtoms
                 diff <- diffDebianizations testDeb1 deb
                 assertEqual label [] diff)
    where
      testDeb1 :: Atoms
      testDeb1 =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                rulesHead %= (const (Just (T.unlines $
                                                [ "#!/usr/bin/make -f"
                                                , ""
                                                , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                , "include /usr/share/cdbs/1/class/hlibrary.mk" ])))
                compat ~= Just 9 -- This will change as new version of debhelper are released
                copyright ~= Just (Left BSD3)
                control %= (\ y -> y { Deb.source = Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                                     , Deb.maintainer = Just (NameAddr (Just "David Fox") "dsf@seereason.com")
                                     , Deb.standardsVersion = Just (StandardsVersion 3 9 3 (Just 1)) -- This will change as new versions of debian-policy are released
                                     , Deb.buildDepends = [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                                           [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                                           [Rel (BinPkgName "cdbs") Nothing Nothing],
                                                           [Rel (BinPkgName "ghc") Nothing Nothing],
                                                           [Rel (BinPkgName "ghc-prof") Nothing Nothing]]
                                     , Deb.buildDependsIndep = [[Rel (BinPkgName "ghc-doc") Nothing Nothing]] }))
            newAtoms
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
                 deb <- execDebT
                          (do let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              copyright ~= Just (Left BSD3)
                              -- inputCabalization top
                              finalizeDebianization')
                          newAtoms
                 diff <- diffDebianizations expect deb
                 assertEqual label [] diff)
    where
      expect =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                rulesHead %= (const (Just (T.unlines $
                                                ["#!/usr/bin/make -f",
                                                 "",
                                                 "include /usr/share/cdbs/1/rules/debhelper.mk",
                                                 "include /usr/share/cdbs/1/class/hlibrary.mk"])))
                compat ~= Just 9
                copyright ~= Just (Left BSD3)
                control %= (\ y -> y { Deb.source = Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"}),
                                       Deb.maintainer = Just (NameAddr {nameAddr_name = Just "David Fox", nameAddr_addr = "dsf@seereason.com"}),
                                       Deb.standardsVersion = Just (StandardsVersion 3 9 3 (Just 1)),
                                       Deb.buildDepends = [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                                           [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                                           [Rel (BinPkgName "cdbs") Nothing Nothing],
                                                           [Rel (BinPkgName "ghc") Nothing Nothing],
                                                           [Rel (BinPkgName "ghc-prof") Nothing Nothing]],
                                       Deb.buildDependsIndep = [[Rel (BinPkgName "ghc-doc") Nothing Nothing]] }))
            newAtoms
      log = ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                              logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                              logDists = [ReleaseName {relName = "unstable"}],
                              logUrgency = "low",
                              logComments = unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                     "    files that were supposed to be installed into packages."],
                              logWho = "David Fox <dsf@seereason.com>",
                              logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}]

test3 :: String -> Test
test3 label =
    TestLabel label $
    TestCase (do deb <- execDebT (inputDebianization (Top "test-data/haskell-devscripts")) newAtoms
                 diff <- diffDebianizations testDeb2 deb
                 assertEqual label [] diff)
    where
      testDeb2 :: Atoms
      testDeb2 =
          execDebM
            (do defaultAtoms
                newDebianization log (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
                sourceFormat ~= Just Native3
                rulesHead %= (const (Just "#!/usr/bin/make -f\n# -*- makefile -*-\n\n# Uncomment this to turn on verbose mode.\n#export DH_VERBOSE=1\n\nDEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')\n\nmanpages = $(shell cat debian/manpages)\n\n%.1: %.pod\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n%.1: %\n\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@\n\n.PHONY: build\nbuild: $(manpages)\n\ninstall-stamp:\n\tdh install\n\n.PHONY: install\ninstall: install-stamp\n\nbinary-indep-stamp: install-stamp\n\tdh binary-indep\n\ttouch $@\n\n.PHONY: binary-indep\nbinary-indep: binary-indep-stamp\n\n.PHONY: binary-arch\nbinary-arch: install-stamp\n\n.PHONY: binary\nbinary: binary-indep-stamp\n\n.PHONY: clean\nclean:\n\tdh clean\n\trm -f $(manpages)\n\n\n"))
                compat ~= Just 7
                copyright ~= Just (Right "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n\n")
                control %= (\ y -> y { Deb.source = Just (SrcPkgName {unSrcPkgName = "haskell-devscripts"})
                                     , Deb.maintainer = Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                                     , Deb.uploaders = [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                                     , Deb.priority = Just Extra
                                     , Deb.section = Just (MainSection "haskell")
                                     , Deb.buildDepends = (Deb.buildDepends y) ++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]]
                                     , Deb.buildDependsIndep = (Deb.buildDependsIndep y) ++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
                                     , Deb.standardsVersion = Just (StandardsVersion 3 9 4 Nothing)
                                     , Deb.vcsFields = Set.union (Deb.vcsFields y) (Set.fromList [ Deb.VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                                                                 , Deb.VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                                     , Deb.binaryPackages = [Deb.BinaryDebDescription { Deb.package = BinPkgName {unBinPkgName = "haskell-devscripts"}
                                                                                      , Deb.architecture = All
                                                                                      , Deb.binarySection = Nothing
                                                                                      , Deb.binaryPriority = Nothing
                                                                                      , Deb.essential = False
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
                                                                                      , Deb.relations =
                                                                                          Deb.PackageRelations
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
                                                                                            , Deb.recommends = []
                                                                                            , Deb.suggests = []
                                                                                            , Deb.preDepends = []
                                                                                            , Deb.breaks = []
                                                                                            , Deb.conflicts = []
                                                                                            , Deb.provides_ = []
                                                                                            , Deb.replaces_ = []
                                                                                            , Deb.builtUsing = [] }}]}))
            newAtoms
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
    TestCase (do old <- execDebT (inputDebianization (Top "test-data/clckwrks-dot-com/output")) newAtoms
                 let log = getL changelog old
                 new <- execDebT (debianization (Top "test-data/clckwrks-dot-com/input") defaultAtoms (customize log)) newAtoms
                 diff <- diffDebianizations old ({-copyFirstLogEntry old-} new)
                 assertEqual label [] diff)
    where
      customize :: Maybe ChangeLog -> DebT IO ()
      customize log =
          do changelog ~= log
             tight
             fixRules
             doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups"
             doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))
             revision ~= Nothing
             missingDependencies += (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")
             sourceFormat ~= Just Native3
             control %= (\ y -> y {Deb.homepage = Just "http://www.clckwrks.com/"})
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
{-
      customize log = modifyM (lift . customize' log)
      customize' :: Maybe ChangeLog -> Atoms -> IO Atoms
      customize' log atoms =
          execDebT (newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))) .
          modL Lenses.control (\ y -> y {Deb.homepage = Just "http://www.clckwrks.com/"}) .
          setL Lenses.sourceFormat (Just Native3) .
          modL Lenses.missingDependencies (insert (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")) .
          setL Lenses.revision Nothing .
          execDebM (doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))) .
          execDebM (doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups") .
          fixRules .
          execDebM tight .
          setL Lenses.changelog log
-}
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules =
          makeRulesHead >>= \ rh -> rulesHead %= (\ mt -> (Just . f) (fromMaybe rh mt))
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
      tight = mapM_ (tightDependencyFixup [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-bugs-dev", BinPkgName "haskell-clckwrks-plugin-bugs-utils"),
                                           (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) serverNames

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

test5 :: String -> Test
test5 label =
    TestLabel label $
    TestCase (do old <- execDebT (inputDebianization (Top "test-data/creativeprompts/output")) newAtoms
                 let standards = Deb.standardsVersion (getL Lenses.control old)
                     level = getL Lenses.compat old
                 new <- execDebT (debianization (Top "test-data/creativeprompts/input") defaultAtoms (customize old level standards)) newAtoms
                 diff <- diffDebianizations old new
                 assertEqual label [] diff)
    where
      customize old level standards =
          do newDebianization' level standards
             changelog ~= (getL Lenses.changelog old)
             doWebsite (BinPkgName "creativeprompts-production") (theSite (BinPkgName "creativeprompts-production"))
             doServer (BinPkgName "creativeprompts-development") (theServer (BinPkgName "creativeprompts-development"))
             doBackups (BinPkgName "creativeprompts-backups") "creativeprompts-backups"
             execMap ++= ("trhsx", [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]])
             Lenses.depends +++= (BinPkgName "creativeprompts-server", anyrel (BinPkgName "markdown"))
             Lenses.description ++= (BinPkgName "creativeprompts-development",
                                     T.intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                                        , "  Testing version of the blog server, runs on port"
                                                        , "  8000 with HTML validation turned on." ])
             Lenses.description ++= (BinPkgName "creativeprompts-data",
                                     T.intercalate "\n" [ "creativeprompts.com data files"
                                                        , "  Static data files for creativeprompts.com"])
             Lenses.description ++= (BinPkgName "creativeprompts-production",
                                     T.intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                                        , "  Production version of the blog server, runs on port"
                                                        , "  9021 with HTML validation turned off." ])
             Lenses.description ++= (BinPkgName "creativeprompts-backups",
                                     T.intercalate "\n" [ "backup program for creativeprompts.com"
                                                        , "  Install this somewhere other than creativeprompts.com to run automated"
                                                        , "  backups of the database."])
             utilsPackageNames += (BinPkgName "creativeprompts-data")
             binaryArchitectures ++= (BinPkgName "creativeprompts-production", All)
             binaryArchitectures ++= (BinPkgName "creativeprompts-data", All)
             binaryArchitectures ++= (BinPkgName "creativeprompts-development", All)
             sourceFormat ~= Just Native3

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
    TestCase (do result <- readProcessWithExitCode "runhaskell" ["-isrc", "test-data/artvaluereport2/input/debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "") result)

test7 :: String -> Test
test7 label =
    TestLabel label $
    TestCase (do new <- readProcessWithExitCode "runhaskell" ["-isrc", "debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "Ignored: ./debian/cabal-debian.1\nIgnored: ./debian/cabal-debian.manpages\n") new)

test8 :: String -> Test
test8 label =
    TestLabel label $
    TestCase ( do old <- execDebT (inputDebianization (Top "test-data/artvaluereport-data/output")) newAtoms
                  log <- evalDebT (inputChangeLog (Top "test-data/artvaluereport-data/input") >> access changelog) newAtoms
                  new <- execDebT (debianization (Top "test-data/artvaluereport-data/input") defaultAtoms (customize log)) newAtoms
                  diff <- diffDebianizations old new
                  assertEqual label [] diff
             )
    where
      customize Nothing = error "Missing changelog"
      customize (Just log) =
          do Lenses.buildDeps %= Set.insert [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]]
             Lenses.control %= (\ y -> y {Deb.homepage = Just "http://artvaluereportonline.com"})
             Lenses.sourceFormat ~= Just Native3
             Lenses.changelog ~= Just log
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))

test9 :: String -> Test
test9 label =
    TestLabel label $
    TestCase ( do old <- execDebT (inputDebianization (Top "test-data/alex/output")) newAtoms
                  new <- execDebT (debianization (Top "test-data/alex/input") defaultAtoms customize) newAtoms
                  diff <- diffDebianizations old new
                  assertEqual label [] diff)
    where
      customize =
          do newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))
             mapM_ (\ name -> installData +++= (BinPkgName "alex", (name, name)))
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
             control %= (\ y -> y {Deb.homepage = Just "http://www.haskell.org/alex/"})
             sourceFormat ~= Just Native3
             debVersion ~= Just (parseDebianVersion ("3.0.2-1~hackage1" :: String))
             doExecutable (BinPkgName "alex")
                          (InstallFile {execName = "alex", destName = "alex", sourceDir = Nothing, destDir = Nothing})
             buildDeps += [[Rel (BinPkgName "alex") Nothing Nothing]]

test10 :: String -> Test
test10 label =
    TestLabel label $
    TestCase (do old <- execDebT (inputDebianization (Top "test-data/archive/output")) newAtoms
                 new <- execDebT (debianization (Top "test-data/archive/input") defaultAtoms customize) newAtoms
                 diff <- diffDebianizations old new
                 assertEqual label [] diff)
    where
      customize :: DebT IO ()
      customize =
          do utilsPackageNames += utils
             installCabalExec +++= (utils, ("seereason-darcs-backups", "/etc/cron.hourly"))
      utils = BinPkgName "seereason-darcs-backups"

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
    do old' <- debianizationFileMap (sortBinaryDebs old)
       new' <- debianizationFileMap (sortBinaryDebs new)
       return $ show $ mconcat $ map prettyChange $ filter (not . isUnchanged) $ diffMaps old' new'
    where
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
      sortBinaryDebs atoms = modL Lenses.control (\ deb -> deb {Deb.binaryPackages = sortBy (compare `on` Deb.package) (Deb.binaryPackages deb)}) atoms

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (unlines [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))

main :: IO ()
main = runTestTT tests >>= putStrLn . show

