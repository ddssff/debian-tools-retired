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
                                     dependencyHints, missingDependency, setRevision, putExecMap, putExtraDevDep, doExecutable, doWebsite, doServer)
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
tests = TestLabel "Debianization Tests" (TestList [test1, test2a, test2b, test3, test4, test5, test6])

test1 :: Test
test1 =
    TestLabel "test1" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization testEntry (Left BSD3) level standards
                 assertEqual "test1" testDeb1 deb)

test2a :: Test
test2a =
    TestLabel "test2a" $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 let deb = newDebianization testEntry (Left BSD3) level standards
                 assertEqual "test2a" expect deb)
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
                 (new, dataDir) <- debianizationWithIO "test-data/clckwrks-dot-com/input" (Flags.verbosity flags) (compilerVersion flags) (Flags.cabalFlagAssignments flags) (Flags.debMaintainer flags) (Flags.debAtoms flags) old
                 let new' = copyFirstLogEntry old (fixRules (tight new))
                 desc <- describeDebianization (Flags.buildDir flags) "test-data/clckwrks-dot-com/output" dataDir new'
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
                      (new, dataDir) <- debianizationWithIO "test-data/creativeprompts/input" (Flags.verbosity flags) (compilerVersion flags) (Flags.cabalFlagAssignments flags) (Flags.debMaintainer flags) (Flags.debAtoms flags) old
                      let new' = setArchitecture (BinPkgName "creativeprompts-development") All $
                                 setArchitecture (BinPkgName "creativeprompts-production") All $
                                 insertAtom Source (UtilsPackageName (BinPkgName "creativeprompts-data")) $
                                 copyFirstLogEntry old $
                                 new
                      desc <- describeDebianization (Flags.buildDir flags) "test-data/creativeprompts/output" dataDir new'
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
                         let new = setArchitecture (BinPkgName "creativeprompts-server") Any $
                                   setArchitecture (BinPkgName "creativeprompts-development") All $
                                   setArchitecture (BinPkgName "creativeprompts-production") All $
                                   setArchitecture (BinPkgName "creativeprompts-data") All $
                                   setArchitecture (BinPkgName "creativeprompts-backups") Any $
                                   doExecutable (BinPkgName "creativeprompts-server") (InstallFile "creativeprompts-server" Nothing Nothing "creativeprompts-server") $
                                   doExecutable (BinPkgName "creativeprompts-development") (InstallFile "creativeprompts-development" Nothing Nothing "creativeprompts-development") $
                                   doExecutable (BinPkgName "creativeprompts-production") (InstallFile "creativeprompts-production" Nothing Nothing "creativeprompts-production") $
                                   doExecutable (BinPkgName "creativeprompts-backups") (InstallFile "creativeprompts-backups" Nothing Nothing "creativeprompts-backups") $
                                   control hints $
                                   insertAtom Source (UtilsPackageName (BinPkgName "creativeprompts-data")) $
                                   -- setSourcePackageName (SrcPkgName "haskell-creativeprompts") $
                                   -- setChangelog oldLog $
                                   buildDeps hints $
                                   insertAtom Source (DHPackageDescription pkgDesc) $
                                   insertAtom Source (DHCompiler cmplr) $
                                   newDebianization entry (Left BSD3) compat' standards
                         desc <- describeDebianization "dist-ghc/build" "test-data/creativeprompts/output" (dataDirectory pkgDesc) new
                         writeFile "/tmp/bar" desc
                         assertEqual "test6" [] (diffDebianizations old new)
             )
    where
      hints = dependencyHints (error "Missing DependencyHints atom") (putExecMap "trhsx" (BinPkgName "haskell-hsx-utils") $ putExtraDevDep (BinPkgName "markdown") $ Flags.defaultFlags)
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      -- dropFirstLogEntry (deb@(Debianization {changelog = ChangeLog (_ : tl)})) = deb {changelog = ChangeLog tl}
      addMarkdownDependency :: Debianization -> Debianization
      addMarkdownDependency deb = updateBinaryPackage (BinPkgName "creativeprompts-server") addMarkdownDependency' deb
      addMarkdownDependency' :: BinaryDebDescription -> BinaryDebDescription
      addMarkdownDependency' deb = deb {relations = (relations deb) {depends = [[Rel (BinPkgName "markdown") Nothing Nothing]] ++ depends (relations deb)}}

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

{-
testDeb3 :: Debianization
testDeb3 =
    Debianization
    { sourceDebDescription =
          SourceDebDescription
          { source = SrcPkgName {unSrcPkgName = "haskell-clckwrks-dot-com"}
          , maintainer = NameAddr {nameAddr_name = Just "Jeremy Shaw", nameAddr_addr = "jeremy@n-heptane.com"}
          , uploaders = []
          , dmUploadAllowed = False
          , priority = Just Optional
          , section = Just "haskell"
          , buildDepends = [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.0" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "haskell-devscripts"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.8" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-base-dev"}) (Just (SGR (Debian.Version.parseDebianVersion ("4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-base-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-base-prof"}) (Just (SGR (Debian.Version.parseDebianVersion ("4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-base-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.13" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.14" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.13" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.14" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-theme-clckwrks-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-theme-clckwrks-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-theme-clckwrks-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-theme-clckwrks-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-containers-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-containers-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-containers-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-containers-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.0" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("7.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.0" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("7.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-hsp-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.7" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-hsp-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.8" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-hsp-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.7" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-hsp-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.8" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-mtl-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("2.0" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-mtl-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("2.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-mtl-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("2.0" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-mtl-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("2.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-text-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.11" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-text-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.12" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-text-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.11" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-text-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.12" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-dev"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.1" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-dev"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.2" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-prof"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.1" :: String)))) Nothing]
                           ,[Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-prof"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.2" :: String)))) Nothing]]
          , buildConflicts = []
          , buildDependsIndep = [[Rel (BinPkgName {unBinPkgName = "ghc-doc"}) Nothing Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-base-doc"}) (Just (SGR (Debian.Version.parseDebianVersion ("4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-doc"}) Nothing Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-base-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-doc"}) Nothing Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.13" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.14" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-bugs-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.3" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-clckwrks-plugin-media-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-containers-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.4" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-doc"}) Nothing Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-containers-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.5" :: String)))) Nothing,Rel (BinPkgName {unBinPkgName = "ghc-doc"}) Nothing Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.0" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-happstack-server-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("7.2" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-hsp-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.7" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-hsp-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.8" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-mtl-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("2.0" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-mtl-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("2.2" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-text-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.11" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-text-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.12" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-doc"}) (Just (GRE (Debian.Version.parseDebianVersion ("0.1" :: String)))) Nothing],
                                 [Rel (BinPkgName {unBinPkgName = "libghc-web-plugins-doc"}) (Just (SLT (Debian.Version.parseDebianVersion ("0.2" :: String)))) Nothing]]
          , buildConflictsIndep = []
          , standardsVersion = StandardsVersion 3 9 4 Nothing
          , vcsFields = fromList []
          , xFields = fromList []
          , binaryPackages = [BinaryDebDescription {package = BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}, architecture = Any, binarySection = Just (MainSection "misc"), binaryPriority = Nothing, essential = False, description = "clckwrks.com",
                                                    depends = [[Rel (BinPkgName {unBinPkgName = "${shlibs:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${haskell:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]],
                                                    recommends = [], suggests = [], preDepends = [], breaks = [], conflicts = [[Rel (BinPkgName {unBinPkgName = "${haskell:Conflicts}"}) Nothing Nothing]], provides = [], replaces = [], builtUsing = []}
                             ,BinaryDebDescription {package = BinPkgName {unBinPkgName = "clckwrks-dot-com-backups"}, architecture = Any, binarySection = Just (MainSection "misc"), binaryPriority = Nothing, essential = False, description = "clckwrks.com",
                                                    depends = [[Rel (BinPkgName {unBinPkgName = "${shlibs:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${haskell:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]],
                                                    recommends = [], suggests = [], preDepends = [], breaks = [], conflicts = [[Rel (BinPkgName {unBinPkgName = "${haskell:Conflicts}"}) Nothing Nothing]], provides = [], replaces = [], builtUsing = []}]
{-
          , binaryPackages = [BinaryDebDescription {package = BinPkgName {unBinPkgName = "haskell-clckwrks-dot-com-utils"}, architecture = Any, binarySection = Just (MainSection "misc"), binaryPriority = Nothing, essential = False, description = " clckwrks.com",
                                                    depends = [[Rel (BinPkgName {unBinPkgName = "${shlibs:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${haskell:Depends}"}) Nothing Nothing],[Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]],
                                                    recommends = [], suggests = [], preDepends = [], breaks = [], conflicts = [[Rel (BinPkgName {unBinPkgName = "${haskell:Conflicts}"}) Nothing Nothing]], provides = [], replaces = [], builtUsing = []}]
-}
          }
    , changelog = ChangeLog [Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.2.3-0+seereason1~precise3" :: String)), logDists = [ReleaseName {relName = "precise-seereason"}], logUrgency = "low", logComments = "  * Added -rtsopts flag because the debian packaging requires it\n  * Debianization generated by cabal-debian  * Built from hackage, revision: Debianize (Patch (Cd \"clckwrks-dot-com\" (Darcs \"http://hub.darcs.net/stepcut/clckwrks\")) \"--- old/clckwrks-dot-com.cabal\\t2012-12-20 23:31:34.000000000 -0800\\n+++ new/clckwrks-dot-com.cabal\\t2012-12-21 05:47:44.357018538 -0800\\n@@ -27,7 +27,7 @@\\n                        clckwrks-theme-clckwrks  == 0.2.*,\\n                        clckwrks-plugin-bugs     == 0.3.*,\\n                        clckwrks-plugin-media    == 0.3.*,\\n-                       containers               == 0.4.*,\\n+                       containers               >= 0.4,\\n                        happstack-server         >= 7.0 && < 7.2,\\n                        hsp                      == 0.7.*,\\n                        mtl                      >= 2.0 && < 2.2,\\n--- old/clckwrks-dot-com.cabal\\t2012-12-06 08:47:42.000000000 -0800\\n+++ new/clckwrks-dot-com.cabal\\t2012-12-06 08:49:46.069545349 -0800\\n@@ -12,7 +12,7 @@\\n \\n Flag backups\\n      Description: enable the backups executable (currently disabled by default do to wacky dependencies not on hackage)\\n-     Default: False\\n+     Default: True\\n \\n PackageHint             clckwrks-dot-com-server\\n   main-is:             Main.hs\\n--- old/debian/Debianize.hs\\t2012-12-21 12:46:13.621189606 -0800\\n+++ old/debian/Debianize.hs\\t2012-12-21 12:46:13.645189607 -0800\\n@@ -1,6 +1,6 @@\\n import Data.List (isPrefixOf)\\n import Debian.Relation (BinPkgName(..))\\n-import Distribution.Debian (Flags(..), defaultFlags, PackageHint(..), Server(..), Site(..), tightDependencyFixup)\\n+import Distribution.Debian (Flags(..), Config(..), defaultFlags, PackageHint(..), Server(..), Site(..), tightDependencyFixup)\\n import Distribution.Debian.DebHelper (DebAtom(..))\\n import Distribution.Debian.Debianize\\n import Distribution.Debian.Server (databaseDirectory)\\n@@ -12,11 +12,12 @@\\n     do jstreePath <- Clckwrks.getDataFileName \\\"jstree\\\"\\n        json2Path  <- Clckwrks.getDataFileName \\\"json2\\\"\\n        Distribution.Debian.Debianize.debianize $\\n-         defaultFlags { missingDependencies = [\\\"libghc-clckwrks-theme-clckwrks-doc\\\"]\\n+         Config { flags = defaultFlags\\n+                      { missingDependencies = [\\\"libghc-clckwrks-theme-clckwrks-doc\\\"]\\n                       , executablePackages  = map (theSite jstreePath json2Path \\\"clckwrks-dot-com-server\\\") serverNames ++ [backups]\\n                       , haddock             = True\\n-                      , revision            = \\\"\\\"\\n-                      , modifyAtoms = \\\\ atoms ->\\n+                      , revision            = \\\"\\\" }\\n+                , modifyAtoms = \\\\ atoms ->\\n                           map fixRulesHead atoms ++\\n                           concatMap\\n                             (\\\\ package -> tightDependencyFixup package\\n@@ -27,8 +28,7 @@\\n                                               (\\\"libghc-clckwrks-plugin-media-dev\\\", \\\"haskell-clckwrks-plugin-media-utils\\\"),\\n                                               (\\\"libghc-clckwrks-plugin-bugs-dev\\\", \\\"haskell-clckwrks-plugin-bugs-utils\\\"),\\n                                               (\\\"libghc-clckwrks-dev\\\", \\\"haskell-clckwrks-utils\\\")])\\n-                              serverNames\\n-                      }\\n+                              serverNames }\\n     where\\n       serverNames = map BinPkgName [\\\"clckwrks-dot-com-production\\\"] -- , \\\"clckwrks-dot-com-staging\\\", \\\"clckwrks-dot-com-development\\\"]\\n       -- Insert a line just above the debhelper.mk include\\n@@ -59,7 +59,8 @@\\n                           _                             -> Nothing\\n                     , headerMessage = \\\"Generated by clckwrks-dot-com/Setup.hs\\\"\\n                     , retry = \\\"60\\\"\\n-                    , flags = [ \\\"--http-port\\\", show portNum\\n+                    , serverFlags =\\n+                              [ \\\"--http-port\\\", show portNum\\n                               , \\\"--hide-port\\\"\\n                               , \\\"--hostname\\\", hostname\\n                               , \\\"--top\\\", databaseDirectory this\\n\")\n", logWho = "SeeReason Autobuilder <autobuilder@seereason.org>", logDate = "Fri, 21 Dec 2012 12:49:54 -0800"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.2.3" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Added -rtsopts flag because the debian packaging requires it\n  * Debianization generated by cabal-debian\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Fri, 21 Dec 2012 12:49:33 -0800"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.2.2" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Added -with-rtsopts=-I0 flag to ghc-options\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Wed, 19 Dec 2012 15:20:12 -0600"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.2.1" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * include blogHandler hack\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Tue, 11 Dec 2012 00:06:09 -0600"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.2.0" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Updated to clckwrks 0.13.*\n  * Debianization generated by cabal-debian\n  * Debianization generated by cabal-debian\n  * Debianization generated by cabal-debian\n  * Debianization generated by cabal-debian\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Wed, 28 Nov 2012 15:58:44 -0600"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.18" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Allow most recent containers\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Fri, 05 Oct 2012 18:49:33 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.17" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Updated to clckwrks 0.12\n  * Added waitForTermination\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Wed, 22 Aug 2012 12:08:33 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.16" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Now with support for page slugs\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Fri, 10 Aug 2012 15:13:24 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.15" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * updated to latest clcwrks\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Tue, 19 Jun 2012 17:48:37 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.13" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Bumped by accident, but whatever.\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Sat, 09 Jun 2012 17:52:18 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.11" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Who knows\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Tue, 05 Jun 2012 16:39:52 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.6" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Also generate depends on haskell-clckwrks-utils\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Mon, 21 May 2012 18:22:27 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.5" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Fixed debian/rules so that it generates the depends for\n    haskell-clckwrks-theme-clckwrks-utils\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Mon, 21 May 2012 16:31:20 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.4" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Added missing 'cpp-options: -DCABAL' to .cabal\n  * Added missing depends on haskell-clckwrks-theme-clckwrks-utils\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Mon, 21 May 2012 15:07:35 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.3" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Updated command-line processing to match what happstack-debianization expects\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Mon, 21 May 2012 12:39:45 -0500"}
                            ,Entry {logPackage = "haskell-clckwrks-dot-com", logVersion = (Debian.Version.parseDebianVersion ("0.1.2-1~hackage1" :: String)), logDists = [ReleaseName {relName = "unstable"}], logUrgency = "low", logComments = "  * Debianization generated by cabal-debian\n", logWho = "Jeremy Shaw <jeremy@seereason.com>", logDate = "Sun, 20 May 2012 13:50:50 -0500"}]
    , rulesHead = "#!/usr/bin/make -f\n\nDEB_CABAL_PACKAGE = clckwrks-dot-com\n\nDEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups\n\ninclude /usr/share/cdbs/1/rules/debhelper.mk\ninclude /usr/share/cdbs/1/class/hlibrary.mk\n\n\nbuild/clckwrks-dot-com-production:: build-ghc-stamp\nbuild/clckwrks-dot-com-backups:: build-ghc-stamp\nbinary-fixup/clckwrks-dot-com-production::\n\tinstall -Dp dist-ghc/build/clckwrks-dot-com-server/clckwrks-dot-com-server debian/clckwrks-dot-com-production/usr/bin/clckwrks-dot-com-production\n\nbinary-fixup/clckwrks-dot-com-production::\n\techo -n 'haskell:Depends=' >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f='haskell-clckwrks-theme-clckwrks-utils (=$${Version})' libghc-clckwrks-theme-clckwrks-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-plugin-media-utils (=$${Version})' libghc-clckwrks-plugin-media-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-plugin-bugs-utils (=$${Version})' libghc-clckwrks-plugin-bugs-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-utils (=$${Version})' libghc-clckwrks-dev >> debian/clckwrks-dot-com-production.substvars\n\techo '' >> debian/clckwrks-dot-com-production.substvars\n\techo -n 'haskell:Conflicts=' >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f='haskell-clckwrks-theme-clckwrks-utils (>>$${Version})' libghc-clckwrks-theme-clckwrks-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-plugin-media-utils (>>$${Version})' libghc-clckwrks-plugin-media-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-plugin-bugs-utils (>>$${Version})' libghc-clckwrks-plugin-bugs-dev >> debian/clckwrks-dot-com-production.substvars\n\tdpkg-query -W -f=', haskell-clckwrks-utils (>>$${Version})' libghc-clckwrks-dev >> debian/clckwrks-dot-com-production.substvars\n\techo '' >> debian/clckwrks-dot-com-production.substvars\n\n"
    , compat = 7
    , copyright = Right "Copyright (c) 2012, Jeremy Shaw\n\nAll rights reserved.\n\nRedistribution and use in source and binary forms, with or without\nmodification, are permitted provided that the following conditions are met:\n\n    * Redistributions of source code must retain the above copyright\n      notice, this list of conditions and the following disclaimer.\n\n    * Redistributions in binary form must reproduce the above\n      copyright notice, this list of conditions and the following\n      disclaimer in the documentation and/or other materials provided\n      with the distribution.\n\n    * Neither the name of Jeremy Shaw nor the names of other\n      contributors may be used to endorse or promote products derived\n      from this software without specific prior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\nLIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\nA PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\nOWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\nSPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\nLIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\nDATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\nTHEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\nOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
    , atoms = [DHInstall (BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}) "debian/cabalInstall/5e4aa8b5a821b94fbd9904695ce17ca6/clckwrks.com" "/etc/apache2/sites-available/"
              ,DHLink (BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}) "/etc/apache2/sites-available/clckwrks.com" "/etc/apache2/sites-enabled/clckwrks.com"
              ,DHInstallLogrotate (BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}) "/var/log/apache2/clckwrks-dot-com-production/access.log {\n  weekly\n  rotate 5\n  compress\n  missingok\n}\n/var/log/apache2/clckwrks-dot-com-production/error.log {\n  weekly\n  rotate 5\n  compress\n  missingok\n}\n/var/log/clckwrks-dot-com-production/access.log {\n  weekly\n  rotate 5\n  compress\n  missingok\n}\n/var/log/clckwrks-dot-com-production/app.log {\n  weekly\n  rotate 5\n  compress\n  missingok\n}\n"
              ,DHInstallInit (BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}) "#! /bin/sh -e\n\n. /lib/lsb/init-functions\n\ncase \"$1\" in\n  start)\n    test -x /usr/bin/clckwrks-dot-com-production || exit 0\n    log_begin_msg \"Starting clckwrks-dot-com-production...\"\n    mkdir -p /srv/clckwrks-dot-com-production\n    'start-stop-daemon' '--start' '-b' '--make-pidfile' '-d' '/srv/clckwrks-dot-com-production' '--exec' '/usr/bin/clckwrks-dot-com-production' '--pidfile' '/var/run/clckwrks-dot-com-production' '--' '--http-port' '9029' '--hide-port' '--hostname' 'clckwrks.com' '--top' '/srv/clckwrks-dot-com-production' '--enable-analytics' '--jquery-path' '/usr/share/javascript/jquery/' '--jqueryui-path' '/usr/share/javascript/jquery-ui/' '--jstree-path' '/usr/share/clckwrks-0.13.2/jstree' '--json2-path' '/usr/share/clckwrks-0.13.2/json2' '+RTS' '-IO' '-RTS'\n    log_end_msg $?\n    ;;\n  stop)\n    log_begin_msg \"Stopping clckwrks-dot-com-production...\"\n    'start-stop-daemon' '--stop' '--oknodo' '--retry=60' '--pidfile' '/var/run/clckwrks-dot-com-production'\n    log_end_msg $?\n    ;;\n  *)\n    log_success_msg \"Usage: ${0} {start|stop}\"\n    exit 1\nesac\n\nexit 0\n"
              ,DebWatch "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|clckwrks-dot-com-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/clckwrks-dot-com \\\n    ([\\d\\.]*\\d)/\n"
              ,DHInstall (BinPkgName {unBinPkgName = "clckwrks-dot-com-backups"}) "dist-ghc/build/clckwrks-dot-com-backups/clckwrks-dot-com-backups" "/etc/cron.hourly"
              ,DHPostInst (BinPkgName {unBinPkgName = "clckwrks-dot-com-production"}) "#!/bin/sh\n\ncase \"$1\" in\n  configure)\n    # Apache won't start if this directory doesn't exist\n    mkdir -p /var/log/apache2/clckwrks-dot-com-production\n    # Restart apache so it sees the new file in /etc/apache2/sites-enabled\n    /usr/sbin/a2enmod proxy\n    /usr/sbin/a2enmod proxy_http\n    service apache2 restart\n    ;;\nesac\n\n#DEBHELPER#\n\nexit 0\n"
              ,DebSourceFormat "3.0 (native)\n"]
{-
    , atoms = [DHInstallCabalExec (BinPkgName {unBinPkgName = "haskell-clckwrks-dot-com-utils"}) "clckwrks-dot-com-server" "usr/bin"
              ,DebRules "build/haskell-clckwrks-dot-com-utils:: build-ghc-stamp"
              ,DebSourceFormat "3.0 (native)\n"
              ,DebWatch "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|clckwrks-dot-com-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/clckwrks-dot-com \\\n    ([\\d\\.]*\\d)/\n"]
-}
    }
-}
