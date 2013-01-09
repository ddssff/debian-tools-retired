-- | Combinator functions for the Debianization type.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Combinators
    ( debianization
    , tightDependencyFixup
    , deSugarDebianization
    , watchAtom
    , versionInfo
    , cdbsRules
    , putCopyright
    , putLicense
    , control
    , buildDeps
    , librarySpecs
    , execAndUtilSpecs
    , addExtraLibDependencies
    , filterMissing
    , setSourcePackageName
    , setChangelog
    , modifyBinaryDeb
    , setArchitecture
    , setBinaryPriority
    , setBinarySection
    , setDescription
    ) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.List as List (nub, intercalate, intersperse, find)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid ((<>), mempty)
import Data.Set (Set)
import Data.Text as Text (Text, pack, intercalate, unpack, unlines)
import Data.Version (Version)
import qualified Debian.Relation as D
import Debian.Cabal.Dependencies (DependencyHints (binaryPackageDeps, extraLibMap, extraDevDeps, binaryPackageConflicts, epochMap, revision, debVersion, missingDependencies),
                                  debianName, debianBuildDeps, debianBuildDepsIndep)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Paths (apacheLogDirectory)
import Debian.Debianize.Server (execAtoms, serverAtoms, siteAtoms)
import Debian.Debianize.Types.Atoms (noProfilingLibrary, noDocumentationLibrary, DebAtomKey(..), DebAtom(..), HasAtoms(getAtoms, putAtoms), insertAtom, foldAtoms, insertAtoms', utilsPackageName)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), newBinaryDebDescription,
                                                       PackageRelations(..), PackageType(Development, Profiling, Documentation, Exec, Utilities, Cabal, Source'))
import Debian.Debianize.Types.PackageHints (PackageHints, PackageHint(..), InstallFile(..), Server(..), Site(..))
import Debian.Debianize.Utility (trim)
import Debian.Policy (StandardsVersion, PackagePriority(Optional), PackageArchitectures(Any, All, Names), Section(..))
import Debian.Relation (BinPkgName, SrcPkgName(..), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription as Cabal (PackageDescription(package, library, {-homepage,-} synopsis, {-description,-} maintainer, dataFiles, executables, author, pkgUrl),
                                                 BuildInfo(buildable, extraLibs), Executable(exeName, buildInfo), allBuildInfo)
import qualified Distribution.PackageDescription as Cabal
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines)
import System.FilePath ((</>), takeDirectory, makeRelative, splitFileName)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

debianization :: DependencyHints
              -> Map DebAtomKey (Set DebAtom)
              -> PackageHints
              -> PackageDescription  -- ^ info from the .cabal file
              -> Compiler            -- ^ compiler details
              -> String              -- ^ current date
              -> Text                -- ^ copyright
              -> NameAddr            -- ^ maintainer
              -> StandardsVersion
              -> Debianization       -- ^ Existing debianization
              -> Debianization       -- ^ New debianization
debianization hints atoms execs pkgDesc compiler date copyright' maint standards oldDeb =
    watchAtom (pkgName . Cabal.package $ pkgDesc)  $
    putCopyright copyright' $
    putStandards standards $
    filterMissing (missingDependencies hints) $
    versionInfo hints (Cabal.package pkgDesc) maint date $
    addExtraLibDependencies hints pkgDesc $
    control hints execs compiler pkgDesc $
    cdbsRules hints (Cabal.package pkgDesc) $
    -- Do we want to replace the atoms in the old deb, or add these?
    -- Or should we delete even more information from the original,
    -- keeping only the changelog?  Probably the latter.  So this is
    -- somewhat wrong.
    putAtoms atoms $
    oldDeb

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: [(BinPkgName, BinPkgName)] -> BinPkgName -> Debianization -> Debianization
tightDependencyFixup [] _ deb = deb
tightDependencyFixup pairs p deb =
    insertAtom Source atom deb
    where
      atom = DebRulesFragment
              (unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

-- | Eliminate atoms that can be expressed as simpler ones now that we
-- know the build directory, and merge the text of all the atoms that
-- contribute to the rules file into the rulesHead field.  The atoms
-- are not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
deSugarDebianization  :: FilePath -> FilePath -> Debianization -> Debianization
deSugarDebianization build datadir deb =
    deSugarAtoms . mergeRules $ deb
    where
      deSugarAtoms deb' = foldAtoms deSugarAtom (putAtoms mempty deb') (getAtoms deb')
      deSugarAtom (Binary b) (DHApacheSite domain' logdir text) deb' =
          insertAtom (Binary b) (DHLink ("/etc/apache2/sites-available/" ++ domain') ("/etc/apache2/sites-enabled/" ++ domain')) $
          insertAtom (Binary b) (DHInstallDir logdir) $ -- Server won't start if log directory doesn't exist
          insertAtom (Binary b) (DHFile ("/etc/apache2/sites-available" </> domain') text) $ deb'
      deSugarAtom (Binary b) x@(DHInstallLogrotate _) deb' = insertAtom (Binary b) x $ insertAtom (Binary b) (DHInstallDir (apacheLogDirectory b)) deb'
      deSugarAtom (Binary pkg) (DHInstallCabalExec name dst) deb' = insertAtom (Binary pkg) (DHInstall (build </> name </> name) dst) deb'
      deSugarAtom (Binary _) (DHInstallCabalExecTo {}) deb' = deb' -- This becomes a rule in rulesAtomText
      deSugarAtom (Binary p) (DHInstallData s d) deb' = insertAtom (Binary p) (DHInstallTo s (datadir </> makeRelative "/" d)) deb'
      deSugarAtom (Binary p) (DHFile path s) deb' =
          let (destDir', destName') = splitFileName path
              tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
              tmpPath = tmpDir </> destName' in
          insertAtom Source (DHIntermediate tmpPath s) (insertAtom (Binary p) (DHInstall tmpPath destDir') deb')
      deSugarAtom k x deb' = insertAtom k x deb'
      mergeRules :: Debianization -> Debianization
      mergeRules x = x { rulesHead = foldAtoms mergeRulesAtom (rulesHead x) (getAtoms x) }
      mergeRulesAtom Source (DebRulesFragment x) text = text <> "\n" <> x
      mergeRulesAtom (Binary p) (DHInstallTo s d) text =
          text <> "\n" <>
               unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                       , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]
      mergeRulesAtom _ (DHInstallData _ _) _ = error "DHInstallData should have been turned into a DHInstallTo"
      mergeRulesAtom (Binary p) (DHInstallCabalExecTo n d) text =
          text <> "\n" <>
               unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                       , "\tinstall -Dp " <> pack (build </> n </> n) <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]
      mergeRulesAtom _ _ text = text

watchAtom :: PackageName -> Debianization -> Debianization
watchAtom (PackageName pkgname) deb =
    insertAtom Source atom deb
    where
      atom =
          DebWatch . pack $
            "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
            "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
            " \\\n    ([\\d\\.]*\\d)/\n"

{-
sourceFormatAtom :: SourceFormat -> Debianization -> Debianization
sourceFormatAtom format deb =
    insertAtom Source (DebSourceFormat format) deb
-}

-- | Set the debianization's version info - everything that goes into
-- the new changelog entry, source package name, exact debian version,
-- log comments, maintainer name, revision date.
versionInfo :: DependencyHints -> PackageIdentifier -> NameAddr -> String -> Debianization -> Debianization
versionInfo hints pkgId debianMaintainer date deb@(Debianization {changelog = ChangeLog oldEntries}) =
    deb { changelog = newLog
        , sourceDebDescription =
            (sourceDebDescription deb)
              { source = sourceName
              , Debian.maintainer = debianMaintainer }}
    where
      newLog =
          case dropWhile (\ entry -> logVersion entry > logVersion newEntry) oldEntries of
            -- If the new package version number matches the old, merge the new and existing log entries
            entry@(Entry {logVersion = d}) : older | d == logVersion newEntry -> ChangeLog (merge entry newEntry : older)
            -- Otherwise prepend the new entry
            entries -> ChangeLog (newEntry : entries)
      newEntry = Entry { logPackage = show (pretty sourceName)
                       , logVersion = convertVersion debinfo (pkgVersion pkgId)
                       , logDists = [parseReleaseName "unstable"]
                       , logUrgency = "low"
                       , logComments = "  * Debianization generated by cabal-debian\n"
                       , logWho = show (pretty debianMaintainer)
                       , logDate = date }
      sourceName :: SrcPkgName
      sourceName = debianName hints Source' pkgId
      merge :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
      merge old new =
          old { logComments = logComments old ++ logComments new
              , logDate = date }
      debinfo = maybe (Right (epoch, revision hints)) Left (debVersion hints)
      epoch = Map.lookup (pkgName pkgId) (epochMap hints)

-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
convertVersion :: Either DebianVersion (Maybe Int, String) -> Version -> DebianVersion
convertVersion debinfo cabalVersion =
    case debinfo of
      Left override | override >= parseDebianVersion (show (pretty cabalVersion)) -> override
      Left override -> error ("Version from --deb-version (" ++ show (pretty override) ++
                              ") is older than hackage version (" ++ show (pretty cabalVersion) ++
                              "), maybe you need to unpin this package?")
      Right (debianEpoch, debianRevision) ->
          buildDebianVersion debianEpoch
                             (show (pretty cabalVersion))
                             (Just debianRevision)

-- | Generate the head of the debian/rules file.
cdbsRules :: DependencyHints -> PackageIdentifier -> Debianization -> Debianization
cdbsRules hints pkgId deb =
    deb { rulesHead =
              unlines ["#!/usr/bin/make -f",
                       "",
                       "DEB_CABAL_PACKAGE = " <> pack (show (pretty (debianName hints Cabal pkgId :: BinPkgName))),
                       "",
                       "include /usr/share/cdbs/1/rules/debhelper.mk",
                       "include /usr/share/cdbs/1/class/hlibrary.mk" ] }

putCopyright :: Text -> Debianization -> Debianization
putCopyright text deb = deb {copyright = Right text}

putStandards :: StandardsVersion -> Debianization -> Debianization
putStandards x deb = deb {sourceDebDescription = (sourceDebDescription deb) {standardsVersion = x}}

putLicense :: License -> Debianization -> Debianization
putLicense license deb = deb {copyright = Left license}

-- | The control file consists of a Source paragraph and one or more
-- Binary paragraphs, each one representing a binary package to be
-- produced.  If the package contains a library we usually want dev,
-- prof, and doc packages.

control :: DependencyHints -> PackageHints -> Compiler -> PackageDescription -> Debianization -> Debianization
control dependencyHints packageHints compiler pkgDesc deb =
    execAndUtilSpecs dependencyHints packageHints pkgDesc describe $
    librarySpecs dependencyHints pkgDesc describe $
    buildDeps dependencyHints compiler pkgDesc $
    deb { sourceDebDescription =
            (sourceDebDescription deb)
              { priority = Just Optional
              , section = Just (pack "haskell")
              , binaryPackages = {- trace ("binaryPackages: " ++ show (binaryPackages (sourceDebDescription deb))) -} [] } }
    where
      describe = debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc)

buildDeps :: DependencyHints -> Compiler -> PackageDescription -> Debianization -> Debianization
buildDeps hints compiler pkgDesc deb =
    deb { sourceDebDescription = (sourceDebDescription deb) { Debian.buildDepends = debianBuildDeps hints compiler pkgDesc deb
                                                            , buildDependsIndep = debianBuildDepsIndep hints compiler pkgDesc deb } }

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: DependencyHints -> PackageDescription -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
librarySpecs hints pkgDesc describe deb =
    deb { sourceDebDescription =
            (sourceDebDescription deb)
              { binaryPackages =
                    maybe []
                          (const ([librarySpec hints Any Development (Cabal.package pkgDesc) describe] ++
                                  if noProfilingLibrary deb then [] else [librarySpec hints Any Profiling (Cabal.package pkgDesc) describe] ++
                                  if noDocumentationLibrary deb then [] else [docSpecsParagraph hints (Cabal.package pkgDesc) describe]))
                          (Cabal.library pkgDesc) ++
                    binaryPackages (sourceDebDescription deb) } }

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: DependencyHints -> PackageDescription -> Debianization -> Debianization
addExtraLibDependencies hints pkgDesc deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = map f (binaryPackages (sourceDebDescription deb))}}
    where
      f :: BinaryDebDescription -> BinaryDebDescription
      f bin
          | debianName hints Development (Cabal.package pkgDesc) == Debian.package bin
              = bin { relations = g (relations bin) }
      f bin = bin
      g :: Debian.PackageRelations -> Debian.PackageRelations 
      g rels = rels { depends = depends rels ++
                                map anyrel' (concatMap (\ cab -> fromMaybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] (Map.lookup cab (extraLibMap hints)))
                                                       (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)) }

librarySpec :: DependencyHints -> PackageArchitectures -> PackageType -> PackageIdentifier -> (PackageType -> PackageIdentifier -> Text) -> BinaryDebDescription
librarySpec hints arch typ pkgId describe =
          BinaryDebDescription
            { Debian.package = debianName hints typ pkgId
            , architecture = arch
            , binarySection = Nothing
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe typ pkgId
            , relations =
                PackageRelations
                { depends = (if typ == Development then [anyrel "${shlibs:Depends}"] ++ map anyrel' (extraDevDeps hints) else []) ++
                            ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                             extraDeps (binaryPackageDeps hints) (debianName hints typ pkgId))
                , recommends = [anyrel "${haskell:Recommends}"]
                , suggests = [anyrel "${haskell:Suggests}"]
                , preDepends = []
                , breaks = []
                , Debian.conflicts = [anyrel "${haskell:Conflicts}"]
                , provides = [anyrel "${haskell:Provides}"]
                , replaces = []
                , builtUsing = []
                }
            }

docSpecsParagraph :: DependencyHints -> PackageIdentifier -> (PackageType -> PackageIdentifier -> Text) -> BinaryDebDescription
docSpecsParagraph hints pkgId describe =
          BinaryDebDescription
            { Debian.package = debianName hints Documentation pkgId
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe Documentation pkgId
            , relations =
                PackageRelations
                { depends = [anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            extraDeps (binaryPackageDeps hints) (debianName hints Documentation pkgId)
                , recommends = [anyrel "${haskell:Recommends}"]
                , Debian.suggests = [anyrel "${haskell:Suggests}"]
                , preDepends = []
                , breaks = []
                , Debian.conflicts = [anyrel "${haskell:Conflicts}"]
                , provides = []
                , replaces = []
                , builtUsing = []
                }
            }

-- | Generate the control file sections and other debhelper atoms for
-- the executable and utility packages.
execAndUtilSpecs :: DependencyHints -> PackageHints -> PackageDescription -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
execAndUtilSpecs dependencyHints packageHints pkgDesc describe deb =
    flip (foldr packageHintAtoms) packageHints $
    makeUtilsAtoms dependencyHints packageHints pkgDesc $
    deb { sourceDebDescription = (sourceDebDescription deb) { binaryPackages = map applyPackageHints (newBinaryPackageList (sourceDebDescription deb)) } }
    where
      newBinaryPackageList src=
          concatMap packageHintDeb packageHints ++
          makeUtilsPackage (Cabal.dataFiles pkgDesc) ++
          binaryPackages src

      -- If this package hint implies a new binary deb, create it
      packageHintDeb :: PackageHint -> [BinaryDebDescription]
      packageHintDeb (SiteHint debName site) = packageHintDeb (ServerHint debName (server site))
      packageHintDeb (ServerHint debName server') = packageHintDeb (InstallFileHint debName (installFile server'))
      packageHintDeb (InstallFileHint debName _p) =
          [BinaryDebDescription
             { Debian.package = debName
             , architecture = Any
             , binarySection = Just (MainSection "misc")
             , binaryPriority = Nothing
             , essential = False
             , Debian.description = describe Exec (Cabal.package pkgDesc)
             , relations =
                 PackageRelations
                 { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps (binaryPackageDeps dependencyHints) debName
                 , recommends = []
                 , suggests = []
                 , preDepends = []
                 , breaks = []
                 , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts dependencyHints) debName
                 , provides = []
                 , replaces = []
                 , builtUsing = []
                 }
             }]

      packageHintAtoms (SiteHint debName site) xs =
          siteAtoms debName (sourceDir (installFile (server site))) (execName (installFile (server site))) (destDir (installFile (server site))) (destName (installFile (server site)))
                    (retry (server site)) (port (server site)) (serverFlags (server site))
                    (domain site) (serverAdmin site) xs
      packageHintAtoms (ServerHint debName server') xs =
          serverAtoms debName (sourceDir (installFile server')) (execName (installFile server')) (destDir (installFile server')) (destName (installFile server'))
                      (retry server') (port server') (serverFlags server') False xs
      packageHintAtoms (InstallFileHint debName e) xs =
          execAtoms debName (sourceDir e) (execName e) (destDir e) (destName e) xs
      packageHintAtoms _ xs = xs

      -- Create a package to hold any executables and data files not
      -- assigned to some other package.
      makeUtilsPackage :: [FilePath] -> [BinaryDebDescription]
      makeUtilsPackage dataFiles' =
          case (bundledExecutables packageHints pkgDesc, dataFiles') of
            ([], []) ->
                []
            _ ->
                let p = fromMaybe (debianName dependencyHints Utilities (Cabal.package pkgDesc)) (utilsPackageName deb) in
                [BinaryDebDescription
                    { Debian.package = p
                    , architecture = Any
                    , binarySection = Just (MainSection "misc")
                    , binaryPriority = Nothing
                    , essential = False
                    , Debian.description = describe Utilities (Cabal.package pkgDesc)
                    , relations =
                        PackageRelations
                        { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++ extraDeps (binaryPackageDeps dependencyHints) p
                        , recommends = []
                        , suggests = []
                        , preDepends = []
                        , breaks = []
                        , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts dependencyHints) p
                        , provides = []
                        , replaces = []
                        , builtUsing = []
                        }
                    }]

      applyPackageHints bin = foldr applyPackageHint bin packageHints
      applyPackageHint (InstallFileHint {}) bin = bin
      applyPackageHint (ServerHint {}) bin = bin
      applyPackageHint (SiteHint {}) bin = bin

makeUtilsAtoms :: DependencyHints -> PackageHints -> PackageDescription -> Debianization -> Debianization
makeUtilsAtoms dependencyHints packageHints pkgDesc deb =
    case (bundledExecutables packageHints pkgDesc, Cabal.dataFiles pkgDesc) of
      ([], []) -> deb
      _ -> let p = debianName dependencyHints Utilities (Cabal.package pkgDesc)
               p' = show (pretty p)
               c = Cabal.package pkgDesc
               c' = display c in
           insertAtom Source (DebRulesFragment (pack ("build" </> p' ++ ":: build-ghc-stamp\n"))) $
           insertAtoms' (Binary p)
             (map (\ e -> DHInstallCabalExec (exeName e) "usr/bin") (bundledExecutables packageHints pkgDesc) ++
              map (\ f -> DHInstall f (takeDirectory ("usr/share" </> c' </> f))) (Cabal.dataFiles pkgDesc))
             deb

-- | The list of executables without a corresponding cabal package to put them into
bundledExecutables :: [PackageHint] -> PackageDescription -> [Executable]
bundledExecutables packageHints pkgDesc =
    filter nopackage (filter (buildable . buildInfo) (Cabal.executables pkgDesc))
    where
      nopackage p = not (elem (exeName p) (mapMaybe execNameOfHint packageHints))
      execNameOfHint (InstallFileHint _ e) = Just (execName e)
      execNameOfHint (ServerHint _ s) = Just (execName (installFile s))
      execNameOfHint (SiteHint _ s) = Just (execName (installFile (server s)))
      execNameOfHint _ = Nothing

debianDescription :: String -> String -> String -> String -> String -> PackageType -> PackageIdentifier -> Text
debianDescription synopsis' description' author' maintainer' url typ pkgId =
    debianDescriptionBase synopsis' description' author' maintainer' url <> "\n" <>
    case typ of
      Profiling ->
          Text.intercalate "\n"
                  [" .",
                   " This package provides a library for the Haskell programming language, compiled",
                   " for profiling.  See http:///www.haskell.org/ for more information on Haskell."]
      Development ->
          Text.intercalate "\n"
                  [" .",
                   " This package provides a library for the Haskell programming language.",
                   " See http:///www.haskell.org/ for more information on Haskell."]
      Documentation ->
          Text.intercalate "\n"
                  [" .",
                   " This package provides the documentation for a library for the Haskell",
                   " programming language.",
                   " See http:///www.haskell.org/ for more information on Haskell." ]
      Exec ->
          Text.intercalate "\n"
                  [" .",
                   " An executable built from the " <> pack (display (pkgName pkgId)) <> " package."]
{-    ServerPackage ->
          Text.intercalate "\n"
                  [" .",
                   " A server built from the " <> pack (display (pkgName pkgId)) <> " package."] -}
      Utilities ->
          Text.intercalate "\n"
                  [" .",
                   " Utility files associated with the " <> pack (display (pkgName pkgId)) <> " package."]
      x -> error $ "Unexpected library package name suffix: " ++ show x

-- | The Cabal package has one synopsis and one description field
-- for the entire package, while in a Debian package there is a
-- description field (of which the first line is synopsis) in
-- each binary package.  So the cabal description forms the base
-- of the debian description, each of which is amended.
debianDescriptionBase :: String -> String -> String -> String -> String -> Text
debianDescriptionBase synopsis' description' author' maintainer' url =
    (pack . unwords . words $ synopsis') <>
    case description' of
      "" -> ""
      text ->
          let text' = text ++ "\n" ++
                      list "" ("\n Author: " ++) author' ++
                      list "" ("\n Upstream-Maintainer: " ++) maintainer' ++
                      list "" ("\n Url: " ++) url in
          "\n " <> (pack . trim . List.intercalate "\n " . map addDot . lines $ text')
    where
      addDot line = if all (flip elem " \t") line then "." else line
      list :: b -> ([a] -> b) -> [a] -> b
      list d f l = case l of [] -> d; _ -> f l

{-
b :: Debian.Debianize.Types.Config.Executable -> D.BinPkgName
b p = debName p
-}

extraDeps :: [(D.BinPkgName, D.BinPkgName)] -> D.BinPkgName -> [[D.Relation]]
extraDeps deps p =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map (mkDep . snd) pairs
    where mkDep name = [D.Rel name Nothing Nothing]

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

filterMissing :: [BinPkgName] -> Debianization -> Debianization
filterMissing missing deb =
    deb {sourceDebDescription = e (sourceDebDescription deb)}
    where
      e src = src { Debian.buildDepends = f (Debian.buildDepends src)
                  , Debian.buildDependsIndep = f (Debian.buildDependsIndep src)
                  , binaryPackages = map g (binaryPackages src) }
      f rels = filter (/= []) (map (filter (\ (Rel name _ _) -> not (elem name missing))) rels)
      g bin = bin { relations = h (relations bin) }
      h rels = PackageRelations { depends = f (depends rels)
                                , recommends = f (recommends rels)
                                , suggests = f (suggests rels)
                                , preDepends = f (preDepends rels)
                                , breaks = f (breaks rels)
                                , conflicts = f (conflicts rels)
                                , provides = f (provides rels)
                                , replaces = f (replaces rels)
                                , builtUsing = f (builtUsing rels) }

setSourcePackageName :: SrcPkgName -> Debianization -> Debianization
setSourcePackageName name@(SrcPkgName string) deb@(Debianization {changelog = ChangeLog (newest : older)}) =
    deb { sourceDebDescription = (sourceDebDescription deb) {source = name}
        , changelog = ChangeLog (newest {logPackage = string} : older)}

setChangelog :: ChangeLog -> Debianization -> Debianization
setChangelog log' deb = deb { changelog = log' }

setArchitecture :: BinPkgName -> PackageArchitectures -> Debianization -> Debianization
setArchitecture bin x deb = modifyBinaryDeb bin (\ b -> b {architecture = x}) deb

setBinaryPriority :: BinPkgName -> Maybe PackagePriority -> Debianization -> Debianization
setBinaryPriority bin x deb = modifyBinaryDeb bin (\ b -> b {binaryPriority = x}) deb

setBinarySection :: BinPkgName -> Maybe Section -> Debianization -> Debianization
setBinarySection bin x deb = modifyBinaryDeb bin (\ b -> b {binarySection = x}) deb

setDescription :: BinPkgName -> Text -> Debianization -> Debianization
setDescription bin x deb = modifyBinaryDeb bin (\ b -> b {Debian.description = x}) deb

modifyBinaryDeb :: BinPkgName -> (BinaryDebDescription -> BinaryDebDescription) -> Debianization -> Debianization
modifyBinaryDeb bin f deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = f b : binaryPackages (sourceDebDescription deb)}}
    where
      b = case find ((== bin) . Debian.package) (binaryPackages (sourceDebDescription deb)) of
            Nothing -> newBinaryDebDescription bin (Names [])
            Just x -> x
