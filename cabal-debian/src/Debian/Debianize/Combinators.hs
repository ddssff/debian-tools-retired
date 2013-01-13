-- | Combinator functions for the Debianization type.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Combinators
    ( debianization
    , tightDependencyFixup
    , finalizeDebianization
    , watchAtom
    , versionInfo
    , cdbsRules
    , putCopyright
    , putLicense
    , control
    , buildDeps
    , librarySpecs
    , execAndUtilSpecs
    , installExec
    , installServer
    , installWebsite
    , addExtraLibDependencies
    , filterMissing
    , setSourcePackageName
    , setSourcePriority
    , setSourceSection
    , setSourceBinaries
    , setChangelog
    , modifyBinaryDeb
    , setArchitecture
    , setBinaryPriority
    , setBinarySection
    , setDescription
    ) where

import Debug.Trace

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.List as List (nub, intercalate, intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid ((<>), mempty)
import Data.Set as Set (Set, difference, union, fromList, null, insert, toList)
import Data.Text as Text (Text, pack, intercalate, unpack, unlines)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Dependencies (debianBuildDeps, debianBuildDepsIndep)
import Debian.Debianize.Server (execAtoms, serverAtoms, siteAtoms)
import Debian.Debianize.Types.Atoms (noProfilingLibrary, noDocumentationLibrary, DebAtomKey(..), DebAtom(..), HasAtoms(getAtoms, putAtoms), insertAtom, foldAtoms, insertAtoms', utilsPackageName, packageDescription, compiler)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), newBinaryDebDescription,
                                                       PackageRelations(..))
import Debian.Debianize.Types.Dependencies (DependencyHints (binaryPackageDeps, extraLibMap, extraDevDeps, binaryPackageConflicts, epochMap,
                                                             revision, debVersion, missingDependencies), debianName)
import Debian.Debianize.Types.PackageHints (PackageHints, PackageHint(..), InstallFile(..), Server(..), Site(..))
import Debian.Debianize.Types.PackageType (PackageType(Development, Profiling, Documentation, Exec, Utilities, Cabal, Source'))
import Debian.Debianize.Utility (trim)
import Debian.Policy (StandardsVersion, PackagePriority(Optional), PackageArchitectures(Any, All, Names), Section(..))
import qualified Debian.Relation as D
import Debian.Relation (BinPkgName, SrcPkgName(..), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription as Cabal (PackageDescription({-package, library, homepage, synopsis, description, maintainer, dataFiles, executables, author, pkgUrl-}),
                                                 BuildInfo(buildable {-, extraLibs-}), Executable(exeName, buildInfo) {-, allBuildInfo-})
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
debianization hints atoms execs pkgDesc cmplr date copyright' maint standards oldDeb =
    watchAtom (pkgName . Cabal.package $ pkgDesc)  $
    putCopyright copyright' $
    putStandards standards $
    filterMissing (missingDependencies hints) $
    versionInfo hints maint date $
    addExtraLibDependencies hints $
    control hints execs $
    cdbsRules hints (Cabal.package pkgDesc) $
    -- Do we want to replace the atoms in the old deb, or add these?
    -- Or should we delete even more information from the original,
    -- keeping only the changelog?  Probably the latter.  So this is
    -- somewhat wrong.
    insertAtom Source (DHPackageDescription pkgDesc) $
    insertAtom Source (DHCompiler cmplr) $
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
finalizeDebianization  :: FilePath -> FilePath -> Debianization -> Debianization
finalizeDebianization build datadir deb =
    finalizeAtoms . mergeRules $ deb
    where
      finalizeAtoms deb' = foldAtoms finalizeAtom (putAtoms mempty deb') (getAtoms deb')
      finalizeAtom (Binary b) (DHApacheSite domain' logdir text) deb' =
          insertAtom (Binary b) (DHLink ("/etc/apache2/sites-available/" ++ domain') ("/etc/apache2/sites-enabled/" ++ domain')) $
          insertAtom (Binary b) (DHInstallDir logdir) $ -- Server won't start if log directory doesn't exist
          insertAtom (Binary b) (DHFile ("/etc/apache2/sites-available" </> domain') text) $ deb'
      finalizeAtom (Binary pkg) (DHInstallCabalExec name dst) deb' = insertAtom (Binary pkg) (DHInstall (build </> name </> name) dst) deb'
      finalizeAtom (Binary _) (DHInstallCabalExecTo {}) deb' = deb' -- This becomes a rule in rulesAtomText
      finalizeAtom (Binary p) (DHInstallData s d) deb' = insertAtom (Binary p) (DHInstallTo s (datadir </> makeRelative "/" d)) deb'
      finalizeAtom (Binary p) (DHFile path s) deb' =
          let (destDir', destName') = splitFileName path
              tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
              tmpPath = tmpDir </> destName' in
          insertAtom Source (DHIntermediate tmpPath s) (insertAtom (Binary p) (DHInstall tmpPath destDir') deb')
      finalizeAtom k x deb' = insertAtom k x deb'
      mergeRules :: Debianization -> Debianization
      mergeRules x = x { rulesHead = foldAtoms mergeRulesAtom (rulesHead x) (getAtoms x) }
      mergeRulesAtom Source (DebRulesFragment x) text = text <> "\n" <> x
      mergeRulesAtom (Binary p) (DHInstallTo s d) text =
          text <> "\n" <>
               unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                       , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]
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
versionInfo :: DependencyHints -> NameAddr -> String -> Debianization -> Debianization
versionInfo hints debianMaintainer date deb@(Debianization {changelog = ChangeLog oldEntries}) =
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
      pkgId = Cabal.package pkgDesc
      pkgDesc = packageDescription (error "versionInfo: no PackageDescription") deb

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

control :: DependencyHints -> PackageHints -> Debianization -> Debianization
control dependencyHints packageHints deb =
    execAndUtilSpecs dependencyHints packageHints describe $
    librarySpecs dependencyHints describe $
    buildDeps dependencyHints $
    setSourcePriority (Just Optional) $
    setSourceSection (Just (MainSection "haskell")) $
    setSourceBinaries [] $
    deb
    where
      describe = debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc)
      pkgDesc = packageDescription (error "control: no PackageDescription") deb

buildDeps :: DependencyHints -> Debianization -> Debianization
buildDeps hints deb =
    deb { sourceDebDescription = (sourceDebDescription deb) { Debian.buildDepends = debianBuildDeps hints cmplr deb
                                                            , buildDependsIndep = debianBuildDepsIndep hints deb } }
    where
      cmplr = compiler (error "debianBuildDeps: no Compiler") deb

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
librarySpecs hints describe deb =
    deb { sourceDebDescription =
            (sourceDebDescription deb)
              { binaryPackages =
                    maybe []
                          (const ([librarySpec hints Any Development (Cabal.package pkgDesc) describe] ++
                                  if noProfilingLibrary deb then [] else [librarySpec hints Any Profiling (Cabal.package pkgDesc) describe] ++
                                  if noDocumentationLibrary deb then [] else [docSpecsParagraph hints (Cabal.package pkgDesc) describe]))
                          (Cabal.library pkgDesc) ++
                    binaryPackages (sourceDebDescription deb) } }
    where
      pkgDesc = packageDescription (error "librarySpecs: no PackageDescription") deb

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: DependencyHints -> Debianization -> Debianization
addExtraLibDependencies hints deb =
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
      pkgDesc = packageDescription (error "addExtraLibDependencies: no PackageDescription") deb

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

data FileInfo
    = DataFile FilePath
    -- ^ A file that is going to be installed into the package's data
    -- file directory, /usr/share/packagename-version/.
    | CabalExecutable String
    -- ^ A Cabal Executable record, which appears in dist/build/name/name,
    -- and is typically installed into /usr/bin.
    deriving (Eq, Ord, Show)

-- | Generate the control file sections and other debhelper atoms for
-- the executable and utility packages.
execAndUtilSpecs :: DependencyHints -> PackageHints -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
execAndUtilSpecs dependencyHints packageHints describe deb =
    makeUtilsPackage dependencyHints packageHints describe $
    applyPackageHints dependencyHints describe packageHints $ deb

t1 x = trace ("t1: " ++ show x) x
t2 x = trace ("t2: " ++ show x) x
t3 x = {- trace ("t3: " ++ show x) -} x
t4 x = trace ("t4: " ++ show x) x
t5 x = trace ("t5: " ++ show x) x

-- Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: DependencyHints -> PackageHints -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
makeUtilsPackage dependencyHints packageHints describe deb =
    case Set.difference (t1 available) (t2 installed) of
      s | Set.null s -> deb
      s ->      let p = t4 (fromMaybe (debianName dependencyHints Utilities (Cabal.package pkgDesc)) (t5 (utilsPackageName deb))) in
                makeUtilsAtoms p s $
                deb { sourceDebDescription =
                          (sourceDebDescription deb)
                          { binaryPackages =
                               binaryPackages (sourceDebDescription deb) ++
                               [BinaryDebDescription
                                { Debian.package = p
                                , architecture = Any
                                , binarySection = Just (MainSection "misc")
                                , binaryPriority = Nothing
                                , essential = False
                                , Debian.description = describe Utilities (Cabal.package pkgDesc)
                                , relations =
                                    PackageRelations
                                    { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                                                extraDeps (binaryPackageDeps dependencyHints) p
                                    , recommends = []
                                    , suggests = []
                                    , preDepends = []
                                    , breaks = []
                                    , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts dependencyHints) p
                                    , provides = []
                                    , replaces = []
                                    , builtUsing = []
                                    }
                                }]} }
    where
      pkgDesc = packageDescription (error "makeUtilsPackage: no PackageDescription") deb
      available = Set.union (Set.fromList (map DataFile (Cabal.dataFiles pkgDesc)))
                            (Set.fromList (map (CabalExecutable . exeName) (Cabal.executables pkgDesc)))
      installed = foldAtoms cabalFile mempty (t3 (getAtoms deb))
      cabalFile _ (DHInstallCabalExec name _) xs = Set.insert (CabalExecutable name) xs
      cabalFile _ (DHInstallCabalExecTo name _) xs = Set.insert (CabalExecutable name) xs
      cabalFile _ (DHInstallData path _) xs = Set.insert (DataFile path) xs
      cabalFile _ _ xs = xs
      makeUtilsAtoms :: BinPkgName -> Set FileInfo -> Debianization -> Debianization
      makeUtilsAtoms p s deb' =
          case (bundledExecutables packageHints pkgDesc, Cabal.dataFiles pkgDesc) of
            ([], []) -> deb'
            _ -> insertAtom Source (DebRulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) $
                 insertAtoms' (Binary p) (map fileInfoAtom (Set.toList s)) $
                 deb'
      fileInfoAtom (DataFile path) = DHInstall path (takeDirectory ("usr/share" </> display (Cabal.package pkgDesc) </> path))
      fileInfoAtom (CabalExecutable name) = DHInstallCabalExec name "usr/bin"

applyPackageHint :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> PackageHint -> Debianization -> Debianization
applyPackageHint dependencyHints describe (SiteHint b x) deb = installWebsite dependencyHints describe b x deb
applyPackageHint dependencyHints describe (ServerHint b x) deb = installServer dependencyHints describe b x deb
applyPackageHint dependencyHints describe (InstallFileHint b x) deb = installExec dependencyHints describe b x deb

installWebsite :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> BinPkgName -> Site -> Debianization -> Debianization
installWebsite dependencyHints describe b site deb =
    siteAtoms b site $ cabalExecBinaryPackage b dependencyHints describe $ deb

installServer :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> BinPkgName -> Server -> Debianization -> Debianization
installServer dependencyHints describe b server deb =
    serverAtoms b server False $ cabalExecBinaryPackage b dependencyHints describe $ deb

installExec :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> BinPkgName -> InstallFile -> Debianization -> Debianization
installExec dependencyHints describe b e deb =
    execAtoms b e $ cabalExecBinaryPackage b dependencyHints describe $ deb

{-
applyPackageHint dependencyHints pkgDesc describe packageHint deb =
    packageHintAtoms packageHint $ packageHintPackages $ deb
    where
      packageHintPackages :: Debianization -> Debianization
      packageHintPackages deb' =
          deb' { sourceDebDescription =
                    (sourceDebDescription deb')
                    { binaryPackages = concatMap packageHintDeb packageHints ++ binaryPackages (sourceDebDescription deb') } }

      packageHintAtoms (SiteHint debName site) xs = siteAtoms debName site xs
      packageHintAtoms (ServerHint debName server') xs = serverAtoms debName server' False xs
      packageHintAtoms (InstallFileHint debName e) xs = execAtoms debName e xs
      -- If this package hint implies a new binary deb, create it
      packageHintDeb :: PackageHint -> [BinaryDebDescription]
      packageHintDeb (SiteHint debName site) = packageHintDeb (ServerHint debName (server site))
      packageHintDeb (ServerHint debName server') = packageHintDeb (InstallFileHint debName (installFile server'))
-}

cabalExecBinaryPackage :: BinPkgName -> DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> Debianization -> Debianization
cabalExecBinaryPackage b dependencyHints describe deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = bin : binaryPackages (sourceDebDescription deb)}}
    where
      bin = BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe Exec (Cabal.package pkgDesc)
            , relations =
                PackageRelations
                { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            extraDeps (binaryPackageDeps dependencyHints) b
                , recommends = []
                , suggests = []
                , preDepends = []
                , breaks = []
                , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts dependencyHints) b
                , provides = []
                , replaces = []
                , builtUsing = []
                }
            }
      pkgDesc = packageDescription (error "cabalExecBinaryPackage: no PackageDescription") deb

applyPackageHints :: DependencyHints -> (PackageType -> PackageIdentifier -> Text) -> PackageHints -> Debianization -> Debianization
applyPackageHints dependencyHints describe packageHints deb =
    foldr (applyPackageHint dependencyHints describe) deb packageHints

-- | The list of executables without a corresponding cabal package to put them into
bundledExecutables :: [PackageHint] -> PackageDescription -> [Executable]
bundledExecutables packageHints pkgDesc =
    filter nopackage (filter (buildable . buildInfo) (Cabal.executables pkgDesc))
    where
      nopackage p = not (elem (exeName p) (mapMaybe execNameOfHint packageHints))
      execNameOfHint (InstallFileHint _ e) = Just (execName e)
      execNameOfHint (ServerHint _ s) = Just (execName (installFile s))
      execNameOfHint (SiteHint _ s) = Just (execName (installFile (server s)))

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

setSourcePriority :: Maybe PackagePriority -> Debianization -> Debianization
setSourcePriority x deb = deb {sourceDebDescription = (sourceDebDescription deb) {priority = x}}

setSourceSection :: Maybe Section -> Debianization -> Debianization
setSourceSection x deb = deb {sourceDebDescription = (sourceDebDescription deb) {section = x}}

setSourceBinaries :: [BinaryDebDescription] -> Debianization -> Debianization
setSourceBinaries xs deb = deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = xs}}

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
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = xs''}}
    where
      -- scan the binary debs and apply f to the target, recording whether we found it
      (xs', found) = foldl g ([], False) (binaryPackages (sourceDebDescription deb))
      g (xs, found) x = if Debian.package x == bin then (f x : xs, True) else (x : xs, found)
      -- If we didn't find the target package create it and apply f
      xs'' = if found then xs' else f (newBinaryDebDescription bin (Names [])) : xs'
