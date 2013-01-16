-- | Combinator functions for the Debianization type.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Combinators
    ( versionInfo
    , cdbsRules
    , putCopyright
    , putStandards
    , putLicense
    , control
    , buildDeps
    , describe
    , extraDeps
    , addExtraLibDependencies
    , filterMissing
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

import Data.List as List (nub, intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, intercalate, unlines)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Dependencies (debianBuildDeps, debianBuildDepsIndep, debianName)
import Debian.Debianize.Atoms (packageDescription, dependencyHints)
import Debian.Debianize.Types.Atoms (DebAtomKey(..), DebAtom(..), HasAtoms, foldAtoms)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), newBinaryDebDescription,
                                                       PackageRelations(..))
import Debian.Debianize.Types.Dependencies (DependencyHints (extraLibMap, epochMap, revision, debVersion))
import Debian.Debianize.Types.PackageType (PackageType(Development, Profiling, Documentation, Exec, Utilities, Cabal, Source'))
import Debian.Debianize.Utility (trim)
import Debian.Policy (StandardsVersion, PackagePriority(Optional), PackageArchitectures(Names), Section(..))
import qualified Debian.Relation as D
import Debian.Relation (BinPkgName, SrcPkgName(..), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Distribution.License (License)
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

{-
sourceFormatAtom :: SourceFormat -> Debianization -> Debianization
sourceFormatAtom format deb =
    insertAtom Source (DebSourceFormat format) deb
-}

-- | Set the debianization's version info - everything that goes into
-- the new changelog entry, source package name, exact debian version,
-- log comments, maintainer name, revision date.
versionInfo :: NameAddr -> String -> Debianization -> Debianization
versionInfo debianMaintainer date deb@(Debianization {changelog = ChangeLog oldEntries}) =
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
      -- Get the source package name, either from the SourcePackageName
      -- atom or construct it from the cabal package name.
      sourceName :: SrcPkgName
      sourceName =
          fromMaybe (debianName deb Source' pkgId) (foldAtoms from Nothing deb)
          where
            from Source (SourcePackageName s) (Just t) | s /= t = error $ "Conflicting source package names: " ++ show s ++ " vs. " ++ show t
            from Source (SourcePackageName s) _ = Just s
            from _ _ x = x
      merge :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
      merge old new =
          old { logComments = logComments old ++ logComments new
              , logDate = date }
      debinfo = maybe (Right (epoch, revision (dependencyHints deb))) Left (debVersion (dependencyHints deb))
      epoch = Map.lookup (pkgName pkgId) (epochMap (dependencyHints deb))
      pkgId = Cabal.package pkgDesc
      pkgDesc = fromMaybe (error "versionInfo: no PackageDescription") $ packageDescription deb

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
cdbsRules :: PackageIdentifier -> Debianization -> Debianization
cdbsRules pkgId deb =
    deb { rulesHead =
              unlines ["#!/usr/bin/make -f",
                       "",
                       "DEB_CABAL_PACKAGE = " <> pack (show (pretty (debianName deb Cabal pkgId :: BinPkgName))),
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

control :: Debianization -> Debianization
control deb =
    buildDeps $
    setSourcePriority (Just Optional) $
    setSourceSection (Just (MainSection "haskell")) $
    setSourceBinaries [] $
    deb

describe :: HasAtoms atoms => atoms -> PackageType -> PackageIdentifier -> Text
describe atoms =
    debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc)
    where
      pkgDesc = fromMaybe (error "describe") $ packageDescription atoms

buildDeps :: Debianization -> Debianization
buildDeps deb =
    deb { sourceDebDescription = (sourceDebDescription deb) { Debian.buildDepends = debianBuildDeps deb
                                                            , buildDependsIndep = debianBuildDepsIndep deb } }

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: Debianization -> Debianization
addExtraLibDependencies deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = map f (binaryPackages (sourceDebDescription deb))}}
    where
      f :: BinaryDebDescription -> BinaryDebDescription
      f bin
          | debianName deb Development (Cabal.package pkgDesc) == Debian.package bin
              = bin { relations = g (relations bin) }
      f bin = bin
      g :: Debian.PackageRelations -> Debian.PackageRelations
      g rels = rels { depends = depends rels ++
                                map anyrel' (concatMap (\ cab -> fromMaybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] (Map.lookup cab (extraLibMap (dependencyHints deb))))
                                                       (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)) }
      pkgDesc = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ packageDescription deb

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

extraDeps :: [(D.BinPkgName, D.BinPkgName)] -> D.BinPkgName -> [[D.Relation]]
extraDeps deps p =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map (mkDep . snd) pairs
    where mkDep name = [D.Rel name Nothing Nothing]

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
      g (xs, found') x = if Debian.package x == bin then (f x : xs, True) else (x : xs, found')
      -- If we didn't find the target package create it and apply f
      xs'' = if found then xs' else f (newBinaryDebDescription bin (Names [])) : xs'
