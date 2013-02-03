-- | Combinator functions for the Debianization type.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Combinators
    ( versionInfo
    , cdbsRules
    , putStandards
    , buildDeps
    , describe
    -- , extraDeps
    , addExtraLibDependencies
    , setSourceBinaries
    , oldFilterMissing
    ) where

import Data.Lens.Lazy (setL, getL)
import Data.List as List (nub, intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text as Text (Text, pack, intercalate, unlines)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.AtomsClass (HasAtoms(rulesHead, packageDescription))
import Debian.Debianize.AtomsType (Atoms, revision, debVersion, sourcePackageName,
                                   extraLibMap, epochMap, changeLog, modifyChangeLog, sourceDebDescription, setSourceDebDescription)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), PackageType(..))
import Debian.Debianize.Dependencies (debianBuildDeps, debianBuildDepsIndep, debianName)
-- import Debian.Debianize.Types.PackageType (PackageType(Development, Profiling, Documentation, Exec, Utilities, Cabal, Source'))
import Debian.Debianize.Utility (trim)
import Debian.Policy (StandardsVersion)
import qualified Debian.Relation as D
import Debian.Relation (BinPkgName, SrcPkgName(..), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | Set the debianization's version info - everything that goes into
-- the new changelog entry, source package name, exact debian version,
-- log comments, maintainer name, revision date.
versionInfo :: NameAddr -> String -> Atoms -> Atoms
versionInfo debianMaintainer date deb =
    modifyChangeLog (const (Just newLog)) $
    setSourceDebDescription ((sourceDebDescription deb)
                             { source = Just sourceName
                             , Debian.maintainer = Just debianMaintainer }) deb
    where
      ChangeLog oldEntries = changeLog deb
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
      sourceName = maybe (debianName deb Source' pkgId) SrcPkgName (sourcePackageName deb)
      merge :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
      merge old new =
          old { logComments = logComments old ++ logComments new
              , logDate = date }
      debinfo = maybe (Right (epoch, revision deb)) Left (debVersion deb)
      epoch = Map.lookup (pkgName pkgId) (epochMap deb)
      pkgId = Cabal.package pkgDesc
      pkgDesc = fromMaybe (error "versionInfo: no PackageDescription") $ getL packageDescription deb

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
cdbsRules :: PackageIdentifier -> Atoms -> Atoms
cdbsRules pkgId deb =
    setL rulesHead
         (Just . unlines $
          ["#!/usr/bin/make -f",
           "",
           "DEB_CABAL_PACKAGE = " <> pack (show (pretty (debianName deb Cabal pkgId :: BinPkgName))),
           "",
           "include /usr/share/cdbs/1/rules/debhelper.mk",
           "include /usr/share/cdbs/1/class/hlibrary.mk" ])
         deb

putStandards :: StandardsVersion -> Atoms -> Atoms
putStandards x deb = setSourceDebDescription ((sourceDebDescription deb) {standardsVersion = Just x}) deb

describe :: Atoms -> PackageType -> PackageIdentifier -> Text
describe atoms typ ident =
    debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc) typ ident
    where
      pkgDesc = fromMaybe (error $ "describe " ++ show ident) $ getL packageDescription atoms

buildDeps :: Atoms -> Atoms
buildDeps deb =
    setSourceDebDescription ((sourceDebDescription deb) { Debian.buildDepends = debianBuildDeps deb
                                                        , buildDependsIndep = debianBuildDepsIndep deb }) deb

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: Atoms -> Atoms
addExtraLibDependencies deb =
    setSourceDebDescription ((sourceDebDescription deb) {binaryPackages = map f (binaryPackages (sourceDebDescription deb))}) deb
    where
      f :: BinaryDebDescription -> BinaryDebDescription
      f bin
          | debianName deb Development (Cabal.package pkgDesc) == Debian.package bin
              = bin { relations = g (relations bin) }
      f bin = bin
      g :: Debian.PackageRelations -> Debian.PackageRelations
      g rels = rels { depends = depends rels ++
                                map anyrel' (concatMap (\ cab -> maybe [D.BinPkgName ("lib" ++ cab ++ "-dev")] Set.toList (Map.lookup cab (extraLibMap deb)))
                                                       (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)) }
      pkgDesc = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ getL packageDescription deb

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
extraDeps :: [(D.BinPkgName, D.Relation)] -> D.BinPkgName -> [[D.Relation]]
extraDeps deps p =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map ((: []) . snd) pairs
-}

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

oldFilterMissing :: [BinPkgName] -> Atoms -> Atoms
oldFilterMissing missing deb =
    setSourceDebDescription (e (sourceDebDescription deb)) deb
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

setSourceBinaries :: [BinaryDebDescription] -> Atoms -> Atoms
setSourceBinaries xs deb = setSourceDebDescription ((sourceDebDescription deb) {binaryPackages = xs}) deb
