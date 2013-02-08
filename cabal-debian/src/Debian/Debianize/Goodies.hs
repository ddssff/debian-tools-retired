-- | Things that seem like they could be clients of this library, but
-- are instead included as part of the library.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Goodies
    ( defaultAtoms
    , tightDependencyFixup
    , doServer
    , doWebsite
    , doBackups
    , doExecutable
    , debianDescription
    , describe
    , watchAtom
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    ) where

import Data.Lens.Lazy (getL, setL, modL)
import Data.List as List (map, intersperse, intercalate)
import Data.Map as Map (Map, fromList, insertWith)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Set as Set (insert, union, singleton)
import Data.Text as Text (Text, pack, unlines, intercalate)
import Data.Version (Version(Version))
import Debian.Debianize.Atoms as Atoms (Atoms, rulesFragments, packageDescription, executable, serverInfo, website, backups, depends, epochMap, versionSplits)
import Debian.Debianize.ControlFile as Debian (PackageType(..))
import Debian.Debianize.Types (InstallFile, Server(..), Site(..), VersionSplits(..))
import Debian.Debianize.Utility (trim)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Package (PackageIdentifier(..), PackageName(PackageName))
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines, log)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | This may not look like a goodie, but it incorporates knowledge
-- about the debian repository - what the epoch number of HaXml is,
-- the fact that the debian package name of parsec changed, etc.
defaultAtoms :: Atoms
defaultAtoms =
    setL epochMap knownEpochMappings $
    setL versionSplits knownVersionSplits $
    mempty

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.
knownVersionSplits :: [VersionSplits]
knownVersionSplits =
    [ VersionSplits {
        packageName = PackageName "parsec"
      , oldestPackage = PackageName "parsec2"
      , splits = [(Version [3] [], PackageName "parsec3")] }
    , VersionSplits {
        packageName = PackageName "QuickCheck"
      , oldestPackage = PackageName "quickcheck1"
      , splits = [(Version [2] [], PackageName "quickcheck2")] }
    ]

-- | We should always call this, just as we should always apply
-- knownVersionSplits.
knownEpochMappings :: Map PackageName Int
knownEpochMappings =
    Map.fromList [(PackageName "HaXml", 1)]

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: [(BinPkgName, BinPkgName)] -> BinPkgName -> Atoms -> Atoms
tightDependencyFixup [] _ deb = deb
tightDependencyFixup pairs p deb =
    modL rulesFragments
             (Set.insert
              (Text.unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))) deb
    where
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

-- | Add a debian binary package to the debianization containing a cabal executable file.
doExecutable :: BinPkgName -> InstallFile -> Atoms -> Atoms
doExecutable bin x deb = modL executable (Map.insertWith (error "executable") bin x) deb

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a server.
doServer :: BinPkgName -> Server -> Atoms -> Atoms
doServer bin x deb = modL serverInfo (Map.insertWith (error "serverInfo") bin x) deb

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a web site.
doWebsite :: BinPkgName -> Site -> Atoms -> Atoms
doWebsite bin x deb = modL website (Map.insertWith (error "website") bin x) deb

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a backup script.
doBackups :: BinPkgName -> String -> Atoms -> Atoms
doBackups bin s deb =
    modL backups (Map.insertWith (error "backups") bin s) $
    modL Atoms.depends (Map.insertWith union bin (singleton (Rel (BinPkgName "anacron") Nothing Nothing))) $
    deb

describe :: Atoms -> PackageType -> PackageIdentifier -> Text
describe atoms typ ident =
    debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc) typ ident
    where
      pkgDesc = fromMaybe (error $ "describe " ++ show ident) $ getL packageDescription atoms

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

oldClckwrksSiteFlags :: Site -> [String]
oldClckwrksSiteFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ domain x ++ "/"
    , "--http-port", show port]
oldClckwrksServerFlags :: Server -> [String]
oldClckwrksServerFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ hostname x ++ ":" ++ show (port x) ++ "/"
    , "--http-port", show port]

watchAtom :: PackageName -> Text
watchAtom (PackageName pkgname) =
    pack $ "version=3\nopts=\"downloadurlmangle=s|archive/([\\w\\d_-]+)/([\\d\\.]+)/|archive/$1/$2/$1-$2.tar.gz|,\\\nfilenamemangle=s|(.*)/$|" ++ pkgname ++
           "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/packages/archive/" ++ pkgname ++
           " \\\n    ([\\d\\.]*\\d)/\n"
