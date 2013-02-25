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
    , siteAtoms
    , serverAtoms
    , backupAtoms
    , execAtoms
    ) where

import Data.Lens.Lazy (getL, setL, modL)
import Data.List as List (map, intersperse, intercalate)
import Data.Map as Map (Map, fromList, insertWith)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Set as Set (insert, union, singleton)
import Data.Text as Text (Text, pack, unlines, intercalate)
import Data.Version (Version(Version))
import Debian.Debianize.Atoms as Atoms ()
import Debian.Debianize.Atoms as Atoms
    (Atoms, packageDescription, rulesFragments, website, serverInfo, link, backups, executable,
     install, installTo, installCabalExecTo, file, installDir, logrotateStanza, postInst,
     installInit, installCabalExec, rulesFragments, packageDescription, executable,
     serverInfo, website, backups, depends, epochMap, versionSplits)
import Debian.Debianize.ControlFile as Debian (PackageType(..))
import Debian.Debianize.Types (InstallFile(..), Server(..), Site(..), VersionSplits(..))
import Debian.Debianize.Utility (trim)
import Debian.Orphans ()
import Debian.Policy (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Package (PackageIdentifier(..), PackageName(PackageName))
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines, log, map)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

showCommand :: String -> [String] -> String
showCommand cmd args =
    unwords (map translate (cmd : args))

translate :: String -> String
translate str =
    '"' : foldr escape "\"" str
    where
      escape '"' = showString "\\\""
      escape c = showChar c

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
    Map.fromList [(PackageName "HaXml", 1), (PackageName "HTTP", 1)]

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
doExecutable bin x deb = modL executable (Map.insertWith (\ a b -> error $ "doExecutable: " ++ show (a, b)) bin x) deb

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a server.
doServer :: BinPkgName -> Server -> Atoms -> Atoms
doServer bin x deb = modL serverInfo (Map.insertWith (\ a b -> error $ "doServer: " ++ show (a, b)) bin x) deb

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a web site.
doWebsite :: BinPkgName -> Site -> Atoms -> Atoms
doWebsite bin x deb = modL website (Map.insertWith (\ a b -> error $ "doWebsite: " ++ show (a, b)) bin x) deb

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
          "\n " <> (pack . trim . List.intercalate "\n " . List.map addDot . lines $ text')
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

siteAtoms :: BinPkgName -> Site -> Atoms -> Atoms
siteAtoms b site =
    modL installDir (Map.insertWith Set.union b (singleton "/etc/apache2/sites-available")) .
    modL link (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available/" ++ domain site, "/etc/apache2/sites-enabled/" ++ domain site))) .
    modL file (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available" </> domain site, apacheConfig))) .
    modL installDir (Map.insertWith Set.union b (singleton (apacheLogDirectory b))) .
    modL logrotateStanza (Map.insertWith Set.union b (singleton (Text.unlines $
                                                                 [ pack (apacheAccessLog b) <> " {"
                                                                 , "  weekly"
                                                                 , "  rotate 5"
                                                                 , "  compress"
                                                                 , "  missingok"
                                                                 , "}"]))) .
    modL logrotateStanza (Map.insertWith Set.union b (singleton (Text.unlines $
                                                                 [ pack (apacheErrorLog b) <> " {"
                                                                 , "  weekly"
                                                                 , "  rotate 5"
                                                                 , "  compress"
                                                                 , "  missingok"
                                                                 , "}" ]))) .
    serverAtoms b (server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          Text.unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " <> pack (serverAdmin site)
                   , "    ServerName www." <> pack (domain site)
                   , "    ServerAlias " <> pack (domain site)
                   , ""
                   , "    ErrorLog " <> pack (apacheErrorLog b)
                   , "    CustomLog " <> pack (apacheAccessLog b) <> " combined"
                   , ""
                   , "    ProxyRequests Off"
                   , "    AllowEncodedSlashes NoDecode"
                   , ""
                   , "    <Proxy *>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                Deny from all"
                   , "                #Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    <Proxy http://127.0.0.1:" <> port' <> "/*>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                #Deny from all"
                   , "                Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    SetEnv proxy-sendcl 1"
                   , ""
                   , "    ProxyPass / http://127.0.0.1:" <> port' <> "/ nocanon"
                   , "    ProxyPassReverse / http://127.0.0.1:" <> port' <> "/"
                   , "</VirtualHost>" ]
      port' = pack (show (port (server site)))

serverAtoms :: BinPkgName -> Server -> Bool -> Atoms -> Atoms
serverAtoms b server isSite =
    modL postInst (insertWith (error "serverAtoms") b debianPostinst) .
    modL installInit (Map.insertWith (error "serverAtoms") b debianInit) .
    serverLogrotate' b .
    execAtoms b exec
    where
      exec = installFile server
      debianInit =
          Text.unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , "test -f /etc/default/" <> pack (destName exec) <> " && source /etc/default/" <> pack (destName exec)
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" <> pack (destName exec) <> " || exit 0"
                   , "    log_begin_msg \"Starting " <> pack (destName exec) <> "...\""
                   , "    mkdir -p " <> pack (databaseDirectory b)
                   , "    " <> startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " <> pack (destName exec) <> "...\""
                   , "    " <> stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = pack $ showCommand "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
      stopCommand = pack $ showCommand "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server /= "" then ["--retry=" ++ retry server ] else []
      serverOptions = serverFlags server ++ commonServerOptions
      -- Without these, happstack servers chew up CPU even when idle
      commonServerOptions = ["+RTS", "-IO", "-RTS"]

      debianPostinst =
          Text.unlines $
                   ([ "#!/bin/sh"
                    , ""
                    , "case \"$1\" in"
                    , "  configure)" ] ++
                    (if isSite
                     then [ "    # Apache won't start if this directory doesn't exist"
                          , "    mkdir -p " <> pack (apacheLogDirectory b)
                          , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
                          , "    /usr/sbin/a2enmod proxy"
                          , "    /usr/sbin/a2enmod proxy_http"
                          , "    service apache2 restart" ]
                     else []) ++
                    [ -- This gets done by the #DEBHELPER# code below.
                      {- "    service " <> pack (show (pretty b)) <> " start", -}
                      "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate' :: BinPkgName -> Atoms -> Atoms
serverLogrotate' b =
    modL logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]))) .
    modL logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])))

backupAtoms :: BinPkgName -> String -> Atoms -> Atoms
backupAtoms b name =
    modL postInst (insertWith (error "backupAtoms") b
                 (Text.unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])) .
    execAtoms b (InstallFile { execName = name
                              , destName = name
                              , sourceDir = Nothing
                              , destDir = Just "/etc/cron.hourly" })

execAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
execAtoms b ifile r =
    modL rulesFragments (Set.insert (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) .
    fileAtoms b ifile $
    r

fileAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
fileAtoms b installFile r =
    fileAtoms' b (sourceDir installFile) (execName installFile) (destDir installFile) (destName installFile) r

fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> Atoms -> Atoms
fileAtoms' b sourceDir execName destDir destName r =
    case (sourceDir, execName == destName) of
      (Nothing, True) -> modL installCabalExec (insertWith Set.union b (singleton (execName, d))) r
      (Just s, True) -> modL install (insertWith Set.union b (singleton (s </> execName, d))) r
      (Nothing, False) -> modL installCabalExecTo (insertWith Set.union b (singleton (execName, (d </> destName)))) r
      (Just s, False) -> modL installTo (insertWith Set.union b (singleton (s </> execName, d </> destName))) r
    where
      d = fromMaybe "usr/bin" destDir
