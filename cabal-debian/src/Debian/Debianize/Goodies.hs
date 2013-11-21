-- | Things that seem like they could be clients of this library, but
-- are instead included as part of the library.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Goodies
    ( tightDependencyFixup
    , doServer
    , doWebsite
    , doBackups
    , doExecutable
    , describe
    , watchAtom
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    , siteAtoms
    , serverAtoms
    , backupAtoms
    , execAtoms
    , makeRulesHead
    ) where

import Control.Monad.State (get)
import Data.Lens.Lazy (getL, modL)
import Data.List as List (map, intersperse, intercalate)
import Data.Map as Map (insertWith)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (insert, union, singleton)
import Data.Text as Text (Text, pack, unlines, intercalate)
import qualified Debian.Debianize.Lenses as Lenses
    (packageDescription, rulesFragments,
     install, installTo, installCabalExecTo, logrotateStanza, postInst,
     installInit, installCabalExec, rulesFragments, packageDescription)
import Debian.Debianize.ControlFile as Debian (PackageType(..))
import Debian.Debianize.Files2 (debianName)
import Debian.Debianize.Monad (Atoms, DebT, execDebM, executable, rulesFragment, installDir, link, file, logrotateStanza,
                               serverInfo, website, backups, depends)
import Debian.Debianize.Types (InstallFile(..), Server(..), Site(..))
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

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: Monad m => [(BinPkgName, BinPkgName)] -> BinPkgName -> DebT m ()
tightDependencyFixup [] _ = return ()
tightDependencyFixup pairs p =
    rulesFragment
          (Text.unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
    where
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

-- | Add a debian binary package to the debianization containing a cabal executable file.
doExecutable :: Monad m => BinPkgName -> InstallFile -> DebT m ()
doExecutable = executable

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a server.
doServer :: Monad m => BinPkgName -> Server -> DebT m ()
doServer = serverInfo

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a web site.
doWebsite :: Monad m => BinPkgName -> Site -> DebT m ()
doWebsite = website

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a backup script.
doBackups :: Monad m => BinPkgName -> String -> DebT m ()
doBackups bin s =
    do backups bin s
       depends bin (Rel (BinPkgName "anacron") Nothing Nothing)

describe :: Monad m => PackageType -> PackageIdentifier -> DebT m Text
describe typ pkgId =
    do atoms <- get
       let pkgDesc = fromMaybe (error $ "describe " ++ show pkgId) $ getL Lenses.packageDescription atoms
       return $ debianDescription (Cabal.synopsis pkgDesc) (Cabal.description pkgDesc) (Cabal.author pkgDesc) (Cabal.maintainer pkgDesc) (Cabal.pkgUrl pkgDesc)
    where
      debianDescription :: String -> String -> String -> String -> String -> Text
      debianDescription synopsis' description' author' maintainer' url =
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
           "-$1.tar.gz|\" \\\n    http://hackage.haskell.org/package/" ++ pkgname ++
           " \\\n    ([\\d\\.]*\\d)/\n"

-- FIXME - use Atoms
siteAtoms :: BinPkgName -> Site -> Atoms -> Atoms
siteAtoms b site =
    execDebM
      (do installDir b "/etc/apache2/sites-available"
          link b ("/etc/apache2/sites-available/" ++ domain site) ("/etc/apache2/sites-enabled/" ++ domain site)
          file b ("/etc/apache2/sites-available" </> domain site) apacheConfig
          installDir b (apacheLogDirectory b)
          logrotateStanza b (Text.unlines $ [ pack (apacheAccessLog b) <> " {"
                                            , "  weekly"
                                            , "  rotate 5"
                                            , "  compress"
                                            , "  missingok"
                                            , "}"])
          logrotateStanza b (Text.unlines $ [ pack (apacheErrorLog b) <> " {"
                                            , "  weekly"
                                            , "  rotate 5"
                                            , "  compress"
                                            , "  missingok"
                                            , "}" ])) .
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

-- FIXME - use Atoms
serverAtoms :: BinPkgName -> Server -> Bool -> Atoms -> Atoms
serverAtoms b server' isSite =
    modL Lenses.postInst (insertWith (\ old new -> if old /= new then error ("serverAtoms: " ++ show old ++ " -> " ++ show new) else old) b debianPostinst) .
    modL Lenses.installInit (Map.insertWith (\ old new -> if old /= new then error ("serverAtoms: " ++ show old ++ " -> " ++ show new) else old) b debianInit) .
    serverLogrotate' b .
    execAtoms b exec
    where
      exec = installFile server'
      debianInit =
          Text.unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , "test -f /etc/default/" <> pack (destName exec) <> " && . /etc/default/" <> pack (destName exec)
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
      startCommand = pack $ showCommand "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverFlags server')
      stopCommand = pack $ showCommand "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server' /= "" then ["--retry=" ++ retry server' ] else []

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
-- FIXME - use Atoms
serverLogrotate' :: BinPkgName -> Atoms -> Atoms
serverLogrotate' b =
    modL Lenses.logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]))) .
    modL Lenses.logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])))

-- FIXME - use Atoms
backupAtoms :: BinPkgName -> String -> Atoms -> Atoms
backupAtoms b name =
    modL Lenses.postInst (insertWith (\ old new -> if old /= new then error $ "backupAtoms: " ++ show old ++ " -> " ++ show new else old) b
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

-- FIXME - use Atoms
execAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
execAtoms b ifile r =
    modL Lenses.rulesFragments (Set.insert (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) .
    fileAtoms b ifile $
    r

-- FIXME - use Atoms
fileAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
fileAtoms b installFile' r =
    fileAtoms' b (sourceDir installFile') (execName installFile') (destDir installFile') (destName installFile') r

-- FIXME - use Atoms
fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> Atoms -> Atoms
fileAtoms' b sourceDir' execName' destDir' destName' r =
    case (sourceDir', execName' == destName') of
      (Nothing, True) -> modL Lenses.installCabalExec (insertWith Set.union b (singleton (execName', d))) r
      (Just s, True) -> modL Lenses.install (insertWith Set.union b (singleton (s </> execName', d))) r
      (Nothing, False) -> modL Lenses.installCabalExecTo (insertWith Set.union b (singleton (execName', (d </> destName')))) r
      (Just s, False) -> modL Lenses.installTo (insertWith Set.union b (singleton (s </> execName', d </> destName'))) r
    where
      d = fromMaybe "usr/bin" destDir'

-- | Build a suitable value for the head of the rules file.
makeRulesHead :: Monad m => DebT m Text
makeRulesHead =
    do pkgDesc <- get >>= return . getL Lenses.packageDescription
       ls <- maybe (return [])
                   (\ p -> do b <- debianName Cabal (Cabal.package p)
                              return ["DEB_CABAL_PACKAGE = " <> pack (show (pretty (b :: BinPkgName))), ""])
                   pkgDesc
       return $
          Text.unlines $
            ["#!/usr/bin/make -f", ""] ++
            ls ++
            ["include /usr/share/cdbs/1/rules/debhelper.mk",
             "include /usr/share/cdbs/1/class/hlibrary.mk"]
