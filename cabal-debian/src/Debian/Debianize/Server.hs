-- | Debianization support for server programs, web server programs in particular.
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Server
       ( siteAtoms
       , serverAtoms
       , backupAtoms
       , execAtoms
       , fileAtoms
       , oldClckwrksSiteFlags
       , oldClckwrksServerFlags
       ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (pack, unlines)
import Debian.Debianize.AtomsType (DebAtomKey(..), DebAtom(..))
import Debian.Debianize.Types.PackageHints (Server(..), Site(..), InstallFile(..))
import Debian.Policy (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (BinPkgName)
import Prelude hiding (unlines)
import System.FilePath ((</>))
import System.Process (showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
siteAtoms :: DebAtomKey -> Site -> [(DebAtomKey, DebAtom)]
siteAtoms k@(Binary b) site =
    [(Binary b, DHInstallDir "/etc/apache2/sites-available"),
     (Binary b, DHLink ("/etc/apache2/sites-available/" ++ domain site) ("/etc/apache2/sites-enabled/" ++ domain site)),
     (Binary b, DHFile ("/etc/apache2/sites-available" </> domain site) apacheConfig),
     (Binary b, DHInstallDir (apacheLogDirectory b)),  -- Server won't start if log directory doesn't exist
     (Binary b, DHLogrotateStanza (unlines $
                                              [ pack (apacheAccessLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}"])),
     (Binary b, DHLogrotateStanza (unlines $
                                              [ pack (apacheErrorLog b) <> " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}" ]))] ++
    serverAtoms k (server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          unlines $
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

serverAtoms :: DebAtomKey -> Server -> Bool -> [(DebAtomKey, DebAtom)]
serverAtoms k@(Binary b) server isSite =
    [(Binary b, DHPostInst debianPostinst),
     (Binary b, DHInstallInit debianInit)] ++
    serverLogrotate b ++
    execAtoms k exec
    where
      exec = installFile server
      debianInit =
          unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
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
      startCommand = pack $ showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
      stopCommand = pack $ showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server /= "" then ["--retry=" ++ retry server ] else []
      serverOptions = serverFlags server ++ commonServerOptions
      -- Without these, happstack servers chew up CPU even when idle
      commonServerOptions = ["+RTS", "-IO", "-RTS"]

      debianPostinst =
          unlines $
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
                    [ "    service " <> pack (show (pretty b)) <> " start"
                    , "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate :: BinPkgName -> [(DebAtomKey, DebAtom)]
serverLogrotate b =
    [(Binary b, (DHLogrotateStanza . unlines $
                   [ pack (serverAccessLog b) <> " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ])),
     (Binary b, (DHLogrotateStanza . unlines $
                   [ pack (serverAppLog b) <> " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ]))]

backupAtoms :: DebAtomKey -> String -> [(DebAtomKey, DebAtom)]
backupAtoms k name =
    [(k, DHPostInst . unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])] ++
    execAtoms k (InstallFile { execName = name
                             , destName = name
                             , sourceDir = Nothing
                             , destDir = Just "/etc/cron.hourly" })

-- | Generate the atom that installs the executable.  Trickier than it should
-- be due to limitations in the dh_install script, and the fact that we don't
-- yet know the build directory path.
execAtoms :: DebAtomKey -> InstallFile -> [(DebAtomKey, DebAtom)]
execAtoms (Binary b) ifile =
    [(Source, DebRulesFragment (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp")))] ++
     fileAtoms (Binary b) ifile

fileAtoms :: DebAtomKey -> InstallFile -> [(DebAtomKey, DebAtom)]
fileAtoms k installFile =
    fileAtoms' k (sourceDir installFile) (execName installFile) (destDir installFile) (destName installFile)

fileAtoms' :: DebAtomKey -> Maybe FilePath -> String -> Maybe FilePath -> String -> [(DebAtomKey, DebAtom)]
fileAtoms' (Binary b) sourceDir execName destDir destName =
    [(Binary b, case (sourceDir, execName == destName) of
                  (Nothing, True) -> DHInstallCabalExec execName d
                  (Just s, True) -> DHInstall (s </> execName) d
                  (Nothing, False) -> DHInstallCabalExecTo execName (d </> destName)
                  (Just s, False) -> DHInstallTo (s </> execName) (d </> destName))]
    where
      d = fromMaybe "usr/bin" destDir

oldClckwrksSiteFlags :: Site -> [String]
oldClckwrksSiteFlags site =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ domain site ++ "/"
    , "--http-port", show port]
oldClckwrksServerFlags :: Server -> [String]
oldClckwrksServerFlags server =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ hostname server ++ ":" ++ show (port server) ++ "/"
    , "--http-port", show port]
