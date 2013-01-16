-- | Debianization support for server programs, web server programs in particular.
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Server
       ( siteAtoms
       , serverAtoms
       , execAtoms
       , fileAtoms
       , oldClckwrksSiteFlags
       , oldClckwrksServerFlags
       ) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Debian.Debianize.Types.Atoms (DebAtomKey(..), DebAtom(..))
import Debian.Debianize.Types.PackageHints (Server(..), Site(..), InstallFile(..))
import Debian.Policy (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (BinPkgName)
import System.FilePath ((</>))
import System.Process (showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
siteAtoms :: DebAtomKey -> Site -> [(DebAtomKey, DebAtom)]
siteAtoms k@(Binary b) site =
    [(Binary b, DHLink ("/etc/apache2/sites-available/" ++ domain site) ("/etc/apache2/sites-enabled/" ++ domain site)),
     (Binary b, DHInstallDir (apacheLogDirectory b)),  -- Server won't start if log directory doesn't exist
     (Binary b, DHFile ("/etc/apache2/sites-available" </> domain site) apacheConfig),
     (Binary b, DHLogrotateStanza (pack . unlines $
                                              [ apacheAccessLog b ++ " {"
                                              , "  weekly"
                                              , "  rotate 5"
                                              , "  compress"
                                              , "  missingok"
                                              , "}"])),
     (Binary b, DHLogrotateStanza (pack . unlines $
                                              [ apacheErrorLog b ++ " {"
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
          pack . unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " ++ serverAdmin site
                   , "    ServerName www." ++ domain site
                   , "    ServerAlias " ++ domain site
                   , ""
                   , "    ErrorLog " ++ apacheErrorLog b
                   , "    CustomLog " ++ apacheAccessLog b ++ " combined"
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
                   , "    <Proxy http://127.0.0.1:" ++ show (port (server site)) ++ "/*>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                #Deny from all"
                   , "                Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    SetEnv proxy-sendcl 1"
                   , ""
                   , "    ProxyPass / http://127.0.0.1:" ++ show (port (server site)) ++ "/ nocanon"
                   , "    ProxyPassReverse / http://127.0.0.1:" ++ show (port (server site)) ++ "/"
                   , "</VirtualHost>" ]

serverAtoms :: DebAtomKey -> Server -> Bool -> [(DebAtomKey, DebAtom)]
serverAtoms k@(Binary b) server isSite =
    [(Binary b, DHPostInst debianPostinst),
     (Binary b, DHInstallInit debianInit)] ++
    serverLogrotate b ++
    execAtoms k exec
    where
      exec = installFile server
      debianInit =
          pack . unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" ++ destName exec ++ " || exit 0"
                   , "    log_begin_msg \"Starting " ++ destName exec ++ "...\""
                   , "    mkdir -p " ++ databaseDirectory b
                   , "    " ++ startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " ++ destName exec ++ "...\""
                   , "    " ++ stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
      stopCommand = showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server /= "" then ["--retry=" ++ retry server ] else []
      serverOptions = serverFlags server ++ commonServerOptions
      -- Without these, happstack servers chew up CPU even when idle
      commonServerOptions = ["+RTS", "-IO", "-RTS"]

      debianPostinst =
          pack . unlines $
                   ([ "#!/bin/sh"
                    , ""
                    , "case \"$1\" in"
                    , "  configure)" ] ++
                    (if isSite
                     then [ "    # Apache won't start if this directory doesn't exist"
                          , "    mkdir -p " ++ apacheLogDirectory b
                          , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
                          , "    /usr/sbin/a2enmod proxy"
                          , "    /usr/sbin/a2enmod proxy_http"
                          , "    service apache2 restart" ]
                     else []) ++
                    [ "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate :: BinPkgName -> [(DebAtomKey, DebAtom)]
serverLogrotate b =
    [(Binary b, (DHLogrotateStanza . pack . unlines $
                   [ serverAccessLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ])),
     (Binary b, (DHLogrotateStanza . pack . unlines $
                   [ serverAppLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ]))]

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
