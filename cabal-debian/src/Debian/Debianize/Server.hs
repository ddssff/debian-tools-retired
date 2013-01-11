-- | Debianization support for server programs, web server programs in particular.
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Server
       ( siteAtoms
       , serverAtoms
       , execAtoms
       , oldClckwrksFlags
       ) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Debian.Debianize.Paths (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Debianize.Types.Atoms (HasAtoms, DebAtomKey(..), DebAtom(..), insertAtom, insertAtoms')
import Debian.Debianize.Types.PackageHints (PackageHint(..), Server(..), Site(..))
import Debian.Relation (BinPkgName)
import System.FilePath ((</>))
import System.Process (showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
siteAtoms :: HasAtoms atoms => BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> String -> Int -> [String] -> String -> String -> atoms -> atoms
siteAtoms b sourceDir execName destDir destName retry port flags domain serverAdmin xs =
    siteAtoms' b port domain serverAdmin $
    apacheLogrotate b $
    serverAtoms b sourceDir execName destDir destName retry port flags True $ xs

serverAtoms :: HasAtoms atoms => BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> String -> Int -> [String] -> Bool -> atoms -> atoms
serverAtoms b sourceDir execName destDir destName retry _port flags isSite xs =
    execAtoms b sourceDir execName destDir destName $
    insertAtoms' (Binary b) (debianPostinst b isSite) $
    serverLogrotate b $
    insertAtom (Binary b) (debianInit b destName retry flags) xs

-- | Generate the atom that installs the executable.  Trickier than it should
-- be due to limitations in the dh_install script, and the fact that we don't
-- yet know the build directory path.
execAtoms :: HasAtoms atoms => BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> atoms -> atoms
execAtoms b sourceDir execName destDir destName xs =
    insertAtom Source (DebRulesFragment (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) $
    insertAtom
      (Binary b)
      (case (sourceDir, execName == destName) of
         (Nothing, True) -> DHInstallCabalExec execName d
         (Just s, True) -> DHInstall s d
         (Nothing, False) -> DHInstallCabalExecTo execName (d </> destName)
         (Just s, False) -> DHInstallTo s (d </> destName))
      xs
    where
      d = fromMaybe "usr/bin" destDir

{-
cabal :: BinPkgName -> PackageHint
cabal name =
    InstallFileHint $
    InstallFile
    { debName = name
    , execName = show (pretty name)
    , sourceDir = Nothing
    , destDir = Nothing
    , destName = show (pretty name) }

script :: FilePath -> PackageHint
script path =
    InstallFileHint $
    InstallFile
    { debName = BinPkgName (takeFileName path)
    , execName = takeFileName path
    , sourceDir = Just (takeDirectory path)
    , destDir = Nothing
    , destName = takeFileName path }
-}

debianPostinst :: BinPkgName -> Bool -> [DebAtom]
debianPostinst b isSite =
    [DHPostInst
          (pack . unlines $
           ([ "#!/bin/sh"
            , ""
            , "case \"$1\" in"
            , "  configure)" ] ++
            apache ++
            [ "    ;;"
            , "esac"
            , ""
            , "#DEBHELPER#"
            , ""
            , "exit 0" ]))]
    where
      apache =
          if isSite
          then [ "    # Apache won't start if this directory doesn't exist"
               , "    mkdir -p " ++ apacheLogDirectory b
               , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
               , "    /usr/sbin/a2enmod proxy"
               , "    /usr/sbin/a2enmod proxy_http"
               , "    service apache2 restart" ]
          else []

debianInit :: BinPkgName -> String -> String -> [String] -> DebAtom
debianInit b destName retry flags =
    DHInstallInit
            (pack . unlines $
             [ "#! /bin/sh -e"
             , ""
             , ". /lib/lsb/init-functions"
             , ""
             , "case \"$1\" in"
             , "  start)"
             , "    test -x /usr/bin/" ++ destName ++ " || exit 0"
             , "    log_begin_msg \"Starting " ++ destName ++ "...\""
             , "    mkdir -p " ++ databaseDirectory b
             , "    " ++ startCommand
             , "    log_end_msg $?"
             , "    ;;"
             , "  stop)"
             , "    log_begin_msg \"Stopping " ++ destName ++ "...\""
             , "    " ++ stopCommand
             , "    log_end_msg $?"
             , "    ;;"
             , "  *)"
             , "    log_success_msg \"Usage: ${0} {start|stop}\""
             , "    exit 1"
             , "esac"
             , ""
             , "exit 0" ])
  where
    startCommand = showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
    stopCommand = showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
    commonOptions = ["--pidfile", "/var/run/" ++ destName]
    startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName]
    stopOptions = ["--stop", "--oknodo"] ++ if retry /= "" then ["--retry=" ++ retry ] else []
    serverOptions = flags ++ commonServerOptions
    -- Without these, happstack servers chew up CPU even when idle
    commonServerOptions = ["+RTS", "-IO", "-RTS"]

oldClckwrksFlags :: PackageHint -> [String]
oldClckwrksFlags (SiteHint _ site) =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ domain site ++ "/"
    , "--http-port", show port]
oldClckwrksFlags (ServerHint _ server) =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ hostname server ++ ":" ++ show (port server) ++ "/"
    , "--http-port", show port]
oldClckwrksFlags _ = []

-- | An apache site configuration file.  This is installed via a line
-- in debianFiles.
siteAtoms' :: HasAtoms atoms => BinPkgName -> Int -> String -> String -> atoms -> atoms
siteAtoms' b port domain serverAdmin xs =
    insertAtom (Binary b) (DHLink ("/etc/apache2/sites-available/" ++ domain) ("/etc/apache2/sites-enabled/" ++ domain)) $
    insertAtom (Binary b) (DHInstallDir (apacheLogDirectory b)) $  -- Server won't start if log directory doesn't exist
    insertAtom (Binary b) (DHFile ("/etc/apache2/sites-available" </> domain) text) $ xs
    where
      text =
           (pack . unlines $
             [  "<VirtualHost *:80>"
              , "    ServerAdmin " ++ serverAdmin
              , "    ServerName www." ++ domain
              , "    ServerAlias " ++ domain
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
              , "    <Proxy http://127.0.0.1:" ++ show port ++ "/*>"
              , "                AddDefaultCharset off"
              , "                Order deny,allow"
              , "                #Allow from .example.com"
              , "                #Deny from all"
              , "                Allow from all"
              , "    </Proxy>"
              , ""
              , "    SetEnv proxy-sendcl 1"
              , ""
              , "    ProxyPass / http://127.0.0.1:" ++ show port ++ "/ nocanon"
              , "    ProxyPassReverse / http://127.0.0.1:" ++ show port ++ "/"
              , "</VirtualHost>" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate :: HasAtoms atoms => BinPkgName -> atoms -> atoms
serverLogrotate b xs =
    insertAtoms' (Binary b)
                 [ DHLogrotateStanza . pack . unlines $
                   [ serverAccessLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ]
                 , DHLogrotateStanza . pack . unlines $
                   [ serverAppLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ] ]
                 xs

apacheLogrotate :: HasAtoms atoms => BinPkgName -> atoms -> atoms
apacheLogrotate b xs =
    insertAtoms' (Binary b)
                 [ DHInstallDir (apacheLogDirectory b),
                   DHLogrotateStanza . pack . unlines $
                   [ apacheAccessLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}"]
                 , DHLogrotateStanza . pack . unlines $
                   [ apacheErrorLog b ++ " {"
                   , "  weekly"
                   , "  rotate 5"
                   , "  compress"
                   , "  missingok"
                   , "}" ] ]
                 xs
