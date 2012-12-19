-- | Debianization support for server programs, web server programs in particular.
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Distribution.Debian.Server
       ( Executable(..)
       , script
       , cabal
       , Server(..)
       , Site(..)
       , execAtoms
       , serviceName
       , databaseDirectory
       , apacheSiteName
       , apacheLogDirectory
       , serverLogDirectory
       , apacheErrorLog
       , apacheAccessLog
       , serverAppLog
       , serverAccessLog
       , oldClckwrksFlags
       ) where

import Data.Maybe (isJust, fromMaybe)
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Debian.DebHelper (DebAtom(..))
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Process (showCommandForUser)

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
execAtoms :: Executable -> [DebAtom]
execAtoms e@(Executable {}) =
    a :
    maybe []
          (\server ->
               (debianPostinst b e server ++
                debianInit b e server ++
                logrotateConfig b e server ++
                apacheSite b e server))
          (execServer e)
    where
      a = case (sourceDir e, execName e == destName e) of
            (Nothing, True) -> DHInstallCabalExec b (execName e) d
            (Just s, True) -> DHInstall b s d
            (Nothing, False) -> DHInstallCabalExecTo b (execName e) (d </> destName e)
            (Just s, False) -> DHInstallTo b s (d </> destName e)
      d = fromMaybe "usr/bin" (destDir e)
      b = BinPkgName (debName e)

-- | Does this package require an apache site file?
needsApacheSite :: Server -> Bool
needsApacheSite spec = isJust (site spec)
-- | Does this package cause an init script to start?  (Yes, its a server.)
needsInit :: Server -> Bool
needsInit _ = True

data Executable
    = Executable
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      , execServer :: Maybe Server -- ^ Information about servers - hostname, port, start and stop info, etc.
      , debName :: String -- ^ Name of the debian binary package
      } deriving (Read, Show, Eq)

cabal :: String -> Executable
cabal name =
    Executable { execName = name
               , sourceDir = Nothing
               , destDir = Nothing
               , destName = name
               , execServer = Nothing
               , debName = name }

script :: FilePath -> Executable
script path =
    Executable { execName = takeFileName path
               , sourceDir = Just (takeDirectory path)
               , destDir = Nothing
               , destName = takeFileName path
               , execServer = Nothing
               , debName = takeFileName path }

-- | Information about the web site we are packaging.
data Server
    = Server
      { hostname :: String      -- ^ Host on which the server will run
      , port :: Int             -- ^ Port on which the server will run.
                                -- Obviously, this must assign each and
                                -- every server package to a different
                                -- port.
      , site :: Maybe Site       -- ^ Information about the web site configuration
      , headerMessage :: String -- ^ A comment that will be inserted to
                                -- explain how the file was generated
      , retry :: String         -- ^ start-stop-daemon --retry argument
      , flags :: [String]   -- ^ Extra flags to pass to the server via the init script
      } deriving (Read, Show, Eq)

data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      } deriving (Read, Show, Eq)


serviceName :: Executable -> String
serviceName = debName -- dh_installinit forces the service name to equal the binary package name
databaseDirectory :: Executable -> String
databaseDirectory x = "/srv" </> debName x
apacheSiteName :: Site -> String
apacheSiteName spec = domain spec
apacheLogDirectory :: Executable -> String
apacheLogDirectory x =  "/var/log/apache2/" ++ debName x
serverLogDirectory :: Executable -> String
serverLogDirectory x = "/var/log/" ++ debName x
apacheErrorLog :: Executable -> String
apacheErrorLog exec = apacheLogDirectory exec </> "error.log"
apacheAccessLog :: Executable -> String
apacheAccessLog exec = apacheLogDirectory exec </> "access.log"
serverAppLog :: Executable -> String
serverAppLog exec = serverLogDirectory exec </> "app.log"
serverAccessLog :: Executable -> String
serverAccessLog exec = serverLogDirectory exec </> "access.log"

debianPostinst :: BinPkgName -> Executable -> Server -> [DebAtom]
debianPostinst b e server@(Server {..}) =
    if needsInit server
    then [DHPostInst
          b
          (unlines $
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
    else []
    where
      apache =
          if needsApacheSite server
          then [ "    # Apache won't start if this directory doesn't exist"
               , "    mkdir -p " ++ apacheLogDirectory e
               , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
               , "    /usr/sbin/a2enmod proxy"
               , "    /usr/sbin/a2enmod proxy_http"
               , "    service apache2 restart" ]
          else []

debianInit :: BinPkgName -> Executable -> Server -> [DebAtom]
debianInit b e spec@(Server{..}) =
    if needsInit spec
    then [DHInstallInit
            b
            (unlines
             [ "#! /bin/sh -e"
             , ""
             , ". /lib/lsb/init-functions"
             , ""
             , "case \"$1\" in"
             , "  start)"
             , "    test -x /usr/bin/" ++ destName e ++ " || exit 0"
             , "    log_begin_msg \"Starting " ++ destName e ++ "...\""
             , "    mkdir -p " ++ databaseDirectory e
             , "    " ++ startCommand
             , "    log_end_msg $?"
             , "    ;;"
             , "  stop)"
             , "    log_begin_msg \"Stopping " ++ destName e ++ "...\""
             , "    " ++ stopCommand
             , "    log_end_msg $?"
             , "    ;;"
             , "  *)"
             , "    log_success_msg \"Usage: ${0} {start|stop}\""
             , "    exit 1"
             , "esac"
             , ""
             , "exit 0" ])]
    else []
  where
    startCommand = showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
    stopCommand = showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
    commonOptions = ["--pidfile", "/var/run/" ++ destName e]
    startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory e, "--exec", "/usr/bin" </> destName e]
    stopOptions = ["--stop", "--oknodo"] ++ if retry /= "" then ["--retry=" ++ retry] else []
    serverOptions = flags ++ commonServerOptions
    -- Without these, happstack servers chew up CPU even when idle
    commonServerOptions = ["+RTS", "-IO", "-RTS"]

oldClckwrksFlags :: Server -> [String]
oldClckwrksFlags (Server{..}) =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ maybe (hostname ++ ":" ++ show port) domain site ++ "/"
    , "--http-port", show port]

-- | An apache site configuration file.  This is installed via a line
-- in debianFiles.
apacheSite :: BinPkgName -> Executable -> Server -> [DebAtom]
apacheSite b e server =
    maybe
      []
      (\ site@(Site{..}) ->
           [DHInstallDir b (apacheLogDirectory e),
            DHLink b [("/etc/apache2/sites-available/" ++ domain, "/etc/apache2/sites-enabled/" ++ domain)],
            DHFile b ("/etc/apache2/sites-available" </> domain) (text site)])
       (site server)
    where
      text (Site{..}) =
           (unlines $
             [ "# " ++ headerMessage server
              , ""
              , "<VirtualHost *:80>"
              , "    ServerAdmin " ++ serverAdmin
              , "    ServerName www." ++ domain
              , "    ServerAlias " ++ domain
              , ""
              , "    ErrorLog " ++ apacheErrorLog e
              , "    CustomLog " ++ apacheAccessLog e ++ " combined"
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
              , "    <Proxy http://127.0.0.1:" ++ show (port server) ++ "/*>"
              , "                AddDefaultCharset off"
              , "                Order deny,allow"
              , "                #Allow from .example.com"
              , "                #Deny from all"
              , "                Allow from all"
              , "    </Proxy>"
              , ""
              , "    SetEnv proxy-sendcl 1"
              , ""
              , "    ProxyPass / http://127.0.0.1:" ++ show (port server) ++ "/ nocanon"
              , "    ProxyPassReverse / http://127.0.0.1:" ++ show (port server) ++ "/"
              , "</VirtualHost>" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
logrotateConfig :: BinPkgName -> Executable -> Server -> [DebAtom]
logrotateConfig b e server@(Server {..}) =
    [DHInstallLogrotate b (unlines (apacheConfig ++ serverConfig))]
    where
      apacheConfig =
          if needsApacheSite server
          then [ apacheAccessLog e ++ " {"
               , "  weekly"
               , "  rotate 5"
               , "  compress"
               , "  missingok"
               , "}"
               , apacheErrorLog e ++ " {"
               , "  weekly"
               , "  rotate 5"
               , "  compress"
               , "  missingok"
               , "}" ]
          else []
      serverConfig =
          if needsInit server
          then [ serverAccessLog e ++ " {"
               , "  weekly"
               , "  rotate 5"
               , "  compress"
               , "  missingok"
               , "}"
               , serverAppLog e ++ " {"
               , "  weekly"
               , "  rotate 5"
               , "  compress"
               , "  missingok"
               , "}" ]
          else []
