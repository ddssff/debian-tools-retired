-- | Debianization support for server programs, web server programs in particular.
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Distribution.Debian.Server
       ( Executable(..)
       , Server(..)
       , Site(..)
       , serverFiles
       , serviceName
       , databaseDirectory
       , apacheSiteName
       , apacheLogDirectory
       , serverLogDirectory
       , apacheErrorLog
       , apacheAccessLog
       , serverAppLog
       , serverAccessLog
       ) where

import Data.Maybe (isJust)
import System.FilePath ((</>))
import System.Process (showCommandForUser)

type File = (FilePath, String)

-- | Return a list of files to add to the debianization to manage the
-- server or web site.
serverFiles :: Executable -> [File]
serverFiles exec@(Script {}) =
    [debianFiles exec]
serverFiles exec@(Executable {}) =
    [debianFiles exec] ++
    maybe []
          (\server ->
               filter (not . null . snd)
                      (map (\ f -> f server)
                               [ debianPostinst exec
                               , debianLinks exec
                               , debianDirs exec
                               , debianInit exec
                               , logrotateConfig exec] ++ maybe [] (\ site -> [apacheSite exec server site]) (site server)))
          (execServer exec)

-- | Does this package require an apache site file?
needsApacheSite :: Server -> Bool
needsApacheSite spec = isJust (site spec)
-- | Does this package cause an init script to start?  (Yes, its a server.)
needsInit :: Server -> Bool
needsInit _ = True

data Executable
    = Script {execName :: String, scriptPath :: FilePath}
      -- ^ Relative path to some executable to be installed
    | Executable {execName :: String, execServer :: Maybe Server}
      -- ^ The name of an Executable section of the cabal file
      deriving (Eq, Show)

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
      } deriving (Eq, Show)

data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      } deriving (Eq, Show)


serviceName :: Executable -> String
serviceName = execName -- dh_installinit forces the service name to equal the binary package name
databaseDirectory :: Executable -> String
databaseDirectory x = "/srv" </> execName x
apacheSiteName :: Site -> String
apacheSiteName spec = domain spec
apacheLogDirectory :: Executable -> String
apacheLogDirectory x =  "/var/log/apache2/" ++ execName x
serverLogDirectory :: Executable -> String
serverLogDirectory x = "/var/log/" ++ execName x
apacheErrorLog :: Executable -> String
apacheErrorLog exec = apacheLogDirectory exec </> "error.log"
apacheAccessLog :: Executable -> String
apacheAccessLog exec = apacheLogDirectory exec </> "access.log"
serverAppLog :: Executable -> String
serverAppLog exec = serverLogDirectory exec </> "app.log"
serverAccessLog :: Executable -> String
serverAccessLog exec = serverLogDirectory exec </> "access.log"

debianPostinst :: Executable -> Server -> File
debianPostinst e server@(Server {..}) =
  ("debian" </> execName e ++ ".postinst",
   unlines $
   if needsInit server
   then ([ "#!/bin/sh"
          , ""
          , "case \"$1\" in"
          , "  configure)" ] ++
          apache ++
          [ "    service " ++ serviceName e ++ " start"
          , "    ;;"
          , "esac" ])
   else [])
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


debianDirs :: Executable -> Server -> File
debianDirs e server@(Server {..}) =
  ("debian" </> execName e ++ ".dirs",
   unlines $
   if needsApacheSite server
   then [apacheLogDirectory e,
         "/etc/apache2/sites-available"]
   else [])

debianFiles :: Executable -> File
debianFiles e@(Script {}) =
    ("debian" </> execName e ++ ".install", scriptPath e ++ " " ++ "usr/bin")
debianFiles e@(Executable {}) =
    ("debian" </> execName e ++ ".install",
     unlines (serverBinaryInstall ++ maybe [] (\ server -> logrotate server ++ apacheSiteInstall server) (execServer e)))
    where
      logrotate server =
          if needsInit server
          then ["dist-ghc/logrotate" </> execName e ++ " etc/logrotate.d"]
          else []
      apacheSiteInstall server =
          maybe [] (\ x -> ["dist-ghc/apachesite" </> apacheSiteName x ++ " etc/apache2/sites-available"]) (site server)
      serverBinaryInstall =
          ["dist-ghc" </> "build" </> execName e </> execName e ++ " " ++ " usr/bin"]

debianLinks :: Executable -> Server -> File
debianLinks e (Server {..}) =
    ("debian" </> execName e ++ ".links", unlines apacheSiteLink)
    where
      apacheSiteLink =
          maybe [] (\ x -> ["/etc/apache2/sites-available/" ++ apacheSiteName x ++ " /etc/apache2/sites-enabled/" ++ apacheSiteName x]) site

debianInit :: Executable -> Server -> File
debianInit e spec@(Server{..}) =
  ("debian" </> serviceName e ++ ".init",
   unlines $
   if needsInit spec
   then   [ "#! /bin/sh -e"
          , ""
          , ". /lib/lsb/init-functions"
          , ""
          , "case \"$1\" in"
          , "  start)"
          , "    test -x /usr/bin/" ++ execName e ++ " || exit 0"
          , "    log_begin_msg \"Starting " ++ execName e ++ "...\""
          , "    mkdir -p " ++ databaseDirectory e
          , "    " ++ startCommand
          , "    log_end_msg $?"
          , "    ;;"
          , "  stop)"
          , "    log_begin_msg \"Stopping " ++ execName e ++ "...\""
          , "    " ++ stopCommand
          , "    log_end_msg $?"
          , "    ;;"
          , "  *)"
          , "    log_success_msg \"Usage: ${0} {start|stop}\""
          , "    exit 1"
          , "esac"
          , ""
          , "exit 0" ]
   else [])
  where
    startCommand = showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
    stopCommand = showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
    commonOptions = ["--pidfile", "/var/run/" ++ execName e]
    startOptions = ["--start", "-b", "--quiet", "--make-pidfile", "-d", databaseDirectory e, "--exec", "/usr/bin" </> execName e]
    stopOptions = ["--stop", "--quiet", "--retry", retry, "--oknodo"]
    serverOptions = baseURI ++ ["--http-port", show port] ++ flags
    baseURI = ["--base-uri", "http://" ++ maybe (hostname ++ ":" ++ show port) domain site]

-- | An apache site configuration file.  This is installed via a line
-- in debianFiles.
apacheSite :: Executable -> Server -> Site -> File
apacheSite exec server (Site {..}) =
          ("dist-ghc/apachesite" </> domain,
           unlines $
           [ "# " ++ headerMessage server
           , ""
           , "<VirtualHost *:80>"
           , "    ServerAdmin " ++ serverAdmin
           , "    ServerName www." ++ domain
           , "    ServerAlias " ++ domain
           , ""
           , "    ErrorLog " ++ apacheErrorLog exec
           , "    CustomLog " ++ apacheAccessLog exec ++ " combined"
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
           , "    ProxyPass / http://127.0.0.1:" ++ show (port server) ++ "/ nocanon"
           , "    ProxyPassReverse / http://127.0.0.1:" ++ show (port server) ++ "/"
           , "</VirtualHost>" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
logrotateConfig :: Executable -> Server -> File
logrotateConfig e server@(Server {..}) =
    ("dist-ghc/logrotate" </> execName e, unlines $ apacheConfig ++ serverConfig)
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
