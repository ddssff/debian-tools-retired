-- | Some FilePath conventions used in our debianization rules.
module Debian.Debianize.Paths
    ( databaseDirectory
    , apacheLogDirectory
    , apacheErrorLog
    , apacheAccessLog
    , serverLogDirectory
    , serverAppLog
    , serverAccessLog
    ) where

import Debian.Relation (BinPkgName)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

databaseDirectory :: BinPkgName -> String
databaseDirectory x = "/srv" </> show (pretty x)

apacheLogDirectory :: BinPkgName -> String
apacheLogDirectory x =  "/var/log/apache2/" ++ show (pretty x)

apacheErrorLog :: BinPkgName -> String
apacheErrorLog x = apacheLogDirectory x </> "error.log"

apacheAccessLog :: BinPkgName -> String
apacheAccessLog x = apacheLogDirectory x </> "access.log"

serverLogDirectory :: BinPkgName -> String
serverLogDirectory x = "/var/log/" ++ show (pretty x)

serverAppLog :: BinPkgName -> String
serverAppLog x = serverLogDirectory x </> "app.log"

serverAccessLog :: BinPkgName -> String
serverAccessLog x = serverLogDirectory x </> "access.log"

-- dh_installinit forces the service name to equal the binary package name.
-- serviceName :: BinPkgName -> String
-- serviceName x = show (pretty x)
