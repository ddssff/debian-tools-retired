import Control.Monad.Trans (liftIO)
import Debian.Repo (runAptIO, prepareLocalRepository)
import Debian.Repo.Types (Layout(Pool), rootEnvPath)
import System.IO (hPutStrLn, stderr)

-- | How long to parse the files in a repository?
root = rootEnvPath "/srv/deb/ubuntu"

main =
    runAptIO
    (do repo <- prepareLocalRepository root (Just Pool)
        -- existingReleases <- findReleases repo
        -- requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
        -- return $ mergeReleases (existingReleases ++ requiredReleases)
        liftIO (hPutStrLn stderr ("repo: " ++ show repo)))
