{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Debian.Repo.Repository
    ( repoArchList
    , readPkgVersion
    , showPkgVersion
    , verifyUploadURI
    , uploadRemote
    ) where

import Control.Applicative.Error ( Failing(Success, Failure) )
import Control.Arrow (second)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (sortBy, groupBy, isSuffixOf)
import Data.Time ( NominalDiffTime )
import qualified Data.Set as Set ( member, fromList )
import Debian.Arch (Arch, parseArch)
import Debian.Changes ( ChangesFile(changeDir, changePackage, changeRelease, changeVersion) )
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile), fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Changes ( findChangesFiles, key, path )
import Debian.Repo.Types (PkgVersion(..), prettyPkgVersion, outsidePath)
import Debian.Repo.Types.Release (Release(..))
import Debian.Repo.Types.Repo (Repo(repoReleaseInfo))
import Debian.Repo.Types.Repository (LocalRepository(repoRoot))
import Debian.URI (URIAuth(uriPort, uriRegName, uriUserInfo), uriToString', URI(uriAuthority, uriPath))
import Debian.Version ( parseDebianVersion, DebianVersion, prettyDebianVersion )
import Extra.Bool ( cond )
--import Extra.Either ( rightOnly )
import Extra.Files ( replaceFile )
import "Extra" Extra.List ( listIntersection )
import Extra.SSH ( {-sshExport,-} sshVerify )
import System.FilePath ( (</>) )
--import System.Cmd ( system )
import System.Directory ( doesFileExist, getDirectoryContents )
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (shell, readProcessWithExitCode, showCommandForUser)
--import System.Unix.LazyProcess (Output)
--import System.Unix.Outputs (checkResult)
import System.Process.Progress (Output, foldOutputsL, timeTask, runProcessV, quieter, qPutStrLn)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex ( matchRegex, mkRegex )

-- |The file produced by dupload when a package upload attempt is made.
data UploadFile = Upload FilePath String DebianVersion Arch

-- |Make sure we can access the upload uri without typing a password.
verifyUploadURI :: MonadIO m => Bool -> URI -> m ()
verifyUploadURI doExport uri = (\ x -> qPutStrLn ("Verifying upload URI: " ++ show uri) >> quieter 2 x) $
    case doExport of
      True -> export
      False -> verify >> mkdir
    where
      export =
          do -- The code in sshExport needs to be rewritten.
             -- liftIO $ uncurry sshExport (uriDest uri) >>= either fail return
             verify
             mkdir
      verify =
          do result <- liftIO $ uncurry sshVerify (uriDest uri)
             case result of
               Right () -> return ()
               Left s -> error $ "Unable to reach " ++ uriToString' uri ++ ": " ++ s
             mkdir
      mkdir =
          case uriAuthority uri of
            Nothing -> error $ "Internal error 7"
            Just auth ->
                do let cmd = "ssh"
                       args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth,
                               "mkdir", "-p", uriPath uri ++ "/incoming"]
                   (result, _, _) <- liftIO (readProcessWithExitCode cmd args "")
                   case result of
                     ExitSuccess -> return ()
                     _ -> fail $ showCommandForUser cmd args ++ " -> " ++ show result

uriDest :: URI -> ([Char], Maybe Int)
uriDest uri =
    (uriUserInfo auth ++ uriRegName auth, port)
    where
      auth = maybe (error "Internal error 8") id (uriAuthority uri)
      port =
          case uriPort auth of
            (':' : number) -> Just (read number)
            "" -> Nothing
            x -> error $ "Internal error 9: invalid port " ++ x

-- | Upload all the packages in a local repository to a the incoming
-- directory of a remote repository (using dupload.)
uploadRemote :: LocalRepository		-- ^ Local repository holding the packages.
             -> URI			-- ^ URI of upload repository
             -> IO [Failing ([Output L.ByteString], NominalDiffTime)]
uploadRemote repo uri =
    do uploaded <- uploadFind (outsidePath root) >>=
                   return . Set.fromList . map uploadKey . successes
       -- Find the changes files
       changesFiles <- findChangesFiles (outsidePath root)
       -- Check that they have not yet been uploaded
       let changesFiles' = map (\ f -> if notUploaded uploaded f then Success f else Failure ["Already uploaded: " ++ show (pretty f)]) changesFiles
       -- Create groups of common name and dist, and sort so latest version appears first.
       let changesFileGroups = map (sortBy compareVersions) . groupByNameAndDist $ changesFiles'
       let changesFiles'' = concatMap keepNewest changesFileGroups
       changesFiles''' <- mapM validRevision' changesFiles''
       mapM dupload' changesFiles'''
    where
      keepNewest (Success newest : older) =
          Success newest : map tooOld older
      keepNewest xs = xs
      -- Add to Control.Applicative.Error
      -- partitionFailing :: [Failing a] -> ([[String]], [a])
      -- partitionFailing = foldr f ([], []) where f (Failure ms) (msgs, xs) = (ms : msgs, xs)
      --                                           f (Success x) (msgs, xs) = (msgs, x : xs)
      tooOld (Failure x) = Failure x
      tooOld (Success x) = Failure ["Not the newest version in incoming: " ++ show (pretty x)]
      successes (Success x : xs) = x : successes xs
      successes (Failure _ : xs) = successes xs
      successes [] = []
      root = repoRoot repo
{-
      rejectOlder :: ([ChangesFile], [(ChangesFile, String)]) ->  ([ChangesFile], [(ChangesFile, String)])
      rejectOlder (accept, reject) =
          (accept', (map tag reject' ++ reject))
          where accept' = map head sortedGroups
                reject' = concat . map tail $ sortedGroups
                sortedGroups = map (sortBy compareVersions) (groupByNameAndDist accept)
                tag x = (x, "Not the newest version in incoming")
-}
      compareVersions (Success a) (Success b) = compare (changeVersion b) (changeVersion a)
      compareVersions (Failure _) (Success _) = LT
      compareVersions (Success _) (Failure _) = GT
      compareVersions (Failure _) (Failure _) = EQ
      groupByNameAndDist :: [Failing ChangesFile] -> [[Failing ChangesFile]]
      groupByNameAndDist = groupBy equalNameAndDist . sortBy compareNameAndDist
      equalNameAndDist a b = compareNameAndDist a b == EQ
      compareNameAndDist (Success a) (Success b) =
          case compare (changePackage a) (changePackage b) of
            EQ -> compare (changeRelease a) (changeRelease b)
            x -> x
      compareNameAndDist (Failure _) (Success _) = LT
      compareNameAndDist (Success _) (Failure _) = GT
      compareNameAndDist (Failure _) (Failure _) = EQ
      notUploaded uploaded changes = not . Set.member (Debian.Repo.Changes.key changes) $ uploaded
      --showReject (changes, tag) = Debian.Repo.Changes.name changes ++ ": " ++ tag
      dupload' (Failure x) = return (Failure x)
      dupload' (Success c) = liftIO (dupload uri (outsidePath root) (Debian.Repo.Changes.path c))

validRevision' :: Failing ChangesFile -> IO (Failing ChangesFile)
validRevision' (Failure x) = return (Failure x)
validRevision' (Success c) = validRevision
    where
      validRevision :: IO (Failing ChangesFile)
      validRevision =
          doesFileExist dscPath >>=
                        cond (S.parseControlFromFile dscPath >>=
                              either (\ e -> return (Failure [show e])) (checkRevision dscPath))
                             (return (Success c))
      dscPath = changeDir c </> changePackage c ++ "_" ++ show (prettyDebianVersion (changeVersion c)) ++ ".dsc"
      checkRevision :: FilePath -> S.Control' String -> IO (Failing ChangesFile)
      checkRevision dscPath' (S.Control [p]) =
          case maybe (Failure ["Missing Fingerprint field in " ++ dscPath']) parseRevision (S.fieldValue "Fingerprint" p) of
            Failure msgs -> return (Failure msgs)
            Success (x, _) | x == invalidRevision -> return (Failure ["Invalid revision: " ++ show x])
            Success _ -> return (Success c)
      checkRevision dscPath' _ = return (Failure ["Invalid .dsc file: " ++ show dscPath'])
      invalidRevision = "none"
      -- Parse the "Fingerprint:" value describing the origin of the
      -- package's source and the dependency versions used to build it:
      --   Revision: <revisionstring> dep1=ver1 dep2=ver2 ...
      parseRevision :: String -> Failing (String, [PkgVersion])
      parseRevision s =
          case reads s :: [(String, String)] of
            [(method, etc)] ->
                case words etc of
                  (sourceVersion : buildDeps)
                    | not (elem '=' sourceVersion) ->
                        Success (method, map readPkgVersion buildDeps)
                  buildDeps ->
                        Success (method, map readPkgVersion buildDeps)
            _ -> Failure ["Invalid revision field: " ++ s]

uploadKey :: UploadFile -> (String, DebianVersion, Arch)
uploadKey (Upload _ name ver arch) = (name, ver, arch)

-- |Find all the .upload files in a directory and parse their names.
uploadFind :: FilePath -> IO [Failing UploadFile]
uploadFind dir =
    getDirectoryContents dir >>=
    return . map (parseUploadFilename dir) . filter (isSuffixOf ".upload")

-- |Parse the name of a .upload file
parseUploadFilename :: FilePath
                    -> String
                    -> Failing UploadFile
parseUploadFilename dir name =
    case matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$") name of
      Just [_, name', version, arch] -> Success (Upload dir name' (parseDebianVersion version) (parseArch arch))
      _ -> Failure ["Invalid .upload file name: " ++ name]

showPkgVersion :: PkgVersion -> String
showPkgVersion v = show (prettyPkgVersion v)

readPkgVersion :: String -> PkgVersion
readPkgVersion s = case second (parseDebianVersion . (drop 1)) (span (/= '=') s) of
                     (n, v) -> PkgVersion { getName = BinPkgName n, getVersion = v }

{-
accept :: (a -> Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> ([a], [(a, String)])
accept p tag (accepted, rejected) =
    (accepted', map tag rejected' ++ rejected)
    where (accepted', rejected') = partition p accepted

acceptM :: (Monad m) => (a -> m Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> m ([a], [(a, String)])
acceptM p tag (accept, reject) =
    do (accept', reject') <- partitionM p accept
       return (accept', (map tag reject' ++ reject))
-}

-- |Run dupload on a changes file with an optional host (--to)
-- argument.
dupload :: URI		-- user
        -> FilePath	-- The directory containing the .changes file
        -> String	-- The name of the .changes file to upload
        -> IO (Failing ([Output L.ByteString], NominalDiffTime))
dupload uri dir changesFile  =
    case uriAuthority uri of
      Nothing -> error ("Invalid Upload-URI: " ++ uriToString' uri)
      Just auth ->
          do
            let config = ("package config;\n" ++
                          "$cfg{'default'} = {\n" ++
                          "        fqdn => \"" ++ uriRegName auth ++ uriPort auth ++ "\",\n" ++
                          "        method => \"scpb\",\n" ++
	                  "        login => \"" ++ init (uriUserInfo auth) ++ "\",\n" ++
                          "        incoming => \"" ++ uriPath uri ++ "/incoming\",\n" ++
                          "        dinstall_runs => 1,\n" ++
                          "};\n\n" ++
			  "$preupload{'changes'} = '';\n\n" ++
                          "1;\n")
            replaceFile (dir ++ "/dupload.conf") config
            let cmd = "cd " ++ dir ++ " && dupload --to default -c " ++ changesFile
            qPutStrLn ("Uploading " ++ show changesFile)
            (result, elapsed) <-
                quieter 1 $ timeTask $ do
                output <- runProcessV (shell cmd) L.empty
                foldOutputsL (doCode cmd) ignore ignore ignore (return (Right output)) output
            qPutStrLn ("Upload finished, elapsed time " ++ show elapsed)
            return (either (Failure . (: [])) (\ output -> Success (output, elapsed)) result)

doCode :: String -> IO (Either String [Output L.ByteString]) -> ExitCode -> IO (Either String [Output L.ByteString])
doCode _ result ExitSuccess = result
doCode cmd _ (ExitFailure n) =
    let message = "dupload failed: " ++ cmd ++ " -> " ++ show n in
    qPutStrLn message >> return (Left message)

ignore :: forall a. IO (Either String [Output L.ByteString]) -> a -> IO (Either String [Output L.ByteString])
ignore result _ = result

repoArchList :: Repo r => r -> [Arch]
repoArchList repo =
    listIntersection (map releaseArchitectures (repoReleaseInfo repo))
