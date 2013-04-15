{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Debian.Repo.Repository
    ( UploadFile(..)
    , prepareRepository
    , repoArchList
    , readPkgVersion
    , showPkgVersion
    , verifyUploadURI
    , uploadRemote
    ) where

import Control.Applicative.Error ( Failing(Success, Failure) )
import Control.Arrow (second)
import Control.Exception ( ErrorCall(..), toException )
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B (concat, ByteString, unpack)
import Data.List ( sortBy, groupBy, intercalate, isSuffixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.Time ( NominalDiffTime )
import qualified Data.Set as Set ( member, fromList )
import Debian.Arch (Arch, parseArch)
import Debian.Changes ( ChangesFile(changeDir, changePackage, changeRelease, changeVersion) )
import qualified Debian.Control.ByteString as B ( Paragraph, Control'(Control), ControlFunctions(parseControl), fieldValue )
import qualified Debian.Control.String as S ( Paragraph', Control'(Control), ControlFunctions(parseControlFromFile), fieldValue )
import Debian.Relation (BinPkgName(..))
import Debian.Release (ReleaseName(..), parseReleaseName, releaseName')
import Debian.Repo.Changes ( findChangesFiles, key, path )
import Debian.Repo.LocalRepository ( prepareLocalRepository, makeReleaseInfo )
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), insertRepository, lookupRepository )
import Debian.Repo.Types ( ReleaseInfo(releaseInfoArchitectures), PkgVersion(..), prettyPkgVersion, Repo(repoReleaseInfo), LocalRepository(repoRoot),
                           Repository(..), EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath )
import Debian.URI ( URIAuth(uriPort, uriRegName, uriUserInfo), uriToString', URI(uriAuthority, uriScheme, uriPath), dirFromURI, fileFromURI, parseURI )
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
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.Process (shell, readProcessWithExitCode, showCommandForUser)
--import System.Unix.LazyProcess (Output)
--import System.Unix.Outputs (checkResult)
import System.Process.Progress (Output, foldOutputsL, timeTask, runProcessV, quieter, qPutStrLn, qPutStr)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Regex ( matchRegex, mkRegex )
import qualified Tmp.File as F ( File(..), Source(RemotePath) )

-- |The file produced by dupload when a package upload attempt is made.
data UploadFile = Upload FilePath String DebianVersion Arch

-- |This is a remote repository which we have queried to find out the
-- names, sections, and supported architectures of its releases.
--data VerifiedRepo = VerifiedRepo URI [ReleaseInfo]

{- instance Show VerifiedRepo where
    show (VerifiedRepo uri _) = "Verified Repository " ++ show uri -- ++ " " ++ show dists
instance Ord VerifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq VerifiedRepo where
    a == b = compare a b == EQ -}

-- |This is a repository whose structure we haven't examined 
-- to determine what release it contains.
--data UnverifiedRepo = UnverifiedRepo URI

{- instance Show UnverifiedRepo where
    show (UnverifiedRepo uri) = "Unverified Repository " ++ show uri -- ++ " (unverified)"
instance Ord UnverifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq UnverifiedRepo where
    a == b = compare a b == EQ -}

-- | Prepare a repository, which may be remote or local depending on
-- the URI.
prepareRepository :: MonadApt m => URI -> m Repository
prepareRepository uri =
    do state <- getApt
       repo <- maybe newRepo return (lookupRepository uri state)
       putApt (insertRepository uri repo state)
       return repo
    where
      newRepo =
             case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               -- FIXME: We only want to verifyRepository on demand.
               _ -> verifyRepository (UnverifiedRepo (show uri))
               -- _ -> return . Repository . UnverifiedRepo $ uri

{-# NOINLINE verifyRepository #-}
verifyRepository :: MonadIO m => Repository -> m Repository
verifyRepository (UnverifiedRepo uri) =
    do --tio (vHPutStrBl IO.stderr 0 $ "Verifying repository " ++ show uri ++ "...")
       -- Use unsafeInterleaveIO to avoid querying the repository
       -- until the value is actually needed.
       -- qPutStrLn $ "verifyRepository " ++ uri
       releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromJust . parseURI $ uri
       {- tio (vHPutStrLn IO.stderr 0 $ "\n" {- -> VerifiedRepo " ++ show uri ++ " " ++ show releaseInfo -} ) -}
       return $ VerifiedRepo uri releaseInfo
verifyRepository x = return x

-- Nice code to do caching, but I figured out how to fix the old code.

--instance Read URI where
--    readsPrec _ s = [(fromJust (parseURI s), "")]
--
---- |Get the list of releases of a remote repository.
--getReleaseInfo :: FilePath
--               -> Bool     -- ^ If False don't look at existing cache
--               -> URI
--               -> IO [ReleaseInfo]
--getReleaseInfo top tryCache uri =
--    readCache >>= \ cache ->
--    return (lookup uri cache) >>=
--    maybe (updateCache cache) return
--    where
--      cachePath = top ++ "/repoCache"
--      readCache :: IO [(URI, [ReleaseInfo])]
--      readCache = if tryCache
--                  then try (readFile cachePath >>= return . read) >>= return . either (\ (_ :: SomeException) -> []) id
--                  else return []
--      updateCache :: [(URI, [ReleaseInfo])] -> IO [ReleaseInfo]
--      updateCache pairs = getReleaseInfoRemote uri >>= \ info ->
--                          writeCache ((uri, info) : pairs) >> return info
--      writeCache :: [(URI, [ReleaseInfo])] -> IO ()
--      writeCache pairs = writeFile (show pairs) cachePath

-- |Get the list of releases of a remote repository.
getReleaseInfoRemote :: URI -> IO [ReleaseInfo]
getReleaseInfoRemote uri =
    qPutStr ("(verifying " ++ uriToString' uri ++ ".") >>
    quieter 2 (dirFromURI distsURI) >>=
    quieter 2 . either (error . show) verify >>=
    return . catMaybes >>= 
    (\ result -> qPutStr ")\n" >> return result)
    where
      distsURI = uri {uriPath = uriPath uri </> "dists/"}
      verify names =
          do let dists = map parseReleaseName names
             (releaseFiles :: [F.File (S.Paragraph' B.ByteString)]) <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap B.unpack (B.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe B.ByteString -> (F.File B.Paragraph) -> ReleaseName -> Maybe ReleaseInfo
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (B.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo info (parseReleaseName (B.unpack dist)) []
      getSuite :: F.File (S.Paragraph' B.ByteString) -> Maybe B.ByteString
      getSuite (F.File {F.text = Success releaseFile}) = B.fieldValue (releaseNameField releaseFile) releaseFile
      getSuite (F.File {F.text = Failure msgs}) = fail (intercalate "\n" msgs)
      getReleaseFile :: ReleaseName -> IO (F.File (S.Paragraph' B.ByteString))
      -- getReleaseFile :: ReleaseName -> IO (S.Paragraph' B.ByteString)
      getReleaseFile distName =
          do qPutStr "."
             release <- fileFromURI releaseURI >>= return . either Left (Right . B.concat . L.toChunks)
             let control = either Left (either (Left . toException . ErrorCall . show) Right . B.parseControl (show releaseURI)) release
             case control of
               Right (B.Control [info]) -> return $ F.File {F.path = F.RemotePath releaseURI, F.text = Success info}
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c

-- |Make sure we can access the upload uri without typing a password.
verifyUploadURI :: MonadApt m => Bool -> URI -> m ()
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
      mkdir :: MonadApt m => m ()
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
    listIntersection (map releaseInfoArchitectures (repoReleaseInfo repo))
