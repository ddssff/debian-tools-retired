{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
-- |Replacement for debpool.  
module Main where

import		 Prelude hiding (putStr, putStrLn, putChar)
import		 Control.Monad.Trans
import		 Debian.Repo (runAptIO, outsidePath, MonadApt, findReleases, scanIncoming, deleteTrumped, deleteGarbage, signReleases,
                              LocalRepository, envPath, repoRoot, InstallResult, explainError, resultToProblems, prepareLocalRepository,
                              setRepositoryCompatibility, Release(..), prepareRelease,
                              EnvPath(EnvPath), EnvRoot(EnvRoot), parseArchitectures,
                              Layout(Pool, Flat), showErrors, mergeReleases, deleteSourcePackages,
                              PackageID, makeBinaryPackageID, PackageIndex(PackageIndex), Repository)
import		 Debian.Config (ParamDescr(..), option)
import		 Control.Monad
import		 Data.Maybe
import           Data.Text (pack)
import		 Extra.GPGSign
import		 Extra.Lock
import           Debian.Arch (Arch(Binary, Source), ArchCPU(..), ArchOS(..), prettyArch)
import           Debian.Changes (ChangesFile(..))
import           Debian.Relation (BinPkgName)
import           Debian.Release
import		 Debian.Version
import		 System.Console.GetOpt
import           System.Directory (createDirectoryIfMissing)
import		 System.Environment
import		 System.Exit
import qualified System.IO as IO
import           System.Process.Progress (quieter, qPutStrLn)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import		 Text.Regex
import           Debian.NewDist.Version (myVersion)
import		 Extra.Email

appName = "newdist"
defaultRoot = "/var/www/" ++ appName
--defaultComponent = Component "main"
-- owner = "root"
-- group = "root"

data ParamRec
    = ParamRec
      { verbosity :: Int
      , rootParam :: FilePath
      , uploadSection :: Maybe FilePath
      , expire :: Bool
      , cleanUp :: Bool
      -- , dryRun :: Bool
      , removePackages :: [String]
      , install :: Bool
      , releases :: [String]
      , aliases :: [String]
      , sections :: [String]
      , replace :: Bool
      , notifyEmail :: [String]
      , senderEmail :: Maybe String
      , verify :: Bool
      , rejectRevision :: Maybe String
      , printVersion :: Bool
      , layout :: Layout
      , signRepo :: Bool
      , architectures :: Maybe String
      , keyName :: Maybe String -- "Name of the pgp key with which to sign the repository."
      }

initialParams :: FilePath -> ParamRec
initialParams home =
    ParamRec
    { verbosity = 0
    , rootParam = home
    , uploadSection = Nothing
    , expire = False
    , cleanUp = False
    , removePackages = []
    , install = True
    , releases = []
    , aliases = []
    , sections = []
    , replace = False
    , notifyEmail = []
    , senderEmail = Nothing
    , verify = False
    , rejectRevision = Nothing
    , printVersion = False
    , layout = Pool
    , signRepo = False
    , architectures = Nothing
    , keyName = Nothing
    }

optSpecs :: [ParamDescr (ParamRec -> ParamRec)]
optSpecs =
    [ Param ['v'] ["verbose"] ["Verbose"] (NoArg (\ p -> p {verbosity = verbosity p + 1}))
                 "Increase the amount of debugging output"
    , Param [] ["root"] ["Root"] (ReqArg (\ s p -> p {rootParam = s}) "PATH")
		 "Specify the root directory of the repository"
    , Param [] ["section"] ["Section"] (ReqArg (\ s p -> p {uploadSection = Just s}) "PATH")
		 "Force uploads to the specified section"
    , Param [] ["expire"] ["Expire"] (NoArg (\ p -> p {expire = True}))
		 "Remove all packages trumped by newer versions from the package lists."
    , Param [] ["clean-up"] ["Clean-Up"] (NoArg (\ p -> p {cleanUp = True}))
		 "Move all unreferenced files in the repository to the removed directory."
{-  , Param ['n'] ["dry-run"] ["Dry-Run"]	(NoArg (Value "Dry-Run" "yes"))
		 "Test run, don't modify the repository." -}
    , Param [] ["remove"] ["Remove"] (ReqArg (\ s p -> p {removePackages = removePackages p ++ [s]}) "DIST,SECTION,PACKAGE=VERSION")
                 "remove a particular version of a package (may be repeated.)"
    , Param ['i'] ["install"] ["Install"] (NoArg (\ p -> p {install = True}))
                 ("Scan the incoming directory and install the packages found there.\n" ++
                  "This option is automatically true if no --remove arguments are given.")
    , Param [] ["create-release"] ["Create-Release"] (ReqArg (\ s p -> p {releases = releases p ++ [s]}) "NAME")
		 ("Create any new releases and/or sections found in Distribution\n" ++
                  "and Section fields of the uploaded .changes files.")
    , Param [] ["create-alias"] ["Create-Alias"] (ReqArg (\ s p -> p {aliases = aliases p ++ [s]}) "ALIAS=RELEASE")
		 "Create an alias for an existing release"
    , Param [] ["create-section"] ["Create-Section"] (ReqArg (\ s p -> p {sections = sections p ++ [s]}) "RELEASE,SECTION")
		 "Create a new section in the given release."
    , Param [] ["replace"] ["Replace"]	(NoArg (\ p -> p {replace = True}))
                 ("Permit uploading of a package whose version is already present.\n" ++
                  "This is normally an error.")
    , Param [] ["notify-email"] ["Notify-Email"] (ReqArg (\ s p -> p {notifyEmail = notifyEmail p ++ [s]}) "USER@HOST")
                 "Email address to send notifications about success and failure of uploads."
    , Param [] ["sender-email"] ["Sender-Email"] (ReqArg (\ s p -> p {senderEmail = Just s}) "USER@HOST")
                 "Sender address for notifications."
    , Param [] ["verify"] ["Verify"] (NoArg (\ p -> p {verify = True}))
                "Verify the structure and contents of the repository."
    , Param [] ["reject-revision"] ["Reject-Revision"] (ReqArg (\ s p -> p {rejectRevision = Just s}) "STRING")
                 ("Disallow uploads of packages with this revision string.  The\n" ++
                  "autobuilder gives 'dir' targets the revision string 'none', the\n" ++
                  "option 'Reject-Revision: none' can be used to prohibit uploads of\n" ++
                  "packages built from a 'dir' target.")
    , Param [] ["version"] ["Version"] (NoArg (\ p -> p {printVersion = True}))
                 "Print the version string and exit"
    , Param [] ["layout"] ["Layout"] (ReqArg (\ s p -> p {layout = case s of
                                                                     "pool" -> Pool
                                                                     "flat" -> Flat
                                                                     _ -> layout p}) "flat or pool")
                 "Specify a default layout"
    , Param [] ["no-sign"] ["No-Sign"] (NoArg (\ p -> p {signRepo = False}))
                 "Do not attempt to cryptographically sign the repository."
    , Param [] ["sign"] ["Sign"] (NoArg (\ p -> p {signRepo = True}))
                 "Cryptographically sign the repository even if nothing changed."
    , Param [] ["architectures"] ["Architectures"] (ReqArg (\ s p -> p {architectures = Just s}) "ARCH,ARCH,...")
                 "Name of the pgp key with which to sign the repository."
    , Param [] ["keyname"] ["Key-Name"] (ReqArg (\ s p -> p {keyName = Just s}) "STRING")
                 "Name of the pgp key with which to sign the repository."
{-
     , Param [] ["rebuild"] ["Rebuild"]	(NoArg (Value "Rebuild" "yes"))
     "(UNIMPLEMENTED) Reconstruct the package lists from scratch."
     , Param [] ["obsolete"] ["Obsolete"]	(NoArg (Value "Obsolete" "yes"))
     (My.consperse "\n"
      ["(UNIMPLEMENTED) Remove any package for which newer versions exist,",
       "remove any package which is not part of any dist."])
-}
    ]

{-
defaultStyles :: IOStyle
defaultStyles = (setFinish (Just "done.") .
                 setError (Just "failed.") .
                 setEcho False .
                 setElapsed False .
                 setDots IO.stdout 1024 '.' .
                 setDots IO.stderr 1024 ',') (Debian.IO.defStyle 0)
-}

defaultArchitectures = [Binary (ArchOS "linux") (ArchCPU "i386"), Binary (ArchOS "linux") (ArchCPU "amd64")]

{-
style :: [Flag] -> TStyle -> TStyle
style flags = foldl (.) id . map readStyle $ findValues flags "Style"

readStyle :: String -> TStyle -> TStyle
readStyle text =
    case (mapSnd tail . break (== '=')) text of
      --("Start", message) -> setStart . Just $ message
      --("Finish", message) -> setFinish . Just $ message
      --("Error", message) -> setError . Just $ message
      --("Output", "Indented") -> addPrefixes "" ""
      --("Output", "Dots") -> dotStyle IO.stdout . dotStyle IO.stderr
      --("Output", "Quiet") -> quietStyle IO.stderr . quietStyle IO.stdout
      --("Echo", flag) -> setEcho (readFlag flag)
      --("Elapsed", flag) -> setElapsed (readFlag flag)
      ("Verbosity", value) -> setVerbosity (read value)
      --("Indent", prefix) -> addPrefixes prefix prefix
      _ -> id
    where
      readFlag "yes" = True
      readFlag "no" = False
      readFlag "true" = True
      readFlag "false" = True
      readFlag text = error ("Unrecognized bool: " ++ text)
      mapSnd f (a, b) = (a, f b)
-}

main :: IO ()
main =
    do args <- getArgs
       home <- getEnv "HOME"
       let verbosity = length $ filter (flip elem ["-v", "--verbose"]) args
       -- runTIO defStyle { verbosity = verbosity }
       runAptIO
               (do -- Compute configuration options
                   let flags' = initialParams home
                   (flags, _) <-
                           case getOpt Permute (map option optSpecs) args of
                             (o, n, []) -> return (foldl (flip id) (initialParams home) o, n)
                             (_, _, errs) -> error (concat errs ++ usageInfo "Usage:" (map option optSpecs))
                   -- flags <- liftIO (computeConfig verbosity appName flags' nameFirstSection) >>= return . concat
                   -- quieter 1 (qPutStrLn ("Flags:\n  " ++ concat (intersperse "\n  " (map show flags))))
                   let lockPath = outsidePath (root flags) ++ "/newdist.lock"
                   liftIO (createDirectoryIfMissing True (outsidePath (root flags)))
                   case printVersion flags of
                     False -> withLock lockPath (runFlags flags)
                     True -> liftIO (IO.putStrLn myVersion) >>
                             liftIO (exitWith ExitSuccess))
    where
{-
      nameFirstSection (flags : more) = nameSection "Main" flags : more
      nameFirstSection [] = []
      nameSection name' flags =
          case any isName flags of
            False -> (Name name' : flags)
            True -> flags
      isName (Name _) = True
      isName _ = False
      useMain flags =
          case any isUse flags of
            False -> (Use ["Main"] : flags)
            True -> flags
      isUse (Use _) = True
      isUse _ = False
-}

runFlags :: MonadApt m => ParamRec -> m ()
runFlags flags =
    do createReleases flags
       repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       -- Get a list of the Release objects in the repository at Root
       releases <- findReleases repo
       -- Get the Repository object, this will certainly be a singleton list.
       --let repos = nub $ map releaseRepo releases
       _ <- liftIO (deletePackages releases flags keyname)
       --vPutStrBl 1 IO.stderr $ "newdist " ++ show (root flags)
       --vPutStrBl 1 IO.stderr $ "signRepository=" ++ show signRepository
       --io $ exitWith ExitSuccess
       liftIO $ setRepositoryCompatibility repo
       when (install flags)
                ((scanIncoming False keyname repo) >>=
                     \ (ok, errors) -> (liftIO (sendEmails senderAddr emailAddrs (map (successEmail repo) ok)) >>
                                        liftIO (sendEmails senderAddr emailAddrs (map (\ (changes, e) -> failureEmail changes e) errors)) >>
                                        liftIO (exitOnError (map snd errors))))
       when (expire flags)  $ liftIO (deleteTrumped keyname releases) >> return ()
       when (cleanUp flags) $ deleteGarbage repo >> return ()
       -- This flag says sign even if nothing changed
       when (signRepo flags) $ liftIO (signReleases keyname releases)
    where
{-
      findReleaseByName :: [Release] -> ReleaseName -> Maybe Release
      findReleaseByName releases dist =
          case filter (\ release -> releaseName release == dist) releases of
            [] -> Nothing
            [release] -> (Just release)
            _ -> error ("Multiple releases with name " ++ show dist)
-}
      --section = Component $ maybe "main" id $ findValue flags "Section"
      --runStyle = (cond Debian.IO.dryRun Debian.IO.realRun dryRun) . setVerbosity verbose . (style flags)
      --dryRun = findBool flags "Dry-Run"
      --verbose = maybe 0 read (findValue flags "Verbose")
      remove = removePackages flags
      emailAddrs :: [(String, String)]
      emailAddrs =
          catMaybes . map parseEmail . concat . map (splitRegex (mkRegex "[ \t]*,[ \t]*")) . notifyEmail $ flags
      senderAddr :: (String, String)
      senderAddr = maybe ("autobuilder", "somewhere") id . maybe Nothing parseEmail . senderEmail $ flags
      successEmail :: LocalRepository -> ChangesFile -> (String, [String])
      successEmail repo changesFile =
          let subject = ("newdist: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++ 
                         " now available in " ++ releaseName' (changeRelease changesFile) ++
                         " (" ++ show (prettyArch (changeArch changesFile)) ++")")
              body = ("Repository " ++ envPath (repoRoot repo)) : [] : (lines $ show $ pretty $ changeInfo changesFile) in
    	  (subject, body)
      failureEmail :: ChangesFile -> InstallResult -> (String, [String])
      failureEmail changesFile e =
          let subject = ("newdist failure: " ++ changePackage changesFile ++ "-" ++ show (prettyDebianVersion (changeVersion changesFile)) ++ 
                         " failed to install in " ++ releaseName' (changeRelease changesFile))
              body = concat (map (lines . explainError) (resultToProblems e)) in
          (subject, body)
      --dists = map ReleaseName $ findValues flags "Create"
      -- expire = findBool flags "Expire"
      -- cleanUp = findBool flags "Clean-Up"
      -- install = findBool flags "Install" || (remove == [] && cleanUp == False)
      --replace = findBool flags "Replace"
      --verify = findBool flags "Verify"
      -- signRepository = sign flags
      keyname =
          case (keyName flags, signRepo flags) of
            (Just "none", _) -> Nothing
            (_, False) -> Nothing
            (Nothing, True) -> Just Extra.GPGSign.Default
            (Just x, True) -> Just (Extra.GPGSign.Key x)
      parseEmail s = case break (== '@') s of
                       (user, ('@' : host)) -> Just (user, host)
                       _ -> Nothing


createReleases flags =
    do repo <- prepareLocalRepository (root flags) (Just . layout $ flags)
       rels <- findReleases repo
       mapM_ (createRelease repo (archList flags)) (map parseReleaseName . releases $ flags)
       mapM_ (createAlias repo) (aliases flags)
       mapM_ (createSectionOfRelease rels repo) (sections flags)
    where
      createSectionOfRelease releases repo arg =
          case break (== ',') arg of
            (rel, ',' : sectName) ->
                case filter (\ (repo, release) -> releaseName release == parseReleaseName rel) releases of
                  [release] -> createSection repo release (parseSection' sectName)
                  [] -> error $ "createReleases: Invalid release name: " ++ rel
                  _ -> error "Internal error 1"
            _ ->
                error $ "Invalid argument to --create-section: " ++ arg
      createSection :: MonadApt m => LocalRepository -> (Repository, Release) -> Section -> m (Repository, Release)
      createSection repo (repo', release) section' =
          case filter ((==) section') (releaseComponents release) of
            [] -> prepareRelease repo (releaseName release) (releaseAliases release)
                    (releaseComponents release ++ [section'])  (releaseArchitectures release)
            _ -> return (repo', release)

root flags = EnvPath (EnvRoot "") (rootParam flags)

archList flags = maybe defaultArchitectures (parseArchitectures . pack) $ architectures flags

createRelease :: MonadApt m => LocalRepository -> [Arch] -> ReleaseName -> m (Repository, Release)
createRelease repo archList' name =
    do releases <- findReleases repo
       case filter (\ (repo', release) -> elem name (releaseName release : releaseAliases release)) releases of
         [] -> prepareRelease repo name [] [parseSection' "main"] archList'
         [release] -> return release
         _ -> error "Internal error 2"

createAlias :: MonadApt m => LocalRepository -> String -> m (Repository, Release)
createAlias repo arg =
    case break (== '=') arg of
      (rel, ('=' : alias)) ->
          do releases <- findReleases repo
             case filter ((==) (parseReleaseName rel) . releaseName . snd) releases of
               [] -> error $ "Attempt to create alias in non-existant release: " ++ rel
               [(repo', release)] -> 
                   case elem (parseReleaseName alias) (releaseAliases release) of
                     False -> prepareRelease repo (parseReleaseName rel) (releaseAliases release ++ [parseReleaseName alias])
                               (releaseComponents release) (releaseArchitectures release)
                     True -> return (repo', release)
               _ -> error $ "Internal error 3"
      _ -> error $ "Invalid argument to --create-alias: " ++ arg

exitOnError :: [InstallResult] -> IO ()
exitOnError [] = return ()
exitOnError errors =
    do qPutStrLn (showErrors errors)
       liftIO $ IO.hFlush IO.stderr
       liftIO $ exitWith (ExitFailure 1)

-- |Return the list of releases in the repository at root, creating
-- the ones in the dists list with the given components and
-- architectures.
getReleases root' layout' dists section' archList' =
    do repo <- prepareLocalRepository root' layout'
       existingReleases <- findReleases repo
       requiredReleases <- mapM (\ dist -> prepareRelease repo dist [] section' archList') dists
       return $ mergeReleases (existingReleases ++ requiredReleases)

deletePackages releases flags keyname =
    deleteSourcePackages keyname toRemove
    where
      toRemove :: [((Repository, Release), PackageIndex, PackageID BinPkgName)]
      toRemove = map parsePackage $ removePackages flags
      parsePackage :: String -> ((Repository, Release), PackageIndex, PackageID BinPkgName)
      -- Parse a string in the form <dist>,<packagename>=<versionnumber>
      parsePackage s =
          case splitRegex (mkRegex "[,=]") s of
            [dist, component, name, ver] ->
                maybe (error ("Can't find release: " ++ dist))
                      (\ release -> (release,
                                     PackageIndex (parseSection' component) Source,
                                     makeBinaryPackageID name (parseDebianVersion ver)))
                      (findReleaseByName (parseReleaseName dist)) 
            x -> error ("Invalid remove spec: " ++ show x)
      findReleaseByName :: ReleaseName -> Maybe (Repository, Release)
      findReleaseByName dist =
          case filter (\ (repo, release) -> releaseName release == dist) releases of
            [] -> Nothing
            [release] -> (Just release)
            _ -> error ("Multiple releases with name " ++ releaseName' dist)
