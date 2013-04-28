{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Release
    ( lookupRelease
    , insertRelease
    , prepareRelease
    , findReleases
    , signRelease
    , signReleases
    , mergeReleases
    ) where

import Control.Monad.State (MonadIO(..), filterM, liftM)
import qualified Data.ByteString.Lazy.Char8 as L ( empty, readFile )
import Data.Digest.Pure.MD5 (md5)
import Data.List (group, intercalate, sort)
import Data.Maybe ( catMaybes )
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, intercalate)
import Data.Time ( getCurrentTime )
import Debian.Arch (Arch(..), prettyArch)
import qualified Debian.Control.Text as S ( Field'(Field), Paragraph'(..), Control'(Control), ControlFunctions(parseControlFromFile), fieldValue )
import Debian.Release (Section, ReleaseName, releaseName', sectionName')
import Debian.Repo.Monads.Apt (MonadApt(getApt, putApt), findRelease, putRelease )
import Debian.Repo.PackageIndex ( packageIndexName, packageIndexDir, releaseDir, packageIndexList )
import Debian.Repo.Types ( PackageIndex(packageIndexArch, packageIndexComponent), Release(..), outsidePath )
import Debian.Repo.Types.Release (parseComponents, parseArchitectures)
import Debian.Repo.Types.Repository (Repository(LocalRepo), LocalRepository(repoLayout, repoRoot, repoReleaseInfoLocal), prepareLocalRepository)
import qualified Extra.Files as EF ( maybeWriteFile, prepareSymbolicLink, writeAndZipFile )
import qualified Extra.GPGSign as EG ( PGPKey, pgpSignFiles, cd )
import qualified Extra.Time as ET ( formatDebianDate )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Posix.Files ( setFileMode )
import qualified System.Posix.Files as F ( fileSize, getFileStatus )
import System.FilePath ( (</>) )
import System.Process.Progress (qPutStrLn)
import Text.PrettyPrint.ANSI.Leijen (pretty)

lookupRelease :: MonadApt m => Repository -> ReleaseName -> m (Maybe Release)
lookupRelease repo dist = getApt >>= return . findRelease repo dist

insertRelease :: MonadApt m => (Repository, Release) -> m Release
insertRelease (repo, release) =
    getApt >>= putApt . putRelease repo dist release >> return release
    where dist = releaseName release

-- | Find or create a (local) release.
prepareRelease :: MonadApt m => LocalRepository -> ReleaseName -> [ReleaseName] -> [Section] -> [Arch] -> m Release
prepareRelease repo dist aliases sections archList =
    -- vPutStrLn 0 ("prepareRelease " ++ name ++ ": " ++ show repo ++ " sections " ++ show sections) >>
    lookupRelease (LocalRepo repo) dist >>= maybe prepare (const prepare) -- return -- JAS - otherwise --create-section does not do anything
    where
      prepare :: MonadApt m => m Release
      prepare =
          do -- FIXME: errors get discarded in the mapM calls here
	     let release = Release { releaseName = dist
                                   , releaseAliases = aliases
                                   , releaseComponents = sections
                                   , releaseArchitectures = archList }
             -- vPutStrLn 0 ("packageIndexList: " ++ show (packageIndexList release))
             _ <- mapM (initIndex (outsidePath root) release) (packageIndexList release)
             mapM_ (initAlias (outsidePath root) dist) aliases
             _ <- liftIO (writeRelease (LocalRepo repo, release))
	     -- This ought to be identical to repo, but the layout should be
             -- something rather than Nothing.
             repo' <- prepareLocalRepository root (repoLayout repo)
             --vPutStrLn 0 $ "prepareRelease: prepareLocalRepository -> " ++ show repo'
             insertRelease (LocalRepo repo', release)
      initIndex root' release index = initIndexFile (root' </> packageIndexDir release index) (packageIndexName index)
      initIndexFile dir name =
          do liftIO $ createDirectoryIfMissing True dir
             liftIO $ setFileMode dir 0o040755
             ensureIndex (dir </> name)
      initAlias root' dist alias = 
          liftIO $ EF.prepareSymbolicLink (releaseName' dist) (root' <> "/dists/" <> releaseName' alias)
      root = repoRoot repo

-- | Make sure an index file exists.
ensureIndex :: MonadApt m => FilePath -> m (Either [String] ())
ensureIndex path = 
    do exists <- liftIO $ doesFileExist path
       case exists of
         False -> liftIO $ EF.writeAndZipFile path L.empty
         True -> return $ Right ()

signReleases :: Maybe EG.PGPKey -> [(LocalRepository, Release)] -> IO ()
signReleases keyname releases = mapM_ (uncurry (signRelease keyname)) releases

signRelease :: Maybe EG.PGPKey -> LocalRepository -> Release -> IO ()
signRelease keyname repo release =
    do let root = repoRoot repo
       files <- writeRelease (LocalRepo repo, release)
       case keyname of
         Nothing -> return ()
         Just key -> do results <- liftIO (EG.pgpSignFiles (outsidePath root) key files)
                        let failed = catMaybes $ map (\ (path, flag) -> if (not flag) then Just path else Nothing) (zip files results)
                        case failed of
                          [] -> return ()
                          files -> qPutStrLn ("Unable to sign:\n  " ++ Data.List.intercalate "\n  " files)

-- |Write out the @Release@ files that describe a 'Release'.
writeRelease :: (Repository, Release) -> IO [FilePath]
writeRelease (LocalRepo repo, release) =
    do let root = repoRoot repo
       indexReleaseFiles <- liftIO $ writeIndexReleases (outsidePath root) release
       masterReleaseFile <- writeMasterRelease (outsidePath root) release
       return (masterReleaseFile : indexReleaseFiles)
    where
      writeIndexReleases root release =
          mapM (writeIndex root release) (packageIndexList release)
      -- It should only be necessary to write these when the component
      -- is created, not every time the index files are changed.  But
      -- for now we're doing it anyway.
      writeIndex root release index =
          do let para =
                     S.Paragraph
                          [S.Field ("Archive", pack . releaseName' . releaseName $ release),
                           S.Field ("Component", pack $ sectionName' (packageIndexComponent index)),
                           S.Field ("Architecture", pack $ show (prettyArch (packageIndexArch index))),
                           S.Field ("Origin", " SeeReason Partners LLC"),
                           S.Field ("Label", " SeeReason")] :: S.Paragraph' Text
             let path = packageIndexDir release index ++ "/Release"
             EF.maybeWriteFile (root </> path) (show (pretty para))
             return path
      writeMasterRelease :: FilePath -> Release -> IO FilePath
      writeMasterRelease root release =
          do let paths = concat . map (indexPaths release) $ (packageIndexList release)
             (paths', sums,sizes) <- 
                 liftIO (EG.cd root
                         (do paths' <- filterM doesFileExist paths
                             sums <-  mapM (\ path -> L.readFile path >>= return . show . md5) paths'
                             sizes <- mapM (liftM F.fileSize . F.getFileStatus) paths'
                             return (paths', sums, sizes)))
             let checksums = Data.List.intercalate "\n" $ zipWith3 (formatFileInfo (fieldWidth sizes))
                      	   sums sizes (map (drop (1 + length (releaseDir release))) paths')
             timestamp <- liftIO (getCurrentTime >>= return . ET.formatDebianDate)
             let para = S.Paragraph [S.Field ("Origin", " SeeReason Partners"),
                                     S.Field ("Label", " SeeReason"),
                                     S.Field ("Suite", " " <> (pack . releaseName' . releaseName $ release)),
                                     S.Field ("Codename", " " <> (pack . releaseName' . releaseName $ release)),
                                     S.Field ("Date", " " <> pack timestamp),
                                     S.Field ("Architectures", " " <> (T.intercalate " " . map (pack . show . prettyArch) . releaseArchitectures $ release)),
                                     S.Field ("Components", " " <> (T.intercalate " " . map (pack . sectionName') . releaseComponents $ release)),
                                     S.Field ("Description", " SeeReason Internal Use - Not Released"),
                                     S.Field ("Md5Sum", "\n" <> pack checksums)] :: S.Paragraph' Text
             let path = "dists/" ++ (releaseName' . releaseName $ release) ++ "/Release"
             liftIO $ EF.maybeWriteFile (root </> path) (show (pretty para))
             return path
      indexPaths release index | packageIndexArch index == Source =
          map ((packageIndexDir release index) </>) ["Sources", "Sources.gz", "Sources.bz2", "Sources.diff/Index", "Release"]
      indexPaths release index =
          map ((packageIndexDir release index) </>) ["Packages", "Packages.gz", "Packages.bz2", "Packages.diff/Index", "Release"]
      formatFileInfo fw sum size name = Data.List.intercalate " " $ ["",sum, pad ' ' fw $ show size, name]
      fieldWidth = ceiling . (logBase (10 :: Double)) . fromIntegral . maximum
writeRelease _release = error $ "Attempt to write release files to non-local repository"

pad :: Char -> Int -> String -> String
pad padchar padlen s = replicate p padchar ++ s
    where p = padlen - length s

-- Merge a list of releases so each dist only appears once
mergeReleases :: Repository -> [Release] -> [Release]
mergeReleases repo releases =
    map merge releases
    where
      merge release =
          Release { releaseName = (releaseName . head $ releases)
                  , releaseAliases = aliases
                  , releaseComponents = components
                  , releaseArchitectures = architectures }
      aliases = map head . group . sort . concat . map releaseAliases $ releases
      components = map head . group . sort . concat . map releaseComponents $ releases
      architectures = map head . group . sort . concat . map releaseArchitectures $ releases

-- | Find all the releases in a repository.
findReleases :: MonadApt m => LocalRepository -> m [Release]
findReleases repo = mapM (findLocalRelease repo) (repoReleaseInfoLocal repo)

findLocalRelease :: MonadApt m => LocalRepository -> Release -> m Release
findLocalRelease repo releaseInfo =
    lookupRelease (LocalRepo repo) dist >>= maybe readRelease return
    where
      readRelease :: MonadApt m => m Release
      readRelease =
          do let path = (outsidePath (repoRoot repo) <> "/dists/" <> releaseName' dist <> "/Release")
             info <- liftIO $ S.parseControlFromFile path
             case info of
               Right (S.Control (paragraph : _)) ->
                   case (S.fieldValue "Components" paragraph, S.fieldValue "Architectures" paragraph) of
                     (Just components, Just architectures) ->
                         let release = (LocalRepo repo,
                                        Release
                                        { releaseName = dist
                                        , releaseAliases = releaseAliases releaseInfo
                                        , releaseComponents = parseComponents components
                                        , releaseArchitectures = parseArchitectures architectures}) in
                         insertRelease release
                     _ ->
                         error $ "Invalid release file: " ++ path
               _ -> error $ "Invalid release file: " ++ path
      dist = releaseName releaseInfo

{-
CB: Here's my take on what needs to be done based on spelunking through the Debian repositories.

Each component (main, contrib, non-free) has a Release file in the same directory as the Packages files.  These are basically static, consisting of the following fields:

    $ cat /var/www/debian/dists/unstable/non-free/binary-i386/Release
    Archive: unstable
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: i386

Slightly different for Source:

    $ cat /var/www/debian/dists/unstable/non-free/source/Release
    Archive: unstable
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: source

Also, if a dist has been released, then a version number is included:

    $ cat /var/www/debian/dists/sarge/non-free/source/Release
    Archive: stable
    Version: 3.1r4
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: source

The top level release file is different.  It has the md5sums of all the indices and release files in the distribution.  We need to find out what we should put in for Origin and Label, but I think those are company and OS respectively, so Linspire and Freespire.  It even has a codename...  As before, the version in sarge has a Version number included.

It is this top level Release file that is signed with gpg.

    $ head /var/www/debian/dists/unstable/Release
    Origin: Debian
    Label: Debian
    Suite: unstable
    Codename: sid
    Date: Wed, 06 Dec 2006 20:13:03 UTC
    Architectures: alpha amd64 arm hppa hurd-i386 i386 ia64 m68k mips mipsel powerpc s390 sparc
    Components: main contrib non-free
    Description: Debian Unstable - Not Released
    MD5Sum:
     397f3216a3a881da263a2fb5e0f8f02e 19487683 main/binary-alpha/Packages
     c3586c0dcdd6be9266a23537b386185d  5712614 main/binary-alpha/Packages.gz
     7acf2b8b938dde2c2e484425c806635a  4355026 main/binary-alpha/Packages.bz2
     dcf0d11800eb007c5435d83c853a241e     2038 main/binary-alpha/Packages.diff/Index
     3af8fbafe3e4538520989de964289712       83 main/binary-alpha/Release
     209177470d05f3a50b5217f64613ccca 19747688 main/binary-amd64/Packages
     a73fd369c6baf422621bfa7170ff1594  5777320 main/binary-amd64/Packages.gz
     380e56b5acec9ac40f160ea97ea088ef  4403167 main/binary-amd64/Packages.bz2
     43cbb8ba1631ab35ed94d43380299c38     2038 main/binary-amd64/Packages.diff/Index
     ab89118658958350f14094b1bc666a62       83 main/binary-amd64/Release
     d605623ac0d088a8bed287e0df128758 19323166 main/binary-arm/Packages


-}
