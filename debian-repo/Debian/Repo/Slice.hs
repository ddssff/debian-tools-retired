{-# LANGUAGE PackageImports #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Slice
    ( sourceSlices
    , binarySlices
    , inexactPathSlices
    , releaseSlices
    , appendSliceLists
    , verifySourceLine
    , verifySourcesList
    , repoSources
    , parseNamedSliceList
    , parseNamedSliceList'
    ) where

import Control.Exception ( throw )
import Control.Monad.Trans (liftIO)
import Data.List ( nubBy )
import Data.Maybe ( catMaybes )
import Data.Text as T (Text, pack, unpack)
import Debian.Control ( Control'(Control), Paragraph', ControlFunctions(parseControl), fieldValue )
import Debian.Release ( ReleaseName, parseReleaseName, parseSection')
import Debian.Sources  ( SourceType(..), SliceName(SliceName), DebSource(..) )
import Debian.Repo.LocalRepository ( prepareLocalRepository )
import Debian.Repo.Monads.Apt (MonadApt)
import Debian.Repo.Repository ( prepareRepository' )
import Debian.Repo.SourcesList ( parseSourceLine, parseSourcesList )
import Debian.Repo.Types ( NamedSliceList(..), SliceList(..), Slice, Repository(LocalRepo), EnvPath(EnvPath), EnvRoot(..) )
import Debian.URI ( URI(uriScheme, uriPath), dirFromURI, fileFromURI )
import Debian.UTF8 as Deb (decode)
import System.FilePath ((</>))
import Text.Regex ( mkRegex, splitRegex )

sourceSlices :: SliceList -> SliceList
sourceSlices = SliceList . filter ((== DebSrc) . sourceType . snd) . slices

binarySlices :: SliceList -> SliceList
binarySlices = SliceList . filter ((== Deb) . sourceType . snd) . slices

inexactPathSlices :: SliceList -> SliceList
inexactPathSlices = SliceList . filter (either (const False) (const True) . sourceDist . snd) . slices

releaseSlices :: ReleaseName -> SliceList -> SliceList
releaseSlices release list =
    SliceList . filter (isRelease . sourceDist . snd) $ (slices list)
    where isRelease = either (const False) (\ (x, _) -> x == release)

appendSliceLists :: [SliceList] -> SliceList
appendSliceLists lists =
    SliceList { slices = concat (map slices lists) }

-- |Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources :: MonadApt m => Maybe EnvRoot -> URI -> m SliceList
repoSources chroot uri =
    do dirs <- liftIO (uriSubdirs chroot (uri {uriPath = uriPath uri ++ "/dists/"}))
       releaseFiles <- mapM (liftIO . readRelease uri) dirs >>= return . catMaybes
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (Maybe EnvRoot) -> URI -> IO [Text]
uriSubdirs root uri =
    liftIO (dirFromURI uri') >>= either throw (return . map pack)
    where
      uri' = case uriScheme uri of
               "file:" -> uri {uriPath = maybe "" rootPath root ++ (uriPath uri)}
               _ -> uri

readRelease :: URI -> Text -> IO (Maybe (Paragraph' Text))
readRelease uri name =
    do output <- liftIO (fileFromURI uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (Deb.decode s) of
                      Right (Control [paragraph]) -> return (Just paragraph)
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri </> "dists" </> unpack name </> "Release"}

parseNamedSliceList :: MonadApt m => (String, String) -> m (Maybe NamedSliceList)
parseNamedSliceList (name, text) =
    (verifySourcesList Nothing . parseSourcesList) text >>=
    \ sources -> return . Just $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

-- |Create ReleaseCache info from an entry in the config file, which
-- includes a dist name and the lines of the sources.list file.
-- This also creates the basic 
parseNamedSliceList' :: MonadApt m => (String, String) -> m NamedSliceList
parseNamedSliceList' (name, text) =
    do sources <- (verifySourcesList Nothing . parseSourcesList) text
       return $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

verifySourcesList :: MonadApt m => Maybe EnvRoot -> [DebSource] -> m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

verifySourceLine :: MonadApt m => Maybe EnvRoot -> String -> m Slice
verifySourceLine chroot str = verifyDebSource chroot (parseSourceLine str)

verifyDebSource :: MonadApt m => Maybe EnvRoot -> DebSource -> m Slice
verifyDebSource chroot line =
    prepareRepository' chroot (sourceUri line) >>= \ repo -> return (repo, line)
