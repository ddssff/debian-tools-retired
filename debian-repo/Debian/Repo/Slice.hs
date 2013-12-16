{-# LANGUAGE PackageImports, StandaloneDeriving, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Slice
    ( Slice(..)
    , SliceList(..)
    , NamedSliceList(..)
    , sourceSlices
    , binarySlices
    , inexactPathSlices
    , releaseSlices
    , appendSliceLists
    ) where

import Debian.Release (ReleaseName)
import Debian.Repo.Repo (RepoKey)
import Debian.Sources (DebSource(..), SliceName, SourceType(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat)

data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = vcat . map (pretty . sliceSource) . slices

deriving instance Show SourceType
deriving instance Show DebSource

sourceSlices :: SliceList -> SliceList
sourceSlices = SliceList . filter ((== DebSrc) . sourceType . sliceSource) . slices

binarySlices :: SliceList -> SliceList
binarySlices = SliceList . filter ((== Deb) . sourceType . sliceSource) . slices

inexactPathSlices :: SliceList -> SliceList
inexactPathSlices = SliceList . filter (either (const False) (const True) . sourceDist . sliceSource) . slices

releaseSlices :: ReleaseName -> SliceList -> SliceList
releaseSlices release list =
    SliceList . filter (isRelease . sourceDist . sliceSource) $ (slices list)
    where isRelease = either (const False) (\ (x, _) -> x == release)

appendSliceLists :: [SliceList] -> SliceList
appendSliceLists lists =
    SliceList { slices = concat (map slices lists) }

{-
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
         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri </> "dists" </> unpack name </> "Release"}
-}
