module Debian.Repo.PackageIndex
    ( packageIndexName
    , packageIndexPath
    , packageIndexDir
    , packageIndexPathList
    , packageIndexDirList
    , packageIndexList
    , sourceIndexList
    , binaryIndexList
    , releaseDir
    , showIndexBrief
    , debSourceFromIndex
    ) where

import Debian.Arch (Arch(..), prettyArch)
import Debian.Release (releaseName', sectionName')
import Debian.Sources (SourceType(..), DebSource(..))
import Debian.Repo.Types ( PackageIndex(..), Release(releaseRepo), Repo(repoURI), releaseName, releaseComponents, releaseArchitectures )
import System.FilePath ( (</>) )

packageIndexName :: PackageIndex -> FilePath
packageIndexName index =
    case packageIndexArch index of
      Source -> "Sources"
      _ -> "Packages"

packageIndexPath :: PackageIndex -> FilePath
packageIndexPath index = packageIndexDir index ++ "/" ++ packageIndexName index

packageIndexDir :: PackageIndex -> FilePath
packageIndexDir index =
    case packageIndexArch index of
      Source -> releaseDir (packageIndexRelease index) ++ "/" ++ sectionName' (packageIndexComponent index) ++ "/source"
      _ -> (releaseDir (packageIndexRelease index) ++ "/" ++
            sectionName' (packageIndexComponent index) ++
            -- Will prettyArch give us linux-amd64 when we just want amd64?
            "/binary-" ++ show (prettyArch (packageIndexArch index)))

releaseDir :: Release -> String
releaseDir release = "dists/" ++ (releaseName' . releaseName $ release)

packageIndexPathList :: Release -> [FilePath]
packageIndexPathList release = map packageIndexPath . packageIndexList $ release

packageIndexDirList :: Release -> [FilePath]
packageIndexDirList release = map packageIndexDir . packageIndexList $ release

packageIndexList :: Release -> [PackageIndex]
packageIndexList release = sourceIndexList release ++ binaryIndexList release

sourceIndexList :: Release -> [PackageIndex]
sourceIndexList release =
    map componentIndex (releaseComponents release)
    where componentIndex component = PackageIndex { packageIndexRelease = release
                                                  , packageIndexComponent = component
                                                  , packageIndexArch = Source }

binaryIndexList :: Release -> [PackageIndex]
binaryIndexList release =
    concat . map componentIndexes $ (releaseComponents release)
    where 
      --componentIndexes :: Section -> [PackageIndex]
      componentIndexes component =
          map archIndex (filter (/= Source) (releaseArchitectures release))
          where
            --archIndex :: Arch -> PackageIndex
            archIndex arch = PackageIndex { packageIndexRelease = release
                                          , packageIndexComponent = component
                                          , packageIndexArch = arch }

showIndexBrief :: PackageIndex -> String
showIndexBrief index =
    (releaseName' . releaseName $ release) </> sectionName' (packageIndexComponent index) </> showArch (packageIndexArch index)
    where release = packageIndexRelease index
          showArch Source = "source"
          showArch All = "all"
          showArch x@(Binary _ _) = "binary-" ++ show (prettyArch x)

debSourceFromIndex :: PackageIndex -> DebSource
debSourceFromIndex index =
    DebSource {sourceType = typ,
               sourceUri = repoURI repo,
               sourceDist = Right (dist, components)}
    where
      typ = case arch of (Binary _ _) -> Deb; Source -> DebSrc; All -> Deb
      arch = packageIndexArch index
      dist = releaseName release
      components = releaseComponents release
      repo = releaseRepo release
      release = packageIndexRelease index
