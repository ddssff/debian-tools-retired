-- | Preliminary.
module Distribution.Debian.Control
    ( Debianization(..)
    , DebControl(..)
    , fromControl
    , SourceSpec(..)
    , BinarySpec(..)
    , Architecture(..)
    ) where

import Control.Applicative.Error (maybeRead)
import Data.List
import Data.Maybe
import Data.Version (showVersion)
import Debian.Changes (ChangeLogEntry(..))
import Debian.Control
import Debian.Relation (Relation, BinPkgName(BinPkgName), PkgName(PkgName), SrcPkgName(SrcPkgName), parseRelations)
import qualified Debian.Relation as D
import Debian.Version (DebianVersion)
import Debian.Version.String
import Distribution.Debian.Config (Flags(..), missingDependencies')
import Distribution.Debian.DebHelper (DebFile)
import Distribution.Debian.Dependencies (PackageType(..), debianSourcePackageName, debianDocPackageName)
import Distribution.Debian.Relations (buildDependencies, docDependencies, allBuildDepends, versionSplits)
import Distribution.Debian.Utility
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (PackageDescription(package, maintainer, homepage, description, author, pkgUrl, synopsis))
import Network.URI (URI, parseURI)
import Prelude hiding (catch)

data Debianization
    = Debianization
      { controlFile :: DebControl -- Control' String
      , changeLog :: [ChangeLogEntry]
      , otherFiles :: [DebFile] }

data DebControl =
    DebControl
    { sourceSpec :: SourceSpec
    , binarySpecs :: [BinarySpec]
    }

fromControl :: Control' String -> DebControl
fromControl (Control {unControl = source : binaries@(_ : _)}) =
    DebControl
    { sourceSpec = fromSourceSection source
    , binarySpecs = map fromBinarySection binaries }
fromControl (Control {unControl = _}) = error "One Source and at least one Package paragraph required"

fromSourceSection :: Paragraph' String -> SourceSpec
fromSourceSection (Paragraph fields) =
    SourceSpec { sourcePackageName = SrcPkgName (PkgName (findField fields "Source"))
               , sourcePriority = findField fields "Priority"
               , sourceSection = findField fields "Section"
               , sourceMaintainer = findField fields "Maintainer"
               , buildDepends = maybe [] (either (error "parse failure in Build-Depends") id . parseRelations) (findFieldMaybe fields "Build-Depends")
               , buildDependsIndep = maybe [] (either (error "parse failure in Build-Depends-Indep") id . parseRelations) (findFieldMaybe fields "Build-Depends-Indep")
               , standardsVersion = parseDebianVersion (findField fields "Standards-Version")
               , homePage = maybe (error "Invalid URI in Homepage") parseURI (findFieldMaybe fields "Homepage")
               }

fromBinarySection :: Paragraph' String -> BinarySpec
fromBinarySection = undefined

findFieldMaybe :: [Field' String] -> String -> Maybe String
findFieldMaybe fs name =
    case catMaybes (map (\ (Field (name', value)) -> if name' == name then Just value else Nothing) fs) of
      [x] -> Just x
      _ -> Nothing

findField :: [Field' String] -> String -> String
findField fs name = fromMaybe (error "findField") (findFieldMaybe fs name)

findFieldReadMaybe :: Read a => [Field' String] -> String -> Maybe a
findFieldReadMaybe fs name =
    case findFieldMaybe fs name of
      Just s -> maybeRead s
      Nothing -> Nothing

findFieldRead :: Read a => [Field' String] -> String -> a
findFieldRead fs name = read (findField fs name )

data SourceSpec =
    SourceSpec
    { sourcePackageName :: SrcPkgName
    , sourcePriority :: String
    , sourceSection :: String
    , sourceMaintainer :: String
    , buildDepends :: [[Relation]] -- omit if empty
    , buildDependsIndep :: [[Relation]] -- omit if empty
    , standardsVersion :: DebianVersion
    , homePage :: Maybe URI
    }

data BinarySpec =
    BinarySpec
    { binaryPackageName :: BinPkgName
    , architecture :: Architecture
    , binarySection :: Maybe String
    , dependencies :: [[Relation]] -- omit if empty
    , debSynopsis :: String
    , debDescription :: String
    , debRecommends :: [[Relation]] -- omit if empty
    , debSuggests :: [[Relation]] -- omit if empty
    , debProvides :: [[Relation]] -- omit if empty
    }

data Architecture
    = Any
    | All
    | Arch String

makeSourceSpec pkgDesc flags compiler maint =
          SourceSpec
          { sourcePackageName = debianSourcePackageName' pkgDesc
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities
          , sourcePriority = "optional"
            -- See http://www.debian.org/doc/debian-policy/ch-archive.html#s-subsections
          , sourceSection = "haskell"
          , sourceMaintainer = maint
          , buildDepends = filterMissing (missingDependencies' flags) (debianBuildDeps ++ map anyrel (buildDeps flags))
          , buildDependsIndep = filterMissing (missingDependencies' flags) debianBuildDepsIndep
          , standardsVersion = parseDebianVersion "3.9.3"
          , homePage = if homepage pkgDesc == ""
                       then parseURI ("http://hackage.haskell.org/package/" ++ unPackageName (pkgName $ package pkgDesc))
                       else parseURI (homepage pkgDesc) }
    where
      -- The haskell-cdbs package contains the hlibrary.mk file with
      -- the rules for building haskell packages.
      debianBuildDeps :: D.Relations
      debianBuildDeps =
          nub $
          [[D.Rel (D.BinPkgName (D.PkgName "debhelper")) (Just (D.GRE (parseDebianVersion "7.0"))) Nothing],
           [D.Rel (D.BinPkgName (D.PkgName "haskell-devscripts")) (Just (D.GRE (parseDebianVersion "0.8"))) Nothing],
           anyrel "cdbs",
           anyrel "ghc"] ++
          (if debLibProf flags then [anyrel "ghc-prof"] else []) ++
          (concat . map (buildDependencies (epochMap flags) (execMap flags) compiler) . allBuildDepends (extraLibMap flags) $ pkgDesc)
      debianBuildDepsIndep :: D.Relations
      debianBuildDepsIndep =
          nub $
          [anyrel "ghc-doc"] ++
          (concat . map (docDependencies (epochMap flags) compiler) . allBuildDepends (extraLibMap flags) $ pkgDesc)

makeLibrarySpec pkgDesc flags arch typ debianName =
          BinarySpec
          { binaryPackageName = debianName pkgDesc
          , architecture = arch
          , binarySection = Nothing
          , dependencies = filterMissing
                             (missingDependencies' flags)
                             ((if typ == Development then [anyrel "${shlibs:Depends}"] ++ map anyrel (extraDevDeps flags) else []) ++
                              ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                               extraDeps (debianName pkgDesc) (binaryPackageDeps flags)))
          , debSynopsis = synopsis
          , debDescription = unlines description
          , debRecommends = [[D.Rel (BinPkgName (PkgName "${haskell:Recommends}")) Nothing Nothing]]
          , debSuggests = [[D.Rel (BinPkgName (PkgName "${haskell:Suggests}")) Nothing Nothing]]
          , debProvides = [[D.Rel (BinPkgName (PkgName "${haskell:Provides}")) Nothing Nothing]]
          }
          where (synopsis : description) = lines (libraryDescription pkgDesc typ)

makeDocSpecsParagraph pkgDesc flags =
          BinarySpec
          { binaryPackageName = debianDocPackageName' pkgDesc
          , architecture = All
          , binarySection = Just "doc"
          , dependencies = filterMissing (missingDependencies' flags)
                             ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                              extraDeps (debianDocPackageName' pkgDesc) (binaryPackageDeps flags))
          , debSynopsis = synopsis
          , debDescription = unlines description
          , debRecommends = [[D.Rel (BinPkgName (PkgName "${haskell:Recommends}")) Nothing Nothing]]
          , debSuggests = [[D.Rel (BinPkgName (PkgName "${haskell:Suggests}")) Nothing Nothing]]
          , debProvides = []
          }
          where (synopsis : description) = lines (libraryDescription pkgDesc Documentation)

debianDescription pkgDesc =
          (unwords . words . synopsis $ pkgDesc) ++
          case description pkgDesc of
            "" -> ""
            text ->
                let text' = text ++ "\n" ++
                            list "" ("\n Author: " ++) (author pkgDesc) ++
                            list "" ("\n Upstream-Maintainer: " ++) (maintainer pkgDesc) ++
                            list "" ("\n Url: " ++) (pkgUrl pkgDesc) in
                "\n " ++ (trim . intercalate "\n " . map addDot . lines $ text')
    where
      addDot line = if all (flip elem " \t") line then "." else line

libraryDescription pkgDesc Profiling = debianDescription pkgDesc ++ "\n .\n This package contains the libraries compiled with profiling enabled."
libraryDescription pkgDesc Development = debianDescription pkgDesc ++ "\n .\n This package contains the normal library files."
libraryDescription pkgDesc Documentation = debianDescription pkgDesc ++ "\n .\n This package contains the documentation files."
libraryDescription pkgDesc x = error $ "Unexpected library package name suffix: " ++ show x

list :: b -> ([a] -> b) -> [a] -> b
list d f l = case l of [] -> d; _ -> f l

extraDeps :: D.BinPkgName -> [(D.BinPkgName, D.BinPkgName)] -> [[D.Relation]]
extraDeps p deps =
    case filter ((== p) . fst) deps of
      [] -> []
      pairs -> map (mkDep . snd) pairs
    where mkDep name = [D.Rel name Nothing Nothing]

anyrel :: String -> [D.Relation]
anyrel x = [D.Rel (D.BinPkgName (D.PkgName x)) Nothing Nothing]

-- | Functions that apply the mapping from cabal names to debian names based on version numbers.
debianSourcePackageName' :: PackageDescription -> D.SrcPkgName
debianSourcePackageName' pkgDesc =
         debianSourcePackageName versionSplits (pkgName . package $ pkgDesc)
                                     (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))

debianDocPackageName' :: PackageDescription -> D.BinPkgName
debianDocPackageName' pkgDesc =
         debianDocPackageName versionSplits (pkgName (package pkgDesc))
                                  (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion (package pkgDesc))))))
