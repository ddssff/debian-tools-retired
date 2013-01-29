-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Files
    ( toFileMap
    ) where

-- import Debug.Trace

import Data.List as List (map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Monoid, (<>), mempty)
import Data.Set as Set (toList, member)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.AtomsClass (HasAtoms, DebAtomKey(..), DebAtom(..))
import Debian.Debianize.AtomsType (lookupAtom, foldAtoms, changeLog, compat, copyright, rulesHead)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                               VersionControlSpec(..), XField(..), XFieldDest(..))
import Debian.Debianize.Utility (showDeps')
import Debian.Relation (Relations)
import Prelude hiding (init, unlines, writeFile)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

sourceFormat :: HasAtoms atoms => atoms -> [(FilePath, Text)]
sourceFormat deb =
    maybe [] (\ x -> [("debian/source/format", pack (show (pretty x)))]) (lookupAtom Source f deb)
    where
      f (DebSourceFormat x) = Just x
      f _ = Nothing

watch :: HasAtoms atoms => atoms -> [(FilePath, Text)]
watch deb =
    maybe [] (\ x -> [("debian/watch", x)]) (lookupAtom Source f deb)
    where
      f (DebWatch x) = Just x
      f _ = Nothing

intermediates :: HasAtoms atoms => atoms -> [(FilePath, Text)]
intermediates deb =
    foldAtoms atomf [] deb
    where
      atomf Source (DHIntermediate path text) files = (path,  text) : files
      atomf _ _ files = files

installs :: HasAtoms atoms => atoms -> [(FilePath, Text)]
installs deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHInstall src dst) files = Map.insertWith with1 (pathf name)  (pack (src ++ " " ++ dst)) files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".install"

dirs :: HasAtoms atoms => atoms -> [(FilePath, Text)]
dirs deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHInstallDir d) files = Map.insertWith with1 (pathf name) (pack d) files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".dirs"

with1 :: (IsString m, Monoid m) => m -> m -> m
with1 old new = old <> "\n" <> new

with2 :: String -> t -> t1 -> t2
with2 msg _ _ = error msg

init :: HasAtoms atoms => atoms -> [(FilePath, Text)]
init deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHInstallInit t) files = Map.insertWith (with2 "init") (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: HasAtoms atoms => atoms -> [(FilePath, Text)]
logrotate deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHLogrotateStanza t) files = Map.insertWith with1 (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: HasAtoms atoms => atoms -> [(FilePath, Text)]
links deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHLink loc txt) files = Map.insertWith with1 (pathf name) (pack (loc ++ " " ++ txt)) files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinst :: HasAtoms atoms => atoms -> [(FilePath, Text)]
postinst deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPostInst t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".postinst"

postrm :: HasAtoms atoms => atoms -> [(FilePath, Text)]
postrm deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPostRm t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinst :: HasAtoms atoms => atoms -> [(FilePath, Text)]
preinst deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPreInst t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prerm :: HasAtoms atoms => atoms -> [(FilePath, Text)]
prerm deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPreRm t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".prerm"

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)
toFileMap :: HasAtoms atoms => atoms -> SourceDebDescription -> Map.Map FilePath Text
toFileMap atoms d =
    Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) $
      [("debian/control", pack (show (pretty (control d)))),
       ("debian/changelog", pack (show (pretty (changeLog atoms)))),
       ("debian/rules", rules atoms),
       ("debian/compat", pack (show (compat (error "Missing DebCompat atom") atoms) <> "\n")),
       ("debian/copyright", either (\ x -> pack (show x) <> "\n") id (copyright (error "No DebCopyright atom") atoms))] ++
      sourceFormat atoms ++
      watch atoms ++
      installs atoms ++
      dirs atoms ++
      init atoms ++
      logrotate atoms ++
      links atoms ++
      postinst atoms ++
      postrm atoms ++
      preinst atoms ++
      prerm atoms ++
      intermediates atoms

rules :: HasAtoms atoms => atoms -> Text
rules deb =
    foldAtoms append (rulesHead deb) deb
    where
      append Source (DebRulesFragment x) text = text <> "\n" <> x
      append _ _ text = text

control :: SourceDebDescription -> Control' String
control src =
    Control
    { unControl =
          (Paragraph
           ([Field ("Source", " " ++ show (pretty (source src))),
             Field ("Maintainer", " " <> show (pretty (maintainer src)))] ++
            lField "Uploaders" (uploaders src) ++
            (case dmUploadAllowed src of True -> [Field ("DM-Upload-Allowed", " yes")]; False -> []) ++
            mField "Priority" (priority src) ++
            mField "Section" (section src) ++
            depField "Build-Depends" (buildDepends src) ++
            depField "Build-Depends-Indep" (buildDependsIndep src) ++
            depField "Build-Conflicts" (buildConflicts src) ++
            depField "Build-Conflicts-Indep" (buildConflictsIndep src) ++
            [Field ("Standards-Version", " " <> show (pretty (standardsVersion src)))] ++
            mField "Homepage" (homepage src) ++
            List.map vcsField (toList (vcsFields src)) ++
            List.map xField (toList (xFields src))) :
           List.map binary (binaryPackages src))
    }
    where
      binary :: BinaryDebDescription -> Paragraph' String
      binary bin =
          Paragraph
           ([Field ("Package", " " ++ show (pretty (package bin))),
             Field ("Architecture", " " ++ show (pretty (architecture bin)))] ++
            mField "Section" (binarySection bin) ++
            mField "Priority" (binaryPriority bin) ++
            bField "Essential" (essential bin) ++
            relFields (relations bin))
      mField tag = maybe [] (\ x -> [Field (tag, " " <> show (pretty x))])
      bField tag flag = if flag then [Field (tag, " yes")] else []
      lField _ [] = []
      lField tag xs = [Field (tag, " " <> show (pretty xs))]
      vcsField (VCSBrowser t) = Field ("Vcs-Browser", " " ++ unpack t)
      vcsField (VCSArch t) = Field ("Vcs-Arch", " " ++ unpack t)
      vcsField (VCSBzr t) = Field ("Vcs-Bzr", " " ++ unpack t)
      vcsField (VCSCvs t) = Field ("Vcs-Cvs", " " ++ unpack t)
      vcsField (VCSDarcs t) = Field ("Vcs-Darcs", " " ++ unpack t)
      vcsField (VCSGit t) = Field ("Vcs-Git", " " ++ unpack t)
      vcsField (VCSHg t) = Field ("Vcs-Hg", " " ++ unpack t)
      vcsField (VCSMtn t) = Field ("Vcs-Mtn", " " ++ unpack t)
      vcsField (VCSSvn t) = Field ("Vcs-Svn", " " ++ unpack t)
      xField (XField dests tag t) = Field (unpack ("X" <> showDests dests <> "-" <> tag), unpack (" " <> t))
      showDests s = if member B s then "B" else "" <>
                    if member S s then "S" else "" <>
                    if member C s then "C" else ""

relFields :: PackageRelations -> [Field' [Char]]
relFields rels =
    depField "Depends" (depends rels) ++
    depField "Recommends" (recommends rels) ++
    depField "Suggests" (suggests rels) ++
    depField "Pre-Depends" (preDepends rels) ++
    depField "Breaks" (breaks rels) ++
    depField "Conflicts" (conflicts rels) ++
    depField "Provides" (provides rels) ++
    depField "Replaces" (replaces rels) ++
    depField "Built-Using" (builtUsing rels)

depField :: [Char] -> Relations -> [Field' [Char]]
depField tag rels = case rels of [] -> []; _ -> [Field (tag, " " ++ showDeps' (tag ++ ":") rels)]
