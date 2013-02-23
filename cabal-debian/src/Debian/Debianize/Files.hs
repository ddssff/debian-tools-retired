-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Files
    ( toFileMap
    ) where

import Data.Lens.Lazy (getL)
import Data.List as List (map, unlines)
import Data.Map as Map (Map, toList, fromListWithKey, mapKeys)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set as Set (toList, member)
import Data.Text as Text (Text, pack, unpack, lines, unlines, strip, null)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Atoms (Atoms, compat, sourceFormat, watch, changelog, control, postInst, postRm, preInst, preRm,
                               intermediateFiles, install, installDir, installInit, logrotateStanza, link,
                               rulesHead, rulesFragments, copyright)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                               VersionControlSpec(..), XField(..), XFieldDest(..))
import Debian.Debianize.Dependencies (getRulesHead)
import Debian.Debianize.Utility (showDeps')
import Debian.Relation (Relations, BinPkgName(BinPkgName))
import Prelude hiding (init, unlines, writeFile)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

sourceFormatFiles :: Atoms -> [(FilePath, Text)]
sourceFormatFiles deb = maybe [] (\ x -> [("debian/source/format", pack (show (pretty x)))]) (getL sourceFormat deb)

watchFile :: Atoms -> [(FilePath, Text)]
watchFile deb = maybe [] (\ x -> [("debian/watch", x)]) (getL watch deb)

intermediates :: Atoms -> [(FilePath, Text)]
intermediates deb = Set.toList $ getL intermediateFiles deb

installs :: Atoms -> [(FilePath, Text)]
installs deb =
    map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (src, dst) -> src <> " " <> dst) (Set.toList pairs))))) $
    Map.toList $
    mapKeys pathf $
    getL install deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".install"

dirs :: Atoms -> [(FilePath, Text)]
dirs deb =
    map (\ (path, dirs) -> (path, pack (List.unlines (Set.toList dirs)))) $ Map.toList $ mapKeys pathf $ getL installDir deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".dirs"

init :: Atoms -> [(FilePath, Text)]
init deb =
    Map.toList $ mapKeys pathf $ getL installInit deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: Atoms -> [(FilePath, Text)]
logrotate deb =
    map (\ (path, stanzas) -> (path, Text.unlines (Set.toList stanzas))) $ Map.toList $ mapKeys pathf $ getL logrotateStanza deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: Atoms -> [(FilePath, Text)]
links deb =
    map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (loc, txt) -> loc ++ " " ++ txt) (Set.toList pairs))))) $
    Map.toList $
    mapKeys pathf $
    getL link deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinstFiles :: Atoms -> [(FilePath, Text)]
postinstFiles deb =
    Map.toList $ mapKeys pathf $ getL postInst deb
    where
      pathf (BinPkgName name) = "debian" </> show (pretty name) ++ ".postinst"

postrmFiles :: Atoms -> [(FilePath, Text)]
postrmFiles deb =
    Map.toList $ mapKeys pathf $ getL postRm deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinstFiles :: Atoms -> [(FilePath, Text)]
preinstFiles deb =
    Map.toList $ mapKeys pathf $ getL preInst deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prermFiles :: Atoms -> [(FilePath, Text)]
prermFiles deb =
    Map.toList $ mapKeys pathf $ getL preRm deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".prerm"

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)
toFileMap :: Atoms -> Map FilePath Text
toFileMap atoms =
    Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) $
      [("debian/control", pack (show (pretty (controlFile d)))),
       ("debian/changelog", pack (show (pretty (fromMaybe (error "Missing debian/changelog") (getL changelog atoms))))),
       ("debian/rules", rules atoms),
       ("debian/compat", pack (show (fromMaybe (error "Missing DebCompat atom") $ getL compat atoms) <> "\n")),
       ("debian/copyright", either (\ x -> pack (show (pretty x))) id (fromMaybe (error "No DebCopyright atom") $ getL copyright atoms))] ++
      sourceFormatFiles atoms ++
      watchFile atoms ++
      installs atoms ++
      dirs atoms ++
      init atoms ++
      logrotate atoms ++
      links atoms ++
      postinstFiles atoms ++
      postrmFiles atoms ++
      preinstFiles atoms ++
      prermFiles atoms ++
      intermediates atoms
    where d = getL control atoms

rules :: Atoms -> Text
rules deb = Text.unlines (maybe (getRulesHead deb) id (getL rulesHead deb) : reverse (Set.toList (getL rulesFragments deb)))

controlFile :: SourceDebDescription -> Control' String
controlFile src =
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
            mField "Standards-Version" (standardsVersion src) ++
            mField "Homepage" (homepage src) ++
            List.map vcsField (Set.toList (vcsFields src)) ++
            List.map xField (Set.toList (xFields src))) :
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
            relFields (relations bin) ++
            [Field ("Description", " " ++ unpack (ensureDescription (description bin)))])
          where
            ensureDescription t =
                case Text.lines t of
                  [] -> "No description available."
                  (short : long) | Text.null (strip short) -> Text.unlines ("No short description available" : long)
                  _ -> t
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
