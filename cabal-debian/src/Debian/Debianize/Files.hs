-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Files
    ( debianizationFileMap
    ) where

import Control.Applicative ((<$>))
import Control.Monad.State (get, modify)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Lens.Lazy (getL, setL, access)
import Data.List as List (map, unlines)
import Data.Map as Map (Map, toList, fromListWithKey, mapKeys)
import Data.Maybe
import Data.Monoid ((<>), mempty)
import Data.Set as Set (toList, member)
import Data.Text as Text (Text, pack, unpack, lines, unlines, strip, null)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Types as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                         VersionControlSpec(..), XField(..), XFieldDest(..))
import Debian.Debianize.Goodies (makeRulesHead)
import qualified Debian.Debianize.Lenses as Lenses
    (compat, sourceFormat, watch, changelog, control, postInst, postRm, preInst, preRm,
     intermediateFiles, install, installDir, installInit, logrotateStanza, link,
     rulesHead, rulesFragments, copyright, license, licenseFile)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Utility (showDeps')
import Debian.Relation (Relations, BinPkgName(BinPkgName))
import Distribution.License (License(AllRightsReserved))
import Prelude hiding (init, unlines, writeFile, log)
--import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

type FilesT m = WriterT [(FilePath, Text)] (DebT m)

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)

debianizationFileMap :: (Monad m, Functor m) => DebT m (Map FilePath Text)
debianizationFileMap =
    fmap (Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b)) $ execWriterT $
    do -- here <- liftIO getCurrentDirectory
       tell =<< control
       tell =<< changelog
       tell =<< rules
       tell =<< compat
       tell =<< copyright
       tell =<< sourceFormatFiles
       tell =<< watchFile
       tell =<< installs
       tell =<< dirs
       tell =<< init
       tell =<< logrotate
       tell =<< links
       tell =<< postinstFiles
       tell =<< postrmFiles
       tell =<< preinstFiles
       tell =<< prermFiles
       tell =<< intermediates

sourceFormatFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
sourceFormatFiles =
    maybe [] (\ x -> [("debian/source/format", pack (show (pretty x)))]) <$> (lift $ access Lenses.sourceFormat)

watchFile :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
watchFile = maybe [] (\ x -> [("debian/watch", x)]) <$> (lift $ access Lenses.watch)

intermediates :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
intermediates = Set.toList <$> (lift $ access Lenses.intermediateFiles)

installs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
installs =
    (map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (src, dst) -> src <> " " <> dst) (Set.toList pairs))))) . Map.toList . mapKeys pathf) <$> (lift $ access Lenses.install)
    where
      pathf name = "debian" </> show (pretty name) ++ ".install"

dirs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
dirs =
    (map (\ (path, dirs') -> (path, pack (List.unlines (Set.toList dirs')))) . Map.toList . mapKeys pathf) <$> (lift $ access Lenses.installDir)
    where
      pathf name = "debian" </> show (pretty name) ++ ".dirs"

init :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
init =
    (Map.toList . mapKeys pathf) <$> (lift $ access Lenses.installInit)
    where
      pathf name = "debian" </> show (pretty name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
logrotate =
    (map (\ (path, stanzas) -> (path, Text.unlines (Set.toList stanzas))) . Map.toList . mapKeys pathf) <$> (lift $ access Lenses.logrotateStanza)
    where
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
links =
    (map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (loc, txt) -> loc ++ " " ++ txt) (Set.toList pairs))))) . Map.toList . mapKeys pathf) <$> (lift $ access Lenses.link)
    where
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postinstFiles =
     (Map.toList . mapKeys pathf) <$> (lift $ access Lenses.postInst)
    where
      pathf (BinPkgName name) = "debian" </> show (pretty name) ++ ".postinst"

postrmFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postrmFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access Lenses.postRm)
    where
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
preinstFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access Lenses.preInst)
    where
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prermFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
prermFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access Lenses.preRm)
    where
      pathf name = "debian" </> show (pretty name) ++ ".prerm"

rules :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
rules =
    do rh <- lift (access Lenses.rulesHead) >>= maybe (lift makeRulesHead) return
       rl <- (reverse . Set.toList) <$> lift (access Lenses.rulesFragments)
       return [("debian/rules", Text.unlines (rh : rl))]

changelog :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
changelog =
    do log <- lift $ access Lenses.changelog
       return [("debian/changelog", pack (show (pretty (fromMaybe (error "No changelog in debianization") log))))]

control :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
control =
    do d <- lift $ access Lenses.control
       return [("debian/control", pack (show (pretty (controlFile d))))]

compat :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
compat =
    do text <- lift $ access Lenses.compat
       return [("debian/compat", pack (show (fromMaybe (error "Missing DebCompat atom - is debhelper installed?") $ text) <> "\n"))]

copyright :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
copyright =
    do copyrt <- lift $ access Lenses.copyright
       license <- lift $ access Lenses.license
       licenseFile <- lift $ access Lenses.licenseFile
       return [("debian/copyright", case (licenseFile, copyrt, license) of
                                      (Just x, _, _) -> x <> "\n"
                                      (_, Just x, y) -> x <> "\n" <> maybe mempty (\ z -> pack ("License: " <> (show  (pretty z)) <> "\n")) y
                                      (_, _, Just x) -> pack ("License: " <> show (pretty x) <> "\n")
                                      _ -> pack ("License: " <> show (pretty AllRightsReserved)))]

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
    depField "Provides" (provides_ rels) ++
    depField "Replaces" (replaces_ rels) ++
    depField "Built-Using" (builtUsing rels)

depField :: [Char] -> Relations -> [Field' [Char]]
depField tag rels = case rels of [] -> []; _ -> [Field (tag, " " ++ showDeps' (tag ++ ":") rels)]
