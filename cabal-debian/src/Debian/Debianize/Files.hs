-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Files
    ( debianizationFileMap    -- Used by Debian.Debianize.Atoms and Debian.Debianize.Tests
    , getRulesHead
    ) where

import Control.Monad.State (get, modify)
import Control.Monad.Trans (MonadIO)
import Data.Lens.Lazy (getL, setL)
import Data.List as List (map, unlines)
import Data.Map as Map (Map, toList, fromListWithKey, mapKeys)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set as Set (toList, member)
import Data.Text as Text (Text, pack, unpack, lines, unlines, strip, null)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Facts.Types as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                               VersionControlSpec(..), XField(..), XFieldDest(..), Atoms)
import Debian.Debianize.Goodies (makeRulesHead)
import qualified Debian.Debianize.Facts.Lenses as Lenses
    (compat, sourceFormat, watch, changelog, control, postInst, postRm, preInst, preRm,
     intermediateFiles, install, installDir, installInit, logrotateStanza, link,
     rulesHead, rulesFragments, copyright)
import Debian.Debianize.Facts.Monad (DebT, evalDebT)
import Debian.Debianize.Utility (showDeps')
import Debian.Relation (Relations, BinPkgName(BinPkgName))
import Prelude hiding (init, unlines, writeFile, log)
--import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | If the rulesHead value is still Nothing, construct a suitable
-- value, save it in the DebT state and return it.
getRulesHead :: MonadIO m => DebT m ()
getRulesHead =
    do oldText <- get >>= return . getL Lenses.rulesHead
       newText <- maybe makeRulesHead return oldText
       modify (setL Lenses.rulesHead (Just newText))

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)
debianizationFileMap :: Atoms -> IO (Map FilePath Text)
debianizationFileMap deb =
    do -- here <- getCurrentDirectory
       let d = getL Lenses.control deb
           log = getL Lenses.changelog deb
           compat = getL Lenses.compat deb
           copyrt = getL Lenses.copyright deb
       prs <- sequence [return ("debian/control", pack (show (pretty (controlFile d)))),
                        return ("debian/changelog", pack (show (pretty (fromMaybe (error "No changelog in debianization") log)))),
                        evalDebT rules deb,
                        return ("debian/compat", pack (show (fromMaybe (error "Missing DebCompat atom - is debhelper installed?") $ compat) <> "\n")),
                        return ("debian/copyright", either (\ x -> pack (show (pretty x))) id (fromMaybe (error "No DebCopyright atom") $ copyrt))]
       prs' <- sequence
                 [return prs,
                  evalDebT sourceFormatFiles deb,
                  evalDebT watchFile deb,
                  evalDebT installs deb,
                  evalDebT dirs deb,
                  evalDebT init deb,
                  evalDebT logrotate deb,
                  evalDebT links deb,
                  evalDebT postinstFiles deb,
                  evalDebT postrmFiles deb,
                  evalDebT preinstFiles deb,
                  evalDebT prermFiles deb,
                  evalDebT intermediates deb] >>= return . concat
       return $ Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) prs'


sourceFormatFiles :: Monad m => DebT m [(FilePath, Text)]
sourceFormatFiles = get >>= return . maybe [] (\ x -> [("debian/source/format", pack (show (pretty x)))]) . getL Lenses.sourceFormat

watchFile :: Monad m => DebT m [(FilePath, Text)]
watchFile = get >>= return . maybe [] (\ x -> [("debian/watch", x)]) . getL Lenses.watch

intermediates :: Monad m => DebT m [(FilePath, Text)]
intermediates = get >>= return . Set.toList . getL Lenses.intermediateFiles

installs :: Monad m => DebT m [(FilePath, Text)]
installs =
    get >>= \ deb -> return $
    map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (src, dst) -> src <> " " <> dst) (Set.toList pairs))))) $
    Map.toList $
    mapKeys pathf $
    getL Lenses.install deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".install"

dirs :: Monad m => DebT m [(FilePath, Text)]
dirs =
    get >>= \ deb -> return $
    map (\ (path, dirs') -> (path, pack (List.unlines (Set.toList dirs')))) $ Map.toList $ mapKeys pathf $ getL Lenses.installDir deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".dirs"

init :: Monad m => DebT m [(FilePath, Text)]
init =
    get >>= \ deb -> return $
    Map.toList $ mapKeys pathf $ getL Lenses.installInit deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: Monad m => DebT m [(FilePath, Text)]
logrotate =
    get >>= \ deb -> return $
    map (\ (path, stanzas) -> (path, Text.unlines (Set.toList stanzas))) $ Map.toList $ mapKeys pathf $ getL Lenses.logrotateStanza deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: Monad m => DebT m [(FilePath, Text)]
links =
    get >>= \ deb -> return $
    map (\ (path, pairs) -> (path, pack (List.unlines (map (\ (loc, txt) -> loc ++ " " ++ txt) (Set.toList pairs))))) $
    Map.toList $
    mapKeys pathf $
    getL Lenses.link deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinstFiles :: Monad m => DebT m [(FilePath, Text)]
postinstFiles =
    get >>= \ deb -> return $
    Map.toList $ mapKeys pathf $ getL Lenses.postInst deb
    where
      pathf (BinPkgName name) = "debian" </> show (pretty name) ++ ".postinst"

postrmFiles :: Monad m => DebT m [(FilePath, Text)]
postrmFiles =
    get >>= \ deb -> return $
    Map.toList $ mapKeys pathf $ getL Lenses.postRm deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinstFiles :: Monad m => DebT m [(FilePath, Text)]
preinstFiles =
    get >>= \ deb -> return $
    Map.toList $ mapKeys pathf $ getL Lenses.preInst deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prermFiles :: Monad m => DebT m [(FilePath, Text)]
prermFiles =
    get >>= \ deb -> return $
    Map.toList $ mapKeys pathf $ getL Lenses.preRm deb
    where
      pathf name = "debian" </> show (pretty name) ++ ".prerm"

rules :: MonadIO m => DebT m (FilePath, Text)
rules =
    do rh <- get >>= return . getL Lenses.rulesHead >>= maybe makeRulesHead return
       rl <- get >>= return . reverse . Set.toList . getL Lenses.rulesFragments
       return ("debian/rules", Text.unlines (rh : rl))

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
