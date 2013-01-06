-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Files
    ( toFiles
    ) where

import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Set (toList)
import Data.Text (Text, pack, unlines)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Combinators (deSugarDebianization)
import Debian.Debianize.Types.Atoms (DebAtom(..))
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..))
import Debian.Debianize.Utility (showDeps')
import Debian.Relation (BinPkgName, Relations)
import Prelude hiding (init, unlines)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (pretty)

sourceFormat :: Debianization -> [(FilePath, Text)]
sourceFormat deb =
    case nub (mapMaybe f (atoms deb)) of
      [x] -> [("debian/source/format", x)]
      [] -> []
      _ -> error "Multiple debian/source/format files"
    where
      f :: DebAtom -> Maybe Text
      f (DebSourceFormat x) = Just x
      f _ = Nothing

watch :: Debianization -> [(FilePath, Text)]
watch deb =
    case nub (mapMaybe f (atoms deb)) of
      [x] -> [("debian/watch", x)]
      [] -> []
      _ -> error "Multiple debian/watch files"
    where
      f :: DebAtom -> Maybe Text
      f (DebWatch x) = Just x
      f _ = Nothing

intermediate :: Debianization -> [(FilePath, Text)]
intermediate deb =
    mapMaybe atomf (atoms deb)
    where
      atomf (DHIntermediate path text) = Just (path,  text)
      atomf _ = Nothing

-- | Assemble the atoms into per-package debianization files, merging
-- the text from each.
assemble :: (DebAtom -> Maybe (BinPkgName, [Text])) -> (BinPkgName -> FilePath) -> Debianization -> [(FilePath, Text)]
assemble atomf pathf deb =
    map (\ (name, ts) -> (pathf name, unlines ts))
        (Map.toList (Map.fromListWith (++) (mapMaybe atomf (atoms deb))))

-- | Assemble the atoms into per-package debianization files, allowing
-- only one atom per package.
assemble1 :: (DebAtom -> Maybe (BinPkgName, [Text])) -> (BinPkgName -> FilePath) -> Debianization -> [(FilePath, Text)]
assemble1 atomf pathf deb =
    map test (Map.toList (Map.fromListWith (++) (mapMaybe atomf (atoms deb))))
    where
      test (name, [t]) = (pathf name, t)
      test (name, ts) = error $ "Multiple entries for " ++ show (pretty name) ++ ": " ++ show ts

install :: FilePath -> Debianization -> [(FilePath, Text)]
install build deb =
    assemble atomf (\ name -> "debian" </> show (pretty name) ++ ".install") deb
    where
      atomf (DHInstall name src dst) = Just (name, [pack (src ++ " " ++ dst)])
      atomf (DHInstallCabalExec name exec dst) = Just (name, [pack (build </> exec </> exec ++ " " ++ dst)])
      atomf _ = Nothing

dirs :: Debianization -> [(FilePath, Text)]
dirs deb =
    assemble atomf pathf deb
    where
      atomf (DHInstallDir name d) = Just (name, [pack d])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".dirs"

init :: Debianization -> [(FilePath, Text)]
init deb =
    assemble1 atomf pathf deb
    where
      atomf (DHInstallInit name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".init"

logrotate :: Debianization -> [(FilePath, Text)]
logrotate deb =
    assemble1 atomf pathf deb
    where
      atomf (DHInstallLogrotate name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
link :: Debianization -> [(FilePath, Text)]
link deb =
    assemble atomf pathf deb
    where
      atomf (DHLink name loc txt) = Just (name, [pack (loc ++ " " ++ txt)])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinst :: Debianization -> [(FilePath, Text)]
postinst deb =
    assemble1 atomf pathf deb
    where
      atomf (DHPostInst name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".postinst"

postrm :: Debianization -> [(FilePath, Text)]
postrm deb =
    assemble1 atomf pathf deb
    where
      atomf (DHPostRm name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinst :: Debianization -> [(FilePath, Text)]
preinst deb =
    assemble1 atomf pathf deb
    where
      atomf (DHPreInst name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prerm :: Debianization -> [(FilePath, Text)]
prerm deb =
    assemble1 atomf pathf deb
    where
      atomf (DHPreRm name t) = Just (name, [t])
      atomf _ = Nothing
      pathf name = "debian" </> show (pretty name) ++ ".prerm"

-- | Turn the DebAtoms into a list of files, making sure the text
-- associated with each path is unique.
toFiles :: FilePath -> FilePath -> Debianization -> [(FilePath, Text)]
toFiles build datadir d0 =
    Map.toList $
    Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) $
      [("debian/control", pack (show (pretty (control (sourceDebDescription d))))),
       ("debian/changelog", pack (show (pretty (changelog d)))),
       ("debian/rules", rulesHead d),
       ("debian/compat", pack (show (compat d) <> "\n")),
       ("debian/copyright", either (\ x -> pack (show x) <> "\n") id (Debian.copyright d))] ++
      sourceFormat d ++
      watch d ++
      install build d ++
      dirs d ++
      init d ++
      logrotate d ++
      link d ++
      postinst d ++
      postrm d ++
      preinst d ++
      prerm d ++
      intermediate d
    where
      d = deSugarDebianization build datadir d0

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
            map vcsField (toList (vcsFields src)) ++
            map xField (toList (xFields src))) :
           map binary (binaryPackages src))
    }
    where
      binary :: BinaryDebDescription -> Paragraph' String
      binary bin =
          Paragraph
           ([Field ("Package", " " ++ show (pretty (package bin))),
             Field ("Architecture", " " ++ show (pretty (architecture bin))),
             Field ("Section", " " ++ show (pretty (binarySection bin)))] ++
            mField "Priority" (binaryPriority bin) ++
            bField "Essential" (essential bin) ++
            relFields (relations bin))
      mField tag = maybe [] (\ x -> [Field (tag, " " <> show (pretty x))])
      bField tag flag = if flag then [Field (tag, " yes")] else []
      lField _ [] = []
      lField tag xs = [Field (tag, " " <> show (pretty xs))]
      vcsField _ = error "vcsField"
      xField _ = error "xField"

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

{-
dh_install (1)       - install files into package build directories
dh_installdirs (1)   - create subdirectories in package build directories
dh_link (1)          - create symlinks in package build directories
dh_installinit (1)   - install upstart jobs or init scripts into package build directories
dh_installlogrotate (1) - install logrotate config files


dh (1)               - debhelper command sequencer
dh_apparmor (1)      - reload AppArmor profile and create local include
dh_auto_build (1)    - automatically builds a package
dh_auto_clean (1)    - automatically cleans up after a build
dh_auto_configure (1) - automatically configure a package prior to building
dh_auto_install (1)  - automatically runs make install or similar
dh_auto_test (1)     - automatically runs a package's test suites
dh_autotools-dev_restoreconfig (1) - restore config.sub and config.guess
dh_autotools-dev_updateconfig (1) - update config.sub and config.guess
dh_bash-completion (1) - install bash completions for package
dh_bugfiles (1)      - install bug reporting customization files into package build directories
dh_builddeb (1)      - build Debian binary packages
dh_clean (1)         - clean up package build directories
dh_compress (1)      - compress files and fix symlinks in package build directories
dh_desktop (1)       - deprecated no-op
dh_dkms (1)          - correctly handle DKMS usage by a kernel module package
dh_fixperms (1)      - fix permissions of files in package build directories
dh_gconf (1)         - install GConf defaults files and register schemas
dh_gencontrol (1)    - generate and install control file
dh_haskell_depends (1) - calculates Haskell dependencies on Cabalized libraries
dh_haskell_extra_depends (1) - generate the extra-depends file in Haskell packages
dh_haskell_provides (1) - calculates Haskell virtual package names on Cabalized libraries
dh_haskell_shlibdeps (1) - calculates Haskell external dependencies on Cabalized libraries
dh_icons (1)         - Update Freedesktop icon caches
dh_installcatalogs (1) - install and register SGML Catalogs
dh_installchangelogs (1) - install changelogs into package build directories
dh_installcron (1)   - install cron scripts into etc/cron.*
dh_installdeb (1)    - install files into the DEBIAN directory
dh_installdebconf (1) - install files used by debconf in package build directories
dh_installdocs (1)   - install documentation into package build directories
dh_installemacsen (1) - register an Emacs add on package
dh_installexamples (1) - install example files into package build directories
dh_installgsettings (1) - install GSettings overrides and set dependencies
dh_installifupdown (1) - install if-up and if-down hooks
dh_installinfo (1)   - install info files
dh_installlogcheck (1) - install logcheck rulefiles into etc/logcheck/
dh_installman (1)    - install man pages into package build directories
dh_installmanpages (1) - old-style man page installer (deprecated)
dh_installmenu (1)   - install Debian menu files into package build directories
dh_installmime (1)   - install mime files into package build directories
dh_installmodules (1) - register modules with modutils
dh_installpam (1)    - install pam support files
dh_installppp (1)    - install ppp ip-up and ip-down files
dh_installtex (1)    - register Type 1 fonts, hyphenation patterns, or formats with TeX
dh_installudev (1)   - install udev rules files
dh_installwm (1)     - register a window manager
dh_installxfonts (1) - register X fonts
dh_installxmlcatalogs (1) - install and register XML catalog files
dh_lintian (1)       - install lintian override files into package build directories
dh_listpackages (1)  - list binary packages debhelper will act on
dh_makeshlibs (1)    - automatically create shlibs file and call dpkg-gensymbols
dh_md5sums (1)       - generate DEBIAN/md5sums file
dh_perl (1)          - calculates Perl dependencies and cleans up after MakeMaker
dh_prep (1)          - perform cleanups in preparation for building a binary package
dh_pysupport (1)     - use the python-support framework to handle Python modules
dh_python (1)        - calculates Python dependencies and adds postinst and prerm Python scripts (deprecated)
dh_python2 (1)       - calculates Python dependencies, adds maintainer scripts to byte compile files, etc.
dh_quilt_patch (1)   - apply patches listed in debian/patches/series
dh_quilt_unpatch (1) - unapply patches listed in debian/patches/series
dh_scour (1)         - run scour optimizer on shipped SVG files
dh_scrollkeeper (1)  - deprecated no-op
dh_shlibdeps (1)     - calculate shared library dependencies
dh_strip (1)         - strip executables, shared libraries, and some static libraries
dh_suidregister (1)  - suid registration program (deprecated)
dh_testdir (1)       - test directory before building Debian package
dh_testroot (1)      - ensure that a package is built as root
dh_translations (1)  - perform common translation related operations
dh_ucf (1)           - register configuration files with ucf
dh_undocumented (1)  - undocumented.7 symlink program (deprecated no-op)
dh_usrlocal (1)      - migrate usr/local directories to maintainer scripts

dh_movefiles (1)     - move files out of debian/tmp into subpackages (use dh_install)

-}
