-- | Preliminary.
{-# LANGUAGE FlexibleInstances #-}
module Distribution.Debian.DebHelper
    ( DebAtom(..)
    , changeLog
    , controlFile
    , toFiles
    , tightDependencyFixup
    ) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debian.Changes (ChangeLog(..))
import Debian.Control
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName))
import Prelude hiding (catch, init)
import System.FilePath ((</>), makeRelative)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | The smallest pieces of debhelper information.
data DebAtom
    = DebControl (Control' String)                -- ^ Write the control file (required)
    | DebChangelog ChangeLog                      -- ^ Write the changelog (required)
    | DebRulesHead String                         -- ^ The beginning of debian/rules
    | DebRules String                             -- ^ A Fragment of debian/rules
    | DebCompat Int                               -- ^ Write debian/compat (required?)
    | DebCopyright String                         -- ^ Write debian/copyright (required?)
    | DebSourceFormat String                      -- ^ Write debian/source/format
    | DebWatch String                             -- ^ Write debian/watch
    | DHInstall BinPkgName FilePath FilePath      -- ^ Install a build file into the binary package
    | DHInstallTo BinPkgName FilePath FilePath      -- ^ Install a build file into the binary package at an exact location
    | DHInstallData BinPkgName FilePath FilePath  -- ^ DHInstallTo the package's data directory: /usr/share/package-version/
    | DHFile BinPkgName FilePath String           -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec BinPkgName String FilePath -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo BinPkgName String FilePath -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir BinPkgName FilePath            -- ^ Create a directory in the binary package
    | DHInstallInit BinPkgName String             -- ^ Add an init.d file to the binary package
    | DHInstallLogrotate BinPkgName String        -- ^ Add a logrotate file to the binary package
    | DHLink BinPkgName [(FilePath, FilePath)]    -- ^ Create a symbolic link in the binary package
    | DHPostInst BinPkgName String                -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm BinPkgName String                  -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst BinPkgName String                 -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm BinPkgName String                   -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
    deriving Show

instance Show (Control' String) where
    show _ = "<control file>"

instance Show ChangeLog where
    show _ = "<log entry>"

changeLog :: [DebAtom] -> ChangeLog
changeLog xs =
    case mapMaybe f xs of
      [x] -> x
      [] -> ChangeLog []
      _ -> error "Multiple debian/changelog files"
    where
      f :: DebAtom -> Maybe ChangeLog
      f (DebChangelog x) = Just x
      f _ = Nothing

controlFile :: [DebAtom] -> Control' String
controlFile xs =
    case mapMaybe f xs of
      [x] -> x
      _ -> error "Missing or multiple debian/control files"
    where
      f :: DebAtom -> Maybe (Control' String)
      f (DebControl x) = Just x
      f _ = Nothing

rules :: FilePath -> [DebAtom] -> [String]
rules build xs =
    mapMaybe f xs ++ mapMaybe g xs
    where
      f :: DebAtom -> Maybe String
      f (DebRulesHead x) = Just x
      f _ = Nothing
      g :: DebAtom -> Maybe String
      g (DebRules x) = Just x
      g (DHInstallTo p s d) =
          Just (unlines [ "binary-fixup" </> show (pretty p) ++ "::"
                        , "\tinstall -Dp " ++ s ++ " " ++ "debian" </> show (pretty p) </> makeRelative "/" d ])
      g (DHInstallData p s d) = error "DHInstallData should have been turned into a DHInstallTo"
      g (DHInstallCabalExecTo p n d) =
          Just (unlines [ "binary-fixup" </> show (pretty p) ++ "::"
                        , "\tinstall -Dp " ++ build </> n </> n ++ " " ++ "debian" </> show (pretty p) </> makeRelative "/" d ])
      g _ = Nothing

compat :: [DebAtom] -> Int
compat xs =
    case mapMaybe f xs of
      [n] -> n
      ns -> error $ "Missing or multiple debian/compat values: " ++ show ns
    where
      f :: DebAtom -> Maybe Int
      f (DebCompat x) = Just x
      f _ = Nothing

copyright :: [DebAtom] -> String
copyright xs =
    case mapMaybe f xs of
      [x] -> x
      _ -> error "Missing or multiple debian/copyright files"
    where
      f :: DebAtom -> Maybe String
      f (DebCopyright x) = Just x
      f _ = Nothing

sourceFormat :: [DebAtom] -> String
sourceFormat xs =
    case mapMaybe f xs of
      [x] -> x
      _ -> error "Missing or multiple debian/source/format files"
    where
      f :: DebAtom -> Maybe String
      f (DebSourceFormat x) = Just x
      f _ = Nothing

watch :: [DebAtom] -> String
watch xs =
    case mapMaybe f xs of
      [x] -> x
      _ -> error "Missing or multiple debian/watch files"
    where
      f :: DebAtom -> Maybe String
      f (DebWatch x) = Just x
      f _ = Nothing

install :: FilePath -> [DebAtom] -> [(FilePath, String)]
install build xs =
    map (\ (name, pairs) -> ("debian" </> show (pretty name) ++ ".install", unlines (map (\ (src, dst) -> src ++ " " ++ dst) pairs)))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHInstall name src dst) = Just (name, [(src, dst)])
      f (DHInstallCabalExec name exec dst) = Just (name, [(build </> exec </> exec, dst)])
      f _ = Nothing

-- Install files directly into the binary package.  Actually,
-- installing them immediately doesn't work because the whole thing
-- gets removed before all the dh scripts are called.  We need to make
-- up a temporary file name and add an entry to the .install file.
file :: [DebAtom] -> [(FilePath, String)]
file xs =
    mapMaybe f xs
    where
      f (DHFile pkg dest s) = Just ("debian" </> show (pretty pkg) </> makeRelative "/" dest, s)
      f _ = Nothing

dir :: [DebAtom] -> [(FilePath, String)]
dir xs =
    map (\ (name, ls) -> ("debian" </> show (pretty name) ++ ".dir", unlines ls))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHInstallDir name d) = Just (name, [d])
      f _ = Nothing

init :: [DebAtom] -> [(FilePath, String)]
init xs =
    map (\ (name, [t]) -> ("debian" </> show (pretty name) ++ ".init", t))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHInstallInit name t) = Just (name, [t])
      f _ = Nothing

logrotate :: [DebAtom] -> [(FilePath, String)]
logrotate xs =
    map g (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      g (name, [t]) = ("debian" </> show (pretty name) ++ ".logrotate", t)
      g (name, ts) = error $ "Multiple logrotate entries for " ++ show name ++ ": " ++ show ts
      f (DHInstallLogrotate name t) = Just (name, [t])
      f _ = Nothing

-- | Collect all the links by package and output one file each
link :: [DebAtom] -> [(FilePath, String)]
link xs =
    map doPackage (Map.toList (Map.fromListWith (++) (mapMaybe collect xs)))
    where
      doPackage (name, pairs) = ("debian" </> show (pretty name) ++ ".links", unlines (map (\ (loc, txt) -> loc ++ " " ++ txt) pairs))
      collect (DHLink name pairs) = Just (name, pairs)
      collect _ = Nothing

postinst :: [DebAtom] -> [(FilePath, String)]
postinst xs =
    map (\ (name, [t]) -> ("debian" </> show (pretty name) ++ ".postinst", t))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHPostInst name t) = Just (name, [t])
      f _ = Nothing

postrm :: [DebAtom] -> [(FilePath, String)]
postrm xs =
    map (\ (name, [t]) -> ("debian" </> show (pretty name) ++ ".postrm", t))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHPostRm name t) = Just (name, [t])
      f _ = Nothing

preinst :: [DebAtom] -> [(FilePath, String)]
preinst xs =
    map (\ (name, [t]) -> ("debian" </> show (pretty name) ++ ".preinst", t))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHPreInst name t) = Just (name, [t])
      f _ = Nothing

prerm :: [DebAtom] -> [(FilePath, String)]
prerm xs =
    map (\ (name, [t]) -> ("debian" </> show (pretty name) ++ ".prerm", t))
        (Map.toList (Map.fromListWith (++) (mapMaybe f xs)))
    where
      f (DHPreRm name t) = Just (name, [t])
      f _ = Nothing

-- | Turn the DebAtoms into a list of files, making sure the text
-- associated with each path is unique.
toFiles :: FilePath -> [DebAtom] -> [(FilePath, String)]
toFiles build d =
    Map.toList $
    Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) $
      [("debian/control", show (pretty (controlFile d))),
       ("debian/changelog", show (pretty (changeLog d))),
       ("debian/rules", unlines (rules build d)),
       ("debian/compat", show (compat d) ++ "\n"),
       ("debian/copyright", copyright d),
       ("debian/source/format", sourceFormat d),
       ("debian/watch", watch d)] ++
      install build d ++
      file d ++
      dir d ++
      init d ++
      logrotate d ++
      link d ++
      postinst d ++
      postrm d ++
      preinst d ++
      prerm d

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

-- | For each pair (A, B) return a rule that ensures that the package
-- requires the same exact version of package B as the version of A
-- currently installed during the build.
tightDependencyFixup :: BinPkgName -> [(String, String)] -> [DebAtom]
tightDependencyFixup _ [] = []
tightDependencyFixup package (hd : tl) =
    let name = show (pretty package)
        -- We need to separate the entries with a comma
        pairs = hd : map (\ (installed, dependent) -> (installed, ", " ++ dependent)) tl in
    [DebRules
      (unlines
       ([ "binary-fixup/" ++ name ++ "::"
        , "\techo -n 'haskell:Depends=' >> debian/" ++ name ++ ".substvars" ] ++
        map (\ (installed, dependent) -> "\tdpkg-query -W -f='" ++ dependent ++ " (=$${Version})' " ++ installed ++ " >> debian/" ++ name ++ ".substvars") pairs ++
        [ "\techo '' >> debian/" ++ name ++ ".substvars"
        , "\techo -n 'haskell:Conflicts=' >> debian/" ++ name ++ ".substvars" ] ++
        map (\ (installed, dependent) -> "\tdpkg-query -W -f='" ++ dependent ++ " (>>$${Version})' " ++ installed ++ " >> debian/" ++ name ++ ".substvars") pairs ++
        [ "\techo '' >> debian/" ++ name ++ ".substvars" ]))]
