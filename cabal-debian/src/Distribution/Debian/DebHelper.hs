-- | Preliminary.
module Distribution.Debian.DebHelper
    ( DebFile(..)
    , DebHelper(..)
    ) where

import Debian.Relation (BinPkgName)

data DebFile
    = DebCompat Int
    | DebCopyright String
    | DebSourceFormat String
    | DebWatch String
    | DH BinPkgName DebHelper
    -- ^ Associate a debhelper value with a binary packge

-- | A small selection of debhelper file types - this should be
-- expanded.
data DebHelper
    = DHInstall [(FilePath, FilePath)] -- ^ Install files from the build into the binary package
    | DHInstallDirs [FilePath]         -- ^ Create directories int he binary package
    | DHInstallInit String             -- ^ Install an /etc/init.d file for the binary package
    | DHInstallLogrotate String        -- ^ Install a logrotate for the binary package
    | DHLink [(FilePath, FilePath)]    -- ^ Create a symbolic link in the binary package
    | DHPostInst String                -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm String                  -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst String                 -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm String                   -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0

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
