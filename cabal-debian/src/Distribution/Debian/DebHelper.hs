-- | Preliminary.
module Distribution.Debian.DebHelper
    ( DebFile(..)
    , DebHelper(..)
    , Debianization(..)
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
import Distribution.Debian.Dependencies (PackageType(..), debianSourcePackageName, debianDocPackageName)
import Distribution.Debian.Relations (buildDependencies, docDependencies, allBuildDepends, versionSplits)
import Distribution.Debian.Utility
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (PackageDescription(package, maintainer, homepage, description, author, pkgUrl, synopsis))
import Network.URI (URI, parseURI)
import Prelude hiding (catch)

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
    | DHInstallDirs [FilePath] -- ^ Create directories int he binary package
    | DHInstallInit String -- ^ Install an /etc/init.d file for the binary package
    | DHInstallLogrotate String -- ^ Install a logrotate for the binary package
    | DHLink [(FilePath, FilePath)] -- ^ Create a symbolic link in the binary package

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
fromControl (Control {unControl = source : binaries}) =
    DebControl
    { sourceSpec = fromSourceSection source
    , binarySpecs = map fromBinarySection binaries }

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
