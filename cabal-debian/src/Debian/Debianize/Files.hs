-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Files
    ( finalizeDebianization
    , toFileMap
    ) where

import Debug.Trace

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as Map
import Data.Monoid (Monoid, (<>), mempty)
import Data.Set as Set (toList, member, filter)
import Data.String (IsString)
import Data.Text (Text, pack, unpack, unlines)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Atoms (buildDir, dataDir, packageDescription, dependencyHints, setArchitecture, setPackageDescription)
import Debian.Debianize.Combinators (describe, extraDeps, buildDeps)
import Debian.Debianize.Server (execAtoms, serverAtoms, siteAtoms, fileAtoms, backupAtoms)
import Debian.Debianize.Types.Atoms (HasAtoms(putAtoms), DebAtomKey(..), DebAtom(..), lookupAtom, foldAtoms, insertAtom, defaultAtoms)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                                       VersionControlSpec(..), XField(..), XFieldDest(..), newBinaryDebDescription, modifyBinaryDeb)
import Debian.Debianize.Types.Dependencies (DependencyHints (binaryPackageDeps, binaryPackageConflicts))
import Debian.Debianize.Types.PackageType (PackageType(Exec))
import Debian.Debianize.Utility (showDeps')
import Debian.Policy (PackageArchitectures(Any), Section(..))
import Debian.Relation (Relations, BinPkgName)
import qualified Debian.Relation as D
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (init, unlines)
import System.FilePath ((</>), makeRelative, splitFileName)
import Text.PrettyPrint.ANSI.Leijen (pretty)

import Data.Maybe
import Data.Set as Set (Set, difference, union, fromList, null, insert)
import Debian.Debianize.Dependencies (debianName)
import Debian.Debianize.Atoms (noProfilingLibrary, noDocumentationLibrary, utilsPackageName)
import Debian.Debianize.Types.Dependencies (DependencyHints (extraDevDeps))
import Debian.Debianize.Types.PackageHints (InstallFile(..))
import Debian.Debianize.Types.PackageType (PackageType(Development, Profiling, Documentation, Utilities))
import Debian.Policy (PackageArchitectures(All))
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription as Cabal (Executable(exeName))
import Prelude hiding (writeFile, init, unlines)
import System.FilePath (takeDirectory, takeFileName)

sourceFormat :: Debianization -> [(FilePath, Text)]
sourceFormat deb =
    maybe [] (\ x -> [("debian/source/format", pack (show (pretty x)))]) (lookupAtom Source f deb)
    where
      f (DebSourceFormat x) = Just x
      f _ = Nothing

watch :: Debianization -> [(FilePath, Text)]
watch deb =
    maybe [] (\ x -> [("debian/watch", x)]) (lookupAtom Source f deb)
    where
      f (DebWatch x) = Just x
      f _ = Nothing

intermediate :: Debianization -> [(FilePath, Text)]
intermediate deb =
    foldAtoms atomf [] deb
    where
      atomf Source (DHIntermediate path text) files = (path,  text) : files
      atomf _ _ files = files

install :: Debianization -> [(FilePath, Text)]
install deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHInstall src dst) files = Map.insertWith with1 (pathf name)  (pack (src ++ " " ++ dst)) files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".install"

dirs :: Debianization -> [(FilePath, Text)]
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

init :: Debianization -> [(FilePath, Text)]
init deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHInstallInit t) files = Map.insertWith (with2 "init") (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: Debianization -> [(FilePath, Text)]
logrotate deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHLogrotateStanza t) files = Map.insertWith with1 (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
link :: Debianization -> [(FilePath, Text)]
link deb =
    Map.toList $ foldAtoms atomf Map.empty deb
    where
      atomf (Binary name) (DHLink loc txt) files = Map.insertWith with1 (pathf name) (pack (loc ++ " " ++ txt)) files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".links"

postinst :: Debianization -> [(FilePath, Text)]
postinst deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPostInst t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".postinst"

postrm :: Debianization -> [(FilePath, Text)]
postrm deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPostRm t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".postrm"

preinst :: Debianization -> [(FilePath, Text)]
preinst deb =
    Map.toList $ foldAtoms atomf mempty deb
    where
      atomf (Binary name) (DHPreInst t) files = Map.insertWith (<>) (pathf name) t files
      atomf _ _ files = files
      pathf name = "debian" </> show (pretty name) ++ ".preinst"

prerm :: Debianization -> [(FilePath, Text)]
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
toFileMap :: Debianization -> Map.Map FilePath Text
toFileMap d =
    Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b) $
      [("debian/control", pack (show (pretty (control (sourceDebDescription d))))),
       ("debian/changelog", pack (show (pretty (changelog d)))),
       ("debian/rules", rules d),
       ("debian/compat", pack (show (compat d) <> "\n")),
       ("debian/copyright", either (\ x -> pack (show x) <> "\n") id (Debian.copyright d))] ++
      sourceFormat d ++
      watch d ++
      install d ++
      dirs d ++
      init d ++
      logrotate d ++
      link d ++
      postinst d ++
      postrm d ++
      preinst d ++
      prerm d ++
      intermediate d

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
finalizeDebianization  :: Debianization -> Debianization
finalizeDebianization deb0 =
    foldAtomsFinalized g deb'' deb'' -- Apply tweaks to the debianization
    where
      -- Fixme - makeUtilsPackage does stuff that needs to go through foldAtomsFinalized
      deb' = foldAtomsFinalized f (putAtoms mempty deb0) deb0
      deb'' = makeUtilsPackage $ librarySpecs $ buildDeps $ deb'

      -- Create the binary packages
      f :: DebAtomKey -> DebAtom -> Debianization -> Debianization
      f k@(Binary b) a@(DHWebsite _) = insertAtom k a . cabalExecBinaryPackage b
      f k@(Binary b) a@(DHServer _) = insertAtom k a . cabalExecBinaryPackage b
      f k@(Binary b) a@(DHBackups _) =
          insertAtom k a .
          setArchitecture (Binary b) Any .
          cabalExecBinaryPackage b
      f k@(Binary b) a@(DHExecutable _) = insertAtom k a . cabalExecBinaryPackage b
      f k a = insertAtom k a

      -- Apply the hints in the atoms to the debianization
      g :: DebAtomKey -> DebAtom -> Debianization -> Debianization
      g (Binary b) (DHArch x) deb = modifyBinaryDeb b (\ (Just bin) -> bin {architecture = x}) deb
      g (Binary b) (DHPriority x) deb = modifyBinaryDeb b (\ (Just bin) -> bin {binaryPriority = Just x}) deb
      g Source (DHPriority x) deb = deb {sourceDebDescription = (sourceDebDescription deb) {priority = Just x}}
      g (Binary b) (DHSection x) deb = modifyBinaryDeb b (\ (Just bin) -> bin {binarySection = Just x}) deb
      g Source (DHSection x) deb = deb {sourceDebDescription = (sourceDebDescription deb) {section = Just x}}
      g (Binary b) (DHDescription x) deb = modifyBinaryDeb b (\ (Just bin) -> bin {Debian.description = x}) deb
      g _ _ deb = deb

foldAtomsFinalized :: HasAtoms atoms => (DebAtomKey -> DebAtom -> r -> r) -> r -> atoms -> r
foldAtomsFinalized f r0 atoms =
    foldr (\ (k, a) r -> f k a r) r0 (expandAtoms pairs)
    where
      pairs = foldAtoms (\ k a xs -> (k, a) : xs) [] atoms
      builddir = buildDir "dist-ghc/build" atoms
      datadir = dataDir (error "foldAtomsFinalized") atoms

      -- | Fully expand an atom set, returning a set containing
      -- both the original and the expansion.
      expandAtoms :: [(DebAtomKey, DebAtom)] -> [(DebAtomKey, DebAtom)]
      expandAtoms [] = []
      expandAtoms xs = xs ++ expandAtoms (concatMap (uncurry expandAtom) xs)

      expandAtom :: DebAtomKey -> DebAtom -> [(DebAtomKey, DebAtom)]
      expandAtom (Binary b) (DHApacheSite domain' logdir text) =
          [(Binary b, DHLink ("/etc/apache2/sites-available/" ++ domain') ("/etc/apache2/sites-enabled/" ++ domain')),
           (Binary b, DHInstallDir logdir), -- Server won't start if log directory doesn't exist
           (Binary b, DHFile ("/etc/apache2/sites-available" </> domain') text)]
      expandAtom (Binary pkg) (DHInstallCabalExec name dst) =
          [(Binary pkg, DHInstall (builddir </> name </> name) dst)]
      expandAtom (Binary p) (DHInstallCabalExecTo n d) =
          [(Source, DebRulesFragment (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                              , "\tinstall -Dp " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ]))]
      expandAtom (Binary p) (DHInstallData s d) =
          [(Binary p, if takeFileName s == takeFileName d
                      then DHInstall s (datadir </> makeRelative "/" (takeDirectory d))
                      else DHInstallTo s (datadir </> makeRelative "/" d))]
      expandAtom (Binary p) (DHInstallTo s d) =
          [(Source, (DebRulesFragment (unlines [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                               , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])))]
      expandAtom (Binary p) (DHFile path s) =
          let (destDir', destName') = splitFileName path
              tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
              tmpPath = tmpDir </> destName' in
          [(Source, DHIntermediate tmpPath s),
           (Binary p, DHInstall tmpPath destDir')]
      expandAtom k (DHWebsite x) =
          siteAtoms k x
      expandAtom k (DHServer x) =
          serverAtoms k x False
      expandAtom k (DHBackups s) =
          backupAtoms k s
      expandAtom k (DHExecutable x) =
          execAtoms k x
      expandAtom _ _ = []

cabalExecBinaryPackage :: BinPkgName -> Debianization -> Debianization
cabalExecBinaryPackage b deb =
    deb {sourceDebDescription = (sourceDebDescription deb) {binaryPackages = bin : binaryPackages (sourceDebDescription deb)}}
    where
      bin = BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe deb Exec (Cabal.package pkgDesc)
            , relations =
                PackageRelations
                { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            extraDeps (binaryPackageDeps (dependencyHints deb)) b
                , recommends = []
                , suggests = []
                , preDepends = []
                , breaks = []
                , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts (dependencyHints deb)) b
                , provides = []
                , replaces = []
                , builtUsing = []
                }
            }
      pkgDesc = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ packageDescription deb

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

rules :: Debianization -> Text
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
            map vcsField (toList (vcsFields src)) ++
            map xField (toList (xFields src))) :
           map binary (binaryPackages src))
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

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: Debianization -> Debianization
librarySpecs deb | isNothing (packageDescription deb) = deb
librarySpecs deb =
    deb { sourceDebDescription =
            (sourceDebDescription deb)
              { binaryPackages =
                    maybe []
                          (const ([librarySpec deb Any Development (Cabal.package pkgDesc)] ++
                                  if noProfilingLibrary deb then [] else [librarySpec deb Any Profiling (Cabal.package pkgDesc)] ++
                                  if noDocumentationLibrary deb then [] else [docSpecsParagraph deb (Cabal.package pkgDesc)]))
                          (Cabal.library pkgDesc) ++
                    binaryPackages (sourceDebDescription deb) } }
    where
      pkgDesc = fromMaybe (error "librarySpecs: no PackageDescription") $ packageDescription deb

docSpecsParagraph :: HasAtoms atoms => atoms -> PackageIdentifier -> BinaryDebDescription
docSpecsParagraph atoms pkgId =
          BinaryDebDescription
            { Debian.package = debianName atoms Documentation pkgId
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms Documentation pkgId
            , relations =
                PackageRelations
                { depends = [anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                            extraDeps (binaryPackageDeps (dependencyHints atoms)) (debianName atoms Documentation pkgId)
                , recommends = [anyrel "${haskell:Recommends}"]
                , Debian.suggests = [anyrel "${haskell:Suggests}"]
                , preDepends = []
                , breaks = []
                , Debian.conflicts = [anyrel "${haskell:Conflicts}"]
                , provides = []
                , replaces = []
                , builtUsing = []
                }
            }

librarySpec :: HasAtoms atoms => atoms -> PackageArchitectures -> PackageType -> PackageIdentifier -> BinaryDebDescription
librarySpec atoms arch typ pkgId =
          BinaryDebDescription
            { Debian.package = debianName atoms typ pkgId
            , architecture = arch
            , binarySection = Nothing
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms typ pkgId
            , relations =
                PackageRelations
                { depends = (if typ == Development then [anyrel "${shlibs:Depends}"] ++ map anyrel' (extraDevDeps (dependencyHints atoms)) else []) ++
                            ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                             extraDeps (binaryPackageDeps (dependencyHints atoms)) (debianName atoms typ pkgId))
                , recommends = [anyrel "${haskell:Recommends}"]
                , suggests = [anyrel "${haskell:Suggests}"]
                , preDepends = []
                , breaks = []
                , Debian.conflicts = [anyrel "${haskell:Conflicts}"]
                , provides = [anyrel "${haskell:Provides}"]
                , replaces = []
                , builtUsing = []
                }
            }

t1 :: Show a => a -> a
t1 x = trace ("util files: " ++ show x) x
t2 :: Show a => a -> a
t2 x = trace ("available: " ++ show x) x
t3 :: Show a => a -> a
t3 x = trace ("installed: " ++ show x) x
{-
t4 :: Show a => a -> a
t4 x = {- trace ("t4: " ++ show x) -} x
t5 :: Show a => a -> a
t5 x = {- trace ("t5: " ++ show x) -} x
-}

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: Debianization -> Debianization
makeUtilsPackage deb | isNothing (packageDescription deb) = deb
makeUtilsPackage deb =
    case Set.difference (t2 available) (t3 installed) of
      s | Set.null s -> deb
      s -> let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (utilsPackageName deb)
               -- Generate the atoms of the utils package
               atoms = foldr (uncurry insertAtom) (setPackageDescription pkgDesc defaultAtoms) (makeUtilsAtoms p (t1 s))
               -- add them to deb
               deb' = foldAtomsFinalized insertAtom deb atoms in
               -- Passing id to modify will cause the package to be created if necessary
               modifyBinaryDeb p (f p s) deb'
    where
      f _ _ (Just bin) = bin
      f p s Nothing = let bin = newBinaryDebDescription p (arch s) in bin {binarySection = Just (MainSection "misc"), relations = g (relations bin)}
      arch s = if Set.null (Set.filter isCabalExecutable s) then All else Any
      isCabalExecutable (CabalExecutable _) = True
      isCabalExecutable _ = False
      g rels = rels { depends = depends rels ++ [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"]
                    , conflicts = conflicts rels ++ [anyrel "${haskell:Conflicts}"] }
{-
               deb' { sourceDebDescription =
                         (sourceDebDescription deb')
                         { binaryPackages =
                               binaryPackages (sourceDebDescription deb') ++
                                   [BinaryDebDescription
                                    { Debian.package = p
                                    , architecture = Any
                                    , binarySection = Just (MainSection "misc")
                                    , binaryPriority = Nothing
                                    , essential = False
                                    , Debian.description = describe deb' Utilities (Cabal.package pkgDesc)
                                    , relations =
                                        PackageRelations
                                        { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                                                    extraDeps (binaryPackageDeps (dependencyHints deb')) p
                                        , recommends = []
                                        , suggests = []
                                        , preDepends = []
                                        , breaks = []
                                        , conflicts = [anyrel "${haskell:Conflicts}"] ++ extraDeps (binaryPackageConflicts (dependencyHints deb')) p
                                        , provides = []
                                        , replaces = []
                                        , builtUsing = []
                                        }
                                    }]} }
-}
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ packageDescription deb
      available :: Set FileInfo
      available = Set.union (Set.fromList (map DataFile (Cabal.dataFiles pkgDesc)))
                            (Set.fromList (map (CabalExecutable . exeName) (Prelude.filter (Cabal.buildable . Cabal.buildInfo) (Cabal.executables pkgDesc))))
      installed :: Set FileInfo
      installed = foldAtoms cabalFile mempty deb
      cabalFile :: DebAtomKey -> DebAtom -> Set FileInfo -> Set FileInfo
      cabalFile _ (DHInstallCabalExec name _) xs = Set.insert (CabalExecutable name) xs
      cabalFile _ (DHInstallCabalExecTo name _) xs = Set.insert (CabalExecutable name) xs
      cabalFile k (DHExecutable i@(InstallFile {})) xs = foldr (uncurry cabalFile) xs (fileAtoms k i)
      cabalFile _ (DHInstall path _) xs = Set.insert (DataFile path) xs
      cabalFile _ (DHInstallTo path _) xs = Set.insert (DataFile path) xs
      cabalFile _ (DHInstallData path _) xs = Set.insert (DataFile path) xs
      cabalFile _ _ xs = xs

makeUtilsAtoms :: BinPkgName -> Set FileInfo -> [(DebAtomKey, DebAtom)]
makeUtilsAtoms p s =
    if Set.null s
    then []
    else (Source, DebRulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) :
         map (\ x -> (Binary p, x)) (map fileInfoAtom (Set.toList s))
    where
      fileInfoAtom (DataFile path) = DHInstallData path path
      fileInfoAtom (CabalExecutable name) = DHInstallCabalExec name "usr/bin"

{-
makeUtilsAtoms :: BinPkgName -> Set FileInfo -> Debianization -> Debianization
makeUtilsAtoms p s deb =
    case (bundledExecutables deb pkgDesc, Cabal.dataFiles pkgDesc) of
      ([], []) -> deb
      _ -> insertAtom Source (DebRulesFragment (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) $
           insertAtoms' (Binary p) (map fileInfoAtom (Set.toList s)) $
           deb
    where
      fileInfoAtom (DataFile path) = DHInstallData path (takeDirectory path)
      fileInfoAtom (CabalExecutable name) = DHInstallCabalExec name "usr/bin"

-- | The list of executables without a corresponding debian package to put them into
bundledExecutables :: HasAtoms atoms => atoms -> PackageDescription -> [Executable]
bundledExecutables atoms pkgDesc =
    filter nopackage (filter (buildable . buildInfo) (Cabal.executables pkgDesc))
    where
      nopackage p = not (elem (exeName p) (foldAtoms execNameOfHint [] atoms))
      execNameOfHint :: DebAtomKey -> DebAtom -> [String] -> [String]
      execNameOfHint (Binary _) (DHExecutable e) xs = execName e : xs
      execNameOfHint (Binary _) (DHServer s) xs = execName (installFile s) : xs
      execNameOfHint (Binary _) (DHWebsite s) xs = execName (installFile (server s)) : xs
      execNameOfHint (Binary _) (DHBackups s) xs = s : xs
      execNameOfHint _ _ xs = xs
-}

data FileInfo
    = DataFile FilePath
    -- ^ A file that is going to be installed into the package's data
    -- file directory, /usr/share/packagename-version/.
    | CabalExecutable String
    -- ^ A Cabal Executable record, which appears in dist/build/name/name,
    -- and is typically installed into /usr/bin.
    deriving (Eq, Ord, Show)
