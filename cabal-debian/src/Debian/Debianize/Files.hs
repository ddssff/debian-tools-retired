-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Files
    ( finalizeDebianization
    , toFileMap
    ) where

-- import Debug.Trace

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Monoid, (<>), mempty)
import Data.Set as Set (Set, difference, union, fromList, null, insert, toList, member, filter)
import Data.String (IsString)
import Data.Text (Text, pack, unpack, unlines)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.AtomsType (HasAtoms(putAtoms), DebAtomKey(..), DebAtom(..), lookupAtom, foldAtoms, insertAtom, defaultAtoms,
                                   buildDir, dataDir, packageDescription, setArchitecture, setPackageDescription, binaryPackageDeps, changeLog,
                                   binaryPackageConflicts, noProfilingLibrary, noDocumentationLibrary, utilsPackageName, extraDevDeps, rulesHead, compat, copyright)
import Debian.Debianize.Combinators (describe, buildDeps)
import Debian.Debianize.Dependencies (debianName)
import Debian.Debianize.Server (execAtoms, serverAtoms, siteAtoms, fileAtoms, backupAtoms)
import Debian.Debianize.Types.DebControl as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                                    VersionControlSpec(..), XField(..), XFieldDest(..), newBinaryDebDescription, modifyBinaryDeb)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), Deb(..))
import Debian.Debianize.Types.PackageHints (InstallFile(..))
import Debian.Debianize.Types.PackageType (PackageType(Exec, Development, Profiling, Documentation, Utilities))
import Debian.Debianize.Utility (showDeps')
import Debian.Policy (PackageArchitectures(Any, All), Section(..))
import Debian.Relation (Relations, BinPkgName)
import qualified Debian.Relation as D
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (init, unlines, writeFile)
import System.FilePath ((</>), (<.>), makeRelative, splitFileName, takeDirectory, takeFileName)
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

intermediate :: HasAtoms atoms => atoms -> [(FilePath, Text)]
intermediate deb =
    foldAtoms atomf [] deb
    where
      atomf Source (DHIntermediate path text) files = (path,  text) : files
      atomf _ _ files = files

install :: HasAtoms atoms => atoms -> [(FilePath, Text)]
install deb =
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
link :: HasAtoms atoms => atoms -> [(FilePath, Text)]
link deb =
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
      install atoms ++
      dirs atoms ++
      init atoms ++
      logrotate atoms ++
      link atoms ++
      postinst atoms ++
      postrm atoms ++
      preinst atoms ++
      prerm atoms ++
      intermediate atoms

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
          setArchitecture b Any .
          cabalExecBinaryPackage b
      f k@(Binary b) a@(DHExecutable _) = insertAtom k a . cabalExecBinaryPackage b
      f k a = insertAtom k a

      -- Apply the hints in the atoms to the debianization
      g :: DebAtomKey -> DebAtom -> Debianization -> Debianization
      g (Binary b) (DHArch x) deb =        setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {architecture = x}) (sourceDebDescription deb)) deb
      g (Binary b) (DHPriority x) deb =    setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {binaryPriority = Just x}) (sourceDebDescription deb)) deb
      g Source (DHPriority x) deb =        setSourceDebDescription ((sourceDebDescription deb) {priority = Just x}) deb
      g (Binary b) (DHSection x) deb =     setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {binarySection = Just x}) (sourceDebDescription deb)) deb
      g Source (DHSection x) deb =         setSourceDebDescription ((sourceDebDescription deb) {section = Just x}) deb
      g (Binary b) (DHDescription x) deb = setSourceDebDescription (modifyBinaryDeb b (\ (Just bin) -> bin {Debian.description = x}) (sourceDebDescription deb)) deb
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
    setSourceDebDescription ((sourceDebDescription deb) {binaryPackages = bin : binaryPackages (sourceDebDescription deb)}) deb
    where
      bin = BinaryDebDescription
            { Debian.package = b
            , architecture = Any
            , binarySection = Just (MainSection "misc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe deb Exec (Cabal.package pkgDesc)
            , relations = binaryPackageRelations b Exec deb
            }
      pkgDesc = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ packageDescription deb

binaryPackageRelations :: HasAtoms atoms => BinPkgName -> PackageType -> atoms -> PackageRelations
binaryPackageRelations b typ deb =
    PackageRelations
    { depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                (if typ == Development then map anyrel' (toList (extraDevDeps deb)) else []) ++
                binaryPackageDeps b deb
    , recommends = [anyrel "${haskell:Recommends}"]
    , suggests = [anyrel "${haskell:Suggests}"]
    , preDepends = []
    , breaks = []
    , conflicts = [anyrel "${haskell:Conflicts}"] ++ binaryPackageConflicts b deb
    , provides = [anyrel "${haskell:Provides}"]
    , replaces = []
    , builtUsing = []
    }

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

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
    (if doc then insertAtom (Binary (debName)) (DHLink ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt") ("/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")) else id) $
    setSourceDebDescription
            ((sourceDebDescription deb)
              { binaryPackages =
                    (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                    (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                    (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                    (binaryPackages (sourceDebDescription deb)) }) deb
    where
      doc = dev && not (noDocumentationLibrary deb)
      prof = dev && not (noProfilingLibrary deb)
      dev = isJust (Cabal.library pkgDesc)
      pkgDesc = fromMaybe (error "librarySpecs: no PackageDescription") $ packageDescription deb
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      debName :: BinPkgName
      debName = debianName deb Documentation (Cabal.package pkgDesc)
      hoogle = map toLower cabal

docSpecsParagraph :: HasAtoms atoms => atoms -> PackageIdentifier -> BinaryDebDescription
docSpecsParagraph atoms pkgId =
          BinaryDebDescription
            { Debian.package = debianName atoms Documentation pkgId
            , architecture = All
            , binarySection = Just (MainSection "doc")
            , binaryPriority = Nothing
            , essential = False
            , Debian.description = describe atoms Documentation pkgId
            , relations = binaryPackageRelations (debianName atoms Documentation pkgId) Development atoms
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
            , relations = binaryPackageRelations (debianName atoms typ pkgId) Development atoms
            }

t1 :: Show a => a -> a
t1 x = {-trace ("util files: " ++ show x)-} x
t2 :: Show a => a -> a
t2 x = {-trace ("available: " ++ show x)-} x
t3 :: Show a => a -> a
t3 x = {-trace ("installed: " ++ show x)-} x
-- t4 :: Show a => a -> a
-- t4 x = trace ("utils package atoms: " ++ show x) x
-- t5 :: Show a => a -> a
-- t5 x = {- trace ("t5: " ++ show x) -} x

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: Debianization -> Debianization
makeUtilsPackage deb | isNothing (packageDescription deb) = deb
makeUtilsPackage deb =
    case Set.difference (t2 available) (t3 installed) of
      s | Set.null s -> deb
      s -> let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (utilsPackageName deb)
               atoms = foldr (uncurry insertAtom) (setPackageDescription pkgDesc defaultAtoms) (makeUtilsAtoms p (t1 s))
               deb' = foldAtomsFinalized insertAtom deb atoms in
           setSourceDebDescription (modifyBinaryDeb p (f deb' p s) (sourceDebDescription deb')) deb'
    where
      f _ _ _ (Just bin) = bin
      f deb' p s Nothing =
          let bin = newBinaryDebDescription p (arch s) in
          bin {binarySection = Just (MainSection "misc"),
               relations = binaryPackageRelations p Utilities deb'}
      arch s = if Set.null (Set.filter isCabalExecutable s) then All else Any
      isCabalExecutable (CabalExecutable _) = True
      isCabalExecutable _ = False
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ packageDescription deb
      available :: Set FileInfo
      available = Set.union (Set.fromList (map DataFile (Cabal.dataFiles pkgDesc)))
                            (Set.fromList (map (CabalExecutable . Cabal.exeName) (Prelude.filter (Cabal.buildable . Cabal.buildInfo) (Cabal.executables pkgDesc))))
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

-- | This is onlyu used to represent files during the computation of
-- @installed@ and @available@ in makeUtilsPackage.
data FileInfo
    = DataFile FilePath
    -- ^ A file that is going to be installed into the package's data
    -- file directory, /usr/share/packagename-version/.
    | CabalExecutable String
    -- ^ A Cabal Executable record, which appears in dist/build/name/name,
    -- and is typically installed into /usr/bin.
    deriving (Eq, Ord, Show)
