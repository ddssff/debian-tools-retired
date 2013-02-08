-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( finalizeDebianization
    ) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Lens.Lazy (setL, getL, modL)
import Data.List as List (map)
import Data.Map as Map (insertWith, foldWithKey, elems)
import Data.Maybe
import Data.Monoid (mempty, (<>))
import Data.Set as Set (Set, difference, fromList, null, insert, toList, filter, fold, map, union, singleton)
import Data.Text as Text (pack, unlines, unpack)
import Debian.Debianize.Atoms as Atoms
    (HasAtoms(packageDescription, control, binaryArchitectures, rulesFragments, website, serverInfo, link,
              backups, executable, sourcePriority, sourceSection, binaryPriorities, binarySections, description,
              install, installTo, installData, installCabalExecTo),
     Atoms, noProfilingLibrary, noDocumentationLibrary, utilsPackageName, extraDevDeps,
     installData, installCabalExec)
import Debian.Debianize.Atoms as Atoms (HasAtoms(file, apacheSite, installDir, buildDir,
                                                 dataDir, intermediateFiles,
                                                 logrotateStanza, postInst, installInit))
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..),
                                               newBinaryDebDescription, modifyBinaryDeb,
                                               PackageType(Exec, Development, Profiling, Documentation, Utilities))
import Debian.Debianize.Dependencies (debianName, binaryPackageDeps, binaryPackageConflicts, putBuildDeps)
import Debian.Debianize.Goodies (describe)
import Debian.Debianize.Types (InstallFile(..), Server(..), Site(..))
import Debian.Policy (PackageArchitectures(Any, All), Section(..), apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (Relation(Rel), BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName), PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (init, unlines, writeFile, map)
import System.FilePath ((</>), (<.>), makeRelative, splitFileName, takeDirectory, takeFileName)
import System.Process (showCommandForUser)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
finalizeDebianization  :: Atoms -> Atoms
finalizeDebianization deb0 =
    deb'''''
    where
      -- Fixme - makeUtilsPackage does stuff that needs to go through foldAtomsFinalized
      deb' = finalizeAtoms deb0
      deb'' = f deb'
      deb''' = makeUtilsPackage $ librarySpecs $ putBuildDeps $ deb''
      deb'''' = finalizeAtoms deb'''
      deb''''' = g deb'''' -- Apply tweaks to the debianization

      -- Create the binary packages
      f :: Atoms -> Atoms
      f atoms = (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL website atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL serverInfo atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> modL binaryArchitectures (Map.insertWith (flip const) b Any) . cabalExecBinaryPackage b $ atoms'') atoms' (getL backups atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b _ atoms'' -> cabalExecBinaryPackage b atoms'') atoms' (getL executable atoms)) $ atoms
      -- Apply the hints in the atoms to the debianization
      g :: Atoms -> Atoms
      g atoms = (\ atoms' -> maybe atoms' (\ x -> modL control (\ y -> y {priority = Just x}) atoms') (getL sourcePriority atoms)) .
                (\ atoms' -> maybe atoms' (\ x -> modL control (\ y -> y {section = Just x}) atoms') (getL sourceSection atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {architecture = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binaryArchitectures atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binaryPriority = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binaryPriorities atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {binarySection = Just x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL binarySections atoms)) .
                (\ atoms' -> Map.foldWithKey (\ b x atoms'' -> modL control (\ y -> modifyBinaryDeb b ((\ bin -> bin {Debian.description = x}) . fromMaybe (newBinaryDebDescription b Any)) y) atoms'') atoms' (getL Atoms.description atoms)) $ atoms

cabalExecBinaryPackage :: BinPkgName -> Atoms -> Atoms
cabalExecBinaryPackage b deb =
    modL control (\ y -> y {binaryPackages = bin : binaryPackages y}) deb
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
      pkgDesc = fromMaybe (error "cabalExecBinaryPackage: no PackageDescription") $ getL packageDescription deb

binaryPackageRelations :: BinPkgName -> PackageType -> Atoms -> PackageRelations
binaryPackageRelations b typ deb =
    PackageRelations
    { Debian.depends = [anyrel "${shlibs:Depends}", anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
                       (if typ == Development then List.map anyrel' (toList (getL extraDevDeps deb)) else []) ++
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

-- debLibProf haddock binaryPackageDeps extraDevDeps extraLibMap
librarySpecs :: Atoms -> Atoms
librarySpecs deb | isNothing (getL packageDescription deb) = deb
librarySpecs deb =
    (if doc
     then modL link (Map.insertWith Set.union debName (singleton ("/usr/share/doc" </> show (pretty debName) </> "html" </> cabal <.> "txt", "/usr/lib/ghc-doc/hoogle" </> hoogle <.> "txt")))
     else id) $
    modL control
         (\ y -> y { binaryPackages =
                               (if dev then [librarySpec deb Any Development (Cabal.package pkgDesc)] else []) ++
                               (if prof then [librarySpec deb Any Profiling (Cabal.package pkgDesc)] else []) ++
                               (if doc then [docSpecsParagraph deb (Cabal.package pkgDesc)] else []) ++
                               (binaryPackages y) })
         deb
    where
      doc = dev && not (getL noDocumentationLibrary deb)
      prof = dev && not (getL noProfilingLibrary deb)
      dev = isJust (Cabal.library pkgDesc)
      pkgDesc = fromMaybe (error "librarySpecs: no PackageDescription") $ getL packageDescription deb
      PackageName cabal = pkgName (Cabal.package pkgDesc)
      debName :: BinPkgName
      debName = debianName deb Documentation (Cabal.package pkgDesc)
      hoogle = List.map toLower cabal

docSpecsParagraph :: Atoms -> PackageIdentifier -> BinaryDebDescription
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

librarySpec :: Atoms -> PackageArchitectures -> PackageType -> PackageIdentifier -> BinaryDebDescription
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

-- | Create a package to hold any executables and data files not
-- assigned to some other package.
makeUtilsPackage :: Atoms -> Atoms
makeUtilsPackage deb | isNothing (getL packageDescription deb) = deb
makeUtilsPackage deb =
    case (Set.difference availableData installedData, Set.difference availableExec installedExec) of
      (datas, execs) | Set.null datas && Set.null execs -> deb
      (datas, execs) ->
          let p = fromMaybe (debianName deb Utilities (Cabal.package pkgDesc)) (getL utilsPackageName deb)
              deb' = setL packageDescription (Just pkgDesc) . makeUtilsAtoms p datas execs $ deb in
          modL control (\ y -> modifyBinaryDeb p (f deb' p (if Set.null execs then All else Any)) y) deb'
    where
      f _ _ _ (Just bin) = bin
      f deb' p arch Nothing =
          let bin = newBinaryDebDescription p arch in
          bin {binarySection = Just (MainSection "misc"),
               relations = binaryPackageRelations p Utilities deb'}
      pkgDesc = fromMaybe (error "makeUtilsPackage: no PackageDescription") $ getL packageDescription deb
      availableData = Set.fromList (Cabal.dataFiles pkgDesc)
      availableExec = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc)))
      installedData :: Set FilePath
      installedData = Set.fromList ((List.map fst . concat . List.map toList . elems $ getL install deb) <>
                                    (List.map fst . concat . List.map toList . elems $ getL installTo deb) <>
                                    (List.map fst . concat . List.map toList . elems $ getL installData deb))
      installedExec :: Set String
      installedExec = Set.fromList ((List.map fst . concat . List.map toList . elems $ getL installCabalExec deb) <>
                                    (List.map fst . concat . List.map toList .  elems $ getL installCabalExecTo deb) <>
                                    (List.map ename . elems $ getL executable deb))
          where ename i =
                    case sourceDir i of
                      (Nothing) -> execName i
                      (Just s) ->  s </> execName i
      -- installedExec = foldCabalExecs (Set.insert :: String -> Set String -> Set String) (Set.empty :: Set String) deb

makeUtilsAtoms :: BinPkgName -> Set FilePath -> Set String -> Atoms -> Atoms
makeUtilsAtoms p datas execs atoms0 =
    if Set.null datas && Set.null execs
    then atoms0
    else modL rulesFragments (Set.insert (pack ("build" </> show (pretty p) ++ ":: build-ghc-stamp\n"))) . g $ atoms0
    where
      g :: Atoms -> Atoms
      g atoms = Set.fold execAtom (Set.fold dataAtom atoms datas) execs
      dataAtom path atoms = modL installData (insertWith union p (singleton (path, path))) atoms
      execAtom name atoms = modL installCabalExec (insertWith union p (singleton (name, "usr/bin"))) atoms

anyrel :: String -> [Relation]
anyrel x = anyrel' (BinPkgName x)

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]

finalizeAtoms :: Atoms -> Atoms
finalizeAtoms atoms | atoms == mempty = atoms
finalizeAtoms atoms = atoms <> finalizeAtoms (expandAtoms atoms)

expandAtoms :: Atoms -> Atoms
expandAtoms old =
    expandApacheSite .
    expandInstallCabalExec .
    expandInstallCabalExecTo .
    expandInstallData .
    expandInstallTo .
    expandFile .
    expandWebsite .
    expandServer .
    expandBackups .
    expandExecutable $
    mempty
    where
      expandApacheSite :: Atoms -> Atoms
      expandApacheSite new =
          foldWithKey (\ b (dom, log, text) atoms ->
                           modL link (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available/" ++ dom, "/etc/apache2/sites-enabled/" ++ dom))) .
                           modL installDir (Map.insertWith Set.union b (singleton log)) .
                           modL file (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available" </> dom, text))) $
                           atoms)
                      new
                      (getL apacheSite old)

      expandInstallCabalExec :: Atoms -> Atoms
      expandInstallCabalExec new =
          foldWithKey (\ b pairs atoms -> Set.fold (\ (name, dst) atoms' -> modL install (Map.insertWith Set.union b (singleton (builddir </> name </> name, dst))) atoms')
                                                    atoms
                                                    pairs)
                      new
                      (getL installCabalExec old)
          where
            builddir = fromMaybe {-(error "finalizeAtoms: no buildDir")-} "dist-ghc/build" (getL buildDir old)

      expandInstallCabalExecTo :: Atoms -> Atoms
      expandInstallCabalExecTo new =
          foldWithKey (\ b pairs atoms ->
                           Set.fold (\ (n, d) atoms' ->
                                         modL rulesFragments (Set.insert (Text.unlines
                                                                          [ pack ("binary-fixup" </> show (pretty b)) <> "::"
                                                                          , "\tinstall -Dp " <> pack (builddir </> n </> n) <> " " <> pack ("debian" </> show (pretty b) </> makeRelative "/" d) ])) atoms')
                                    atoms
                                    pairs)
                      new
                      (getL installCabalExecTo old)
          where
            builddir = fromMaybe {-(error "finalizeAtoms: no buildDir")-} "dist-ghc/build" (getL buildDir old)

      expandInstallData :: Atoms -> Atoms
      expandInstallData new =
          foldWithKey (\ b pairs atoms ->
                           Set.fold (\ (s, d) atoms' ->
                                         if takeFileName s == takeFileName d
                                         then modL install (Map.insertWith Set.union b (singleton (s, datadir </> makeRelative "/" (takeDirectory d)))) atoms'
                                         else modL installTo (Map.insertWith Set.union b (singleton (s, datadir </> makeRelative "/" d))) atoms')
                                    atoms
                                    pairs)
                      new
                      (getL installData old)
          where
            datadir = fromMaybe (error "finalizeAtoms: no dataDir") $ getL dataDir old

      expandInstallTo :: Atoms -> Atoms
      expandInstallTo new =
          foldWithKey (\ p pairs atoms ->
                           Set.fold (\ (s, d) atoms' ->
                                         modL rulesFragments (Set.insert (Text.unlines
                                                                          [ pack ("binary-fixup" </> show (pretty p)) <> "::"
                                                                          , "\tinstall -Dp " <> pack s <> " " <> pack ("debian" </> show (pretty p) </> makeRelative "/" d) ])) atoms') atoms pairs)
                      new
                      (getL installTo old)

      expandFile :: Atoms -> Atoms
      expandFile new =
          foldWithKey (\ p pairs atoms ->
                           Set.fold (\ (path, s) atoms' ->
                                         let (destDir', destName') = splitFileName path
                                             tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack s)))
                                             tmpPath = tmpDir </> destName' in
                                         modL intermediateFiles (Set.insert (tmpPath, s)) .
                                         modL install (Map.insertWith Set.union p (singleton (tmpPath, destDir'))) $
                                         atoms')
                                    atoms
                                    pairs)
                      new
                      (getL file old)

      expandWebsite :: Atoms -> Atoms
      expandWebsite new =
          foldWithKey (\ b x atoms -> siteAtoms b x atoms)
                      new
                      (getL website old)

      expandServer :: Atoms -> Atoms
      expandServer new =
          foldWithKey (\ b x atoms -> serverAtoms b x False atoms)
                      new
                      (getL serverInfo old)

      expandBackups :: Atoms -> Atoms
      expandBackups new =
          foldWithKey (\ b s atoms -> backupAtoms b s atoms)
                      new
                      (getL backups old)

      expandExecutable :: Atoms -> Atoms
      expandExecutable new =
          foldWithKey (\ b x atoms -> execAtoms b x atoms)
                      new
                      (getL executable old)

siteAtoms :: BinPkgName -> Site -> Atoms -> Atoms
siteAtoms b site =
    modL installDir (Map.insertWith Set.union b (singleton "/etc/apache2/sites-available")) .
    modL link (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available/" ++ domain site, "/etc/apache2/sites-enabled/" ++ domain site))) .
    modL file (Map.insertWith Set.union b (singleton ("/etc/apache2/sites-available" </> domain site, apacheConfig))) .
    modL installDir (Map.insertWith Set.union b (singleton (apacheLogDirectory b))) .
    modL logrotateStanza (Map.insertWith Set.union b (singleton (Text.unlines $
                                                                 [ pack (apacheAccessLog b) <> " {"
                                                                 , "  weekly"
                                                                 , "  rotate 5"
                                                                 , "  compress"
                                                                 , "  missingok"
                                                                 , "}"]))) .
    modL logrotateStanza (Map.insertWith Set.union b (singleton (Text.unlines $
                                                                 [ pack (apacheErrorLog b) <> " {"
                                                                 , "  weekly"
                                                                 , "  rotate 5"
                                                                 , "  compress"
                                                                 , "  missingok"
                                                                 , "}" ]))) .
    serverAtoms b (server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          Text.unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " <> pack (serverAdmin site)
                   , "    ServerName www." <> pack (domain site)
                   , "    ServerAlias " <> pack (domain site)
                   , ""
                   , "    ErrorLog " <> pack (apacheErrorLog b)
                   , "    CustomLog " <> pack (apacheAccessLog b) <> " combined"
                   , ""
                   , "    ProxyRequests Off"
                   , "    AllowEncodedSlashes NoDecode"
                   , ""
                   , "    <Proxy *>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                Deny from all"
                   , "                #Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    <Proxy http://127.0.0.1:" <> port' <> "/*>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                #Deny from all"
                   , "                Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    SetEnv proxy-sendcl 1"
                   , ""
                   , "    ProxyPass / http://127.0.0.1:" <> port' <> "/ nocanon"
                   , "    ProxyPassReverse / http://127.0.0.1:" <> port' <> "/"
                   , "</VirtualHost>" ]
      port' = pack (show (port (server site)))

serverAtoms :: BinPkgName -> Server -> Bool -> Atoms -> Atoms
serverAtoms b server isSite =
    modL postInst (insertWith (error "serverAtoms") b debianPostinst) .
    modL installInit (Map.insertWith (error "serverAtoms") b debianInit) .
    serverLogrotate' b .
    execAtoms b exec
    where
      exec = installFile server
      debianInit =
          Text.unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" <> pack (destName exec) <> " || exit 0"
                   , "    log_begin_msg \"Starting " <> pack (destName exec) <> "...\""
                   , "    mkdir -p " <> pack (databaseDirectory b)
                   , "    " <> startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " <> pack (destName exec) <> "...\""
                   , "    " <> stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = pack $ showCommandForUser "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ serverOptions)
      stopCommand = pack $ showCommandForUser "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if retry server /= "" then ["--retry=" ++ retry server ] else []
      serverOptions = serverFlags server ++ commonServerOptions
      -- Without these, happstack servers chew up CPU even when idle
      commonServerOptions = ["+RTS", "-IO", "-RTS"]

      debianPostinst =
          Text.unlines $
                   ([ "#!/bin/sh"
                    , ""
                    , "case \"$1\" in"
                    , "  configure)" ] ++
                    (if isSite
                     then [ "    # Apache won't start if this directory doesn't exist"
                          , "    mkdir -p " <> pack (apacheLogDirectory b)
                          , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
                          , "    /usr/sbin/a2enmod proxy"
                          , "    /usr/sbin/a2enmod proxy_http"
                          , "    service apache2 restart" ]
                     else []) ++
                    [ "    service " <> pack (show (pretty b)) <> " start"
                    , "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate' :: BinPkgName -> Atoms -> Atoms
serverLogrotate' b =
    modL logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]))) .
    modL logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])))

backupAtoms :: BinPkgName -> String -> Atoms -> Atoms
backupAtoms b name =
    modL postInst (insertWith (error "backupAtoms") b
                 (Text.unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])) .
    execAtoms b (InstallFile { execName = name
                              , destName = name
                              , sourceDir = Nothing
                              , destDir = Just "/etc/cron.hourly" })

execAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
execAtoms b ifile r =
    modL rulesFragments (Set.insert (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) .
    fileAtoms b ifile $
    r

fileAtoms :: BinPkgName -> InstallFile -> Atoms -> Atoms
fileAtoms b installFile r =
    fileAtoms' b (sourceDir installFile) (execName installFile) (destDir installFile) (destName installFile) r

fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> Atoms -> Atoms
fileAtoms' b sourceDir execName destDir destName r =
    case (sourceDir, execName == destName) of
      (Nothing, True) -> modL installCabalExec (insertWith Set.union b (singleton (execName, d))) r
      (Just s, True) -> modL install (insertWith Set.union b (singleton (s </> execName, d))) r
      (Nothing, False) -> modL installCabalExecTo (insertWith Set.union b (singleton (execName, (d </> destName)))) r
      (Just s, False) -> modL installTo (insertWith Set.union b (singleton (s </> execName, d </> destName))) r
    where
      d = fromMaybe "usr/bin" destDir
