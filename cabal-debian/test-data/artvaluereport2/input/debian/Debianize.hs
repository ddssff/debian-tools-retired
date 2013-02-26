{-# LANGUAGE OverloadedStrings #-}
import Debian.Debianize
import Data.Lens.Lazy
import Debian.Debianize.Atoms as Atoms (depends, description)
import Debian.Debianize.Types (Top(Top))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(..), Relation(Rel), VersionReq(SLT))
import Debian.Version (parseDebianVersion)
import Data.Map as Map (insertWith, insert)
import Data.Maybe (fromMaybe)
import Data.Set as Set (insert, union, singleton)
import Data.Text as Text (intercalate)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

-- This looks just like a "real" Debianize.hs file except that it
-- returns the comparison string instead of doing a
-- writeDebianization, and it reads and writes the test-data
-- directories instead of ".".  Also, you wouldn't want to
-- copyFirstLogEntry.
main :: IO ()
main =
    do log <- inputChangeLog (Top "test-data/artvaluereport2/input")
       new <- debianization (Top "test-data/artvaluereport2/input")
              (return .
               modL control (\ y -> y {homepage = Just "http://appraisalreportonline.com"}) .
               setL compat (Just 7) .
               modL control (\ x -> x {standardsVersion = Just (StandardsVersion 3 9 1 Nothing)}) .
               setL sourcePackageName (Just (SrcPkgName "haskell-artvaluereport2")) .
               -- setL utilsPackageName (Just (BinPkgName "artvaluereport2-server")) .
               modL binaryArchitectures (Map.insert (BinPkgName "artvaluereport2-development") All) .
               modL binaryArchitectures (Map.insert (BinPkgName "artvaluereport2-production") All) .
               modL binaryArchitectures (Map.insert (BinPkgName "artvaluereport2-staging") All) .
               modL buildDepsIndep (Set.insert (Rel (BinPkgName "libjs-jcrop") Nothing Nothing)) .
               modL buildDepsIndep (Set.insert (Rel (BinPkgName "libjs-jquery") Nothing Nothing)) .
               modL buildDepsIndep (Set.insert (Rel (BinPkgName "libjs-jquery-ui") (Just (SLT (parseDebianVersion ("1.10" :: String)))) Nothing)) .
               modL description (Map.insert (BinPkgName "appraisalscope") "Offline manipulation of appraisal database") .
               addServerDeps .
               addServerData .
               addDep (BinPkgName "artvaluereport2-production") (BinPkgName "apache2") .
               -- This should go into the "real" data directory.  And maybe a different icon for each server?
               -- modL install (Map.insertWith union (BinPkgName "artvaluereport2-server") (singleton ("theme/ArtValueReport_SunsetSpectrum.ico", "usr/share/artvaluereport2-data"))) .
               modL Atoms.description (Map.insertWith (error "test6") (BinPkgName "artvaluereport2-backups")
                                       (Text.intercalate "\n"
                                        [ "backup program for the appraisalreportonline.com site"
                                        , "  Install this somewhere other than where the server is running get"
                                        , "  automated backups of the database." ])) .
               doBackups (BinPkgName "artvaluereport2-backups") "artvaluereport2-backups" .
               doWebsite (BinPkgName "artvaluereport2-production") (theSite (BinPkgName "artvaluereport2-production")) .
               doServer (BinPkgName "artvaluereport2-staging") (theServer (BinPkgName "artvaluereport2-staging")) .
               doServer (BinPkgName "artvaluereport2-development") (theServer (BinPkgName "artvaluereport2-development")) .
               doExecutable (BinPkgName "appraisalscope") (InstallFile {execName = "appraisalscope", sourceDir = Nothing, destDir = Nothing, destName = "appraisalscope"}) .
               modL installCabalExec (Map.insertWith Set.union (BinPkgName "appraisalscope") (singleton ("lookatareport", "usr/bin"))) .
               setL changelog (Just log))
       old <- inputDebianization (Top "test-data/artvaluereport2/output")
       -- The newest log entry gets modified when the Debianization is
       -- generated, it won't match so drop it for the comparison.
       putStr $ compareDebianization old (copyFirstLogEntry old new)
    where
      addServerDeps :: Atoms -> Atoms
      addServerDeps atoms = foldr addDeps atoms (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addDeps p atoms = foldr (addDep p) atoms (map BinPkgName ["libjpeg-progs", "libjs-jcrop", "libjs-jquery", "libjs-jquery-ui", "netpbm", "texlive-fonts-extra", "texlive-fonts-recommended", "texlive-latex-extra", "texlive-latex-recommended"])
      addDep p dep atoms = modL Atoms.depends (Map.insertWith union p (singleton (Rel dep Nothing Nothing))) atoms

      addServerData :: Atoms -> Atoms
      addServerData atoms = foldr addData atoms (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addData p atoms =
          modL installData (Map.insertWith union p (singleton ("theme/ArtValueReport_SunsetSpectrum.ico", "ArtValueReport_SunsetSpectrum.ico"))) $
          foldr (addDataFile p) atoms ["Udon.js", "flexbox.css", "DataTables-1.8.2", "html5sortable", "jGFeed", "searchMag.png",
                                       "Clouds.jpg", "tweaks.css", "verticalTabs.css", "blueprint", "jquery.blockUI", "jquery.tinyscrollbar"]
      addDataFile p path atoms = modL installData (Map.insertWith union p (singleton (path, path))) atoms

      theSite :: BinPkgName -> Site
      theSite deb =
          Site { domain = hostname'
               , serverAdmin = "logic@seereason.com"
               , server = theServer deb }
      theServer :: BinPkgName -> Server
      theServer deb =
          Server { hostname =
                       case deb of
                         BinPkgName "artvaluereport2-production" -> hostname'
                         _ -> hostname'
                 , port = portNum deb
                 , headerMessage = "Generated by artvaluereport2/Setup.hs"
                 , retry = "60"
                 , serverFlags =
                    ([ "--http-port", show (portNum deb)
                     , "--base-uri", case deb of
                                       BinPkgName "artvaluereport2-production" -> "http://" ++ hostname' ++ "/"
                                       _ -> "http://seereason.com:" ++ show (portNum deb) ++ "/"
                     , "--top", databaseDirectory deb
                     , "--logs", "/var/log/" ++ show (pretty deb)
                     , "--log-mode", case deb of
                                       BinPkgName "artvaluereport2-production" -> "Production"
                                       _ -> "Development"
                     , "--static", "/usr/share/artvaluereport2-data"
                     , "--no-validate" ] ++
                     (case deb of
                        BinPkgName "artvaluereport2-production" -> [{-"--enable-analytics"-}]
                        _ -> []) {- ++
                     [ "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path ] -})
                 , installFile =
                     InstallFile { execName   = "artvaluereport2-server"
                                 , destName   = show (pretty deb)
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }
      hostname' = "my.appraisalreportonline.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "artvaluereport2-production"  -> 9027
            "artvaluereport2-staging"     -> 9031
            "artvaluereport2-development" -> 9032
            _ -> error $ "Unexpected package name: " ++ deb

anyrel :: BinPkgName -> Relation
anyrel b = Rel b Nothing Nothing

copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)
