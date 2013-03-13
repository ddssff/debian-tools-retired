import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Set as Set (union, singleton)
import Debian.Changes (ChangeLog)
import Debian.Debianize (Atoms, Top(..), sourcePackageName, inputChangeLog, debianize, changelog, copyright, doExecutable, sourceFormat, depends, SourceFormat(Native3), InstallFile(..))
import Debian.Debianize.Details (seereasonDefaultAtoms)
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName), VersionReq(EEQ), Relation(Rel))
import Debian.Version (buildDebianVersion)
import Distribution.License (License(AllRightsReserved))
import Prelude hiding (log)

main =
    do log <- inputChangeLog top
       debianize top (customize log) seereasonDefaultAtoms
    where
      top = Top "."

customize :: ChangeLog -> Atoms -> IO Atoms
customize log =
    return .
    setL sourcePackageName (Just (SrcPkgName "autobuilder")) .
    setL copyright (Just (Left AllRightsReserved)) .
    setL sourceFormat (Just Native3) .
    setL changelog (Just log) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "libghc-autobuilder-dev") (Just (EEQ (buildDebianVersion Nothing "${Source-Version}" Nothing))) Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "debootstrap") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "rsync") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "dupload") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "darcs") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "git") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "tla") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "mercurial") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "subversion") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "apt") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "build-essential") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "quilt") Nothing Nothing))) .
    modL depends (Map.insertWith union (BinPkgName "autobuilder") (singleton (Rel (BinPkgName "curl") Nothing Nothing))) .
    doExecutable (BinPkgName "autobuilder") (InstallFile {execName = "autobuilder", sourceDir = Just ".", destDir = Nothing, destName = "autobuilder"})
