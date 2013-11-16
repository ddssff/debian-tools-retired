import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Set as Set (union, singleton)
import Debian.Changes (ChangeLog)
import Debian.Debianize (Top(..), SourceFormat(Native3), InstallFile(..), doExecutable, inputChangeLog, debianize)
import Debian.Debianize.Details (seereasonDefaultAtoms)
import Debian.DebT (Atoms, DebT, runDebT, sourcePackageName, copyright, sourceFormat, changelog, depends)
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName), VersionReq(EEQ), Relation(Rel))
import Debian.Version (buildDebianVersion)
import Distribution.License (License(AllRightsReserved))
import Prelude hiding (log)

main =
    do log <- inputChangeLog top
       debianize top (customize log) seereasonDefaultAtoms
    where
      top = Top "."

customize :: ChangeLog -> DebT IO ()
customize log =
    do sourcePackageName (SrcPkgName "autobuilder")
       copyright (Left AllRightsReserved)
       sourceFormat Native3
       changelog log
       mapM (depends (BinPkgName "autobuilder"))
            [ Rel (BinPkgName "ghc") Nothing Nothing
            , Rel (BinPkgName "libghc-autobuilder-dev") (Just (EEQ (buildDebianVersion Nothing "${Source-Version}" Nothing))) Nothing
            , Rel (BinPkgName "debootstrap") Nothing Nothing
            , Rel (BinPkgName "rsync") Nothing Nothing
            , Rel (BinPkgName "dupload") Nothing Nothing
            , Rel (BinPkgName "darcs") Nothing Nothing
            , Rel (BinPkgName "git") Nothing Nothing
            , Rel (BinPkgName "tla") Nothing Nothing
            , Rel (BinPkgName "mercurial") Nothing Nothing
            , Rel (BinPkgName "subversion") Nothing Nothing
            , Rel (BinPkgName "apt") Nothing Nothing
            , Rel (BinPkgName "build-essential") Nothing Nothing
            , Rel (BinPkgName "quilt") Nothing Nothing
            , Rel (BinPkgName "curl") Nothing Nothing
            , Rel (BinPkgName "debian-archive-keyring") Nothing Nothing
            , Rel (BinPkgName "seereason-keyring") Nothing Nothing
            , Rel (BinPkgName "ubuntu-keyring") Nothing Nothing ]
       doExecutable (BinPkgName "autobuilder") (InstallFile {execName = "autobuilder", sourceDir = Just ".", destDir = Nothing, destName = "autobuilder"})
