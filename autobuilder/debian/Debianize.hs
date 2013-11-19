import Debian.Changes (ChangeLog)
import Debian.Debianize (changelog, copyright, debianize, DebT, depends, doExecutable, inputChangeLog, seereasonDefaultAtoms, sourceFormat, sourcePackageName)
import Debian.Debianize.ControlFile hiding (depends)
import Debian.Debianize.Types (InstallFile(InstallFile, destDir, destName, execName, sourceDir), Top(Top))
import Debian.Policy (SourceFormat(Native3))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), SrcPkgName(SrcPkgName), VersionReq(EEQ))
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
