import Data.Lens.Lazy (setL)
import Debian.Changes (ChangeLog)
import Debian.Debianize (Atoms, Top(..), sourcePackageName, inputChangeLog, debianize, changelog, copyright, doExecutable, sourceFormat, SourceFormat(Native3), InstallFile(..))
import Debian.Debianize.Details (seereasonDefaultAtoms)
import Debian.Relation (BinPkgName(BinPkgName), SrcPkgName(SrcPkgName))
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
    doExecutable (BinPkgName "autobuilder") (InstallFile {execName = "autobuilder", sourceDir = Just ".", destDir = Nothing, destName = "autobuilder"})
