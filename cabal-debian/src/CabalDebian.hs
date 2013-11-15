-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.

import Data.Lens.Lazy (setL)
import Data.Map as Map (fromList)
import Data.Monoid (mempty)
import Data.Version (Version(Version))
import Debian.Debianize.Atoms (cabalDebian)
import Debian.Debianize.Lenses (Atoms, epochMap)
import Debian.Debianize.VersionSplits (mapCabal, splitCabal)
import Distribution.Package (PackageName(PackageName))

main :: IO ()
main = cabalDebian defaultAtoms

defaultAtoms :: Atoms
defaultAtoms =
    setL epochMap (fromList [(PackageName "HaXml", 1), (PackageName "HTTP", 1)]) .
    splitCabal (PackageName "parsec") "parsec2" (Version [3] []) .
    mapCabal (PackageName "parsec") "parsec3" .
    splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] []) .
    mapCabal (PackageName "QuickCheck") "quickcheck2" .
    mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools" $
    mempty
