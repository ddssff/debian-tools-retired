{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaultAtoms
    , seereasonDefaultAtoms
    ) where

import Data.Lens.Lazy (modL, setL)
import Data.Map as Map (fromList)
import Data.Monoid (mempty)
import Data.Set as Set (insert)
import Debian.Relation (BinPkgName(BinPkgName))
import Data.Version (Version(Version))
import Debian.Debianize (Atoms, missingDependencies, epochMap)
import Debian.Debianize.VersionSplits (mapCabal, splitCabal)
import Distribution.Package (PackageName(PackageName))

debianDefaultAtoms :: Atoms
debianDefaultAtoms =
    setL epochMap (Map.fromList [(PackageName "HaXml", 1), (PackageName "HTTP", 1)]) .
    splitCabal (PackageName "parsec") "parsec2" (Version [3] []) .
    mapCabal (PackageName "parsec") "parsec3" .
    splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] []) .
    mapCabal (PackageName "QuickCheck") "quickcheck2" .
    mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools" $
    mempty

seereasonDefaultAtoms :: Atoms
seereasonDefaultAtoms =
    modL missingDependencies (Set.insert (BinPkgName "libghc-happstack-authenticate-9-doc")) .

    splitCabal (PackageName "clckwrks") "clckwrks-13" (Version [0, 14] []) .
    splitCabal (PackageName "clckwrks") "clckwrks-14" (Version [0, 15] []) .
    mapCabal (PackageName "clckwrks") "clckwrks" .
    splitCabal (PackageName "blaze-html") "blaze-html-5" (Version [0, 6] []) .
    mapCabal (PackageName "blaze-html") "blaze-html" .
    splitCabal (PackageName "happstack-authenticate") "happstack-authenticate-9" (Version [0, 10] []) .
    mapCabal (PackageName "happstack-authenticate") "happstack-authenticate" .
    splitCabal (PackageName "http-types") "http-types-7" (Version [0, 8] []) .
    mapCabal (PackageName "http-types") "http-types" .
    splitCabal (PackageName "web-plugins") "web-plugins-1" (Version [0, 2] []) .
    mapCabal (PackageName "web-plugins") "web-plugins" .
    splitCabal (PackageName "case-insensitive") "case-insensitive-0" (Version [1] []) .
    mapCabal (PackageName "case-insensitive") "case-insensitive" $

    debianDefaultAtoms
