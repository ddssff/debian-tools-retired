{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaultAtoms
    , seereasonDefaultAtoms
    ) where

import Data.Version (Version(Version))
import Debian.Debianize.Monad (DebT, epochMap, mapCabal, splitCabal, missingDependency)
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName))

debianDefaultAtoms :: Monad m => DebT m ()
debianDefaultAtoms =
    do epochMap (PackageName "HaXml") 1
       epochMap (PackageName "HTTP") 1
       mapCabal (PackageName "parsec") "parsec3"
       splitCabal (PackageName "parsec") "parsec2" (Version [3] [])
       mapCabal (PackageName "QuickCheck") "quickcheck2"
       splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools"

seereasonDefaultAtoms :: Monad m => DebT m ()
seereasonDefaultAtoms =
    do debianDefaultAtoms

       missingDependency (BinPkgName "libghc-happstack-authenticate-9-doc")

       mapCabal (PackageName "clckwrks") "clckwrks"
       splitCabal (PackageName "clckwrks") "clckwrks-13" (Version [0, 14] [])
       splitCabal (PackageName "clckwrks") "clckwrks-14" (Version [0, 15] [])

       mapCabal (PackageName "blaze-html") "blaze-html"
       splitCabal (PackageName "blaze-html") "blaze-html-5" (Version [0, 6] [])

       mapCabal (PackageName "happstack-authenticate") "happstack-authenticate"
       splitCabal (PackageName "happstack-authenticate") "happstack-authenticate-9" (Version [0, 10] [])

       mapCabal (PackageName "http-types") "http-types"
       splitCabal (PackageName "http-types") "http-types-7" (Version [0, 8] [])

       mapCabal (PackageName "web-plugins") "web-plugins"
       splitCabal (PackageName "web-plugins") "web-plugins-1" (Version [0, 2] [])

       mapCabal (PackageName "case-insensitive") "case-insensitive"
       splitCabal (PackageName "case-insensitive") "case-insensitive-0" (Version [1] [])
