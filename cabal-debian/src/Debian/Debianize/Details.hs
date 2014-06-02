{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaultAtoms
    ) where

import Data.Version (Version(Version))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Types.Atoms as T (epochMap)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude ((++=))
import Distribution.Package (PackageName(PackageName))

-- | Some details about the debian repository - special cases for how
-- some cabal packages are mapped to debian package names.
debianDefaultAtoms :: Monad m => DebT m ()
debianDefaultAtoms =
    do T.epochMap ++= (PackageName "HaXml", 1)
       T.epochMap ++= (PackageName "HTTP", 1)
       mapCabal (PackageName "parsec") "parsec3"
       splitCabal (PackageName "parsec") "parsec2" (Version [3] [])
       mapCabal (PackageName "QuickCheck") "quickcheck2"
       splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools"
