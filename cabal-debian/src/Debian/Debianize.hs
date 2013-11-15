-- | QUICK START: You can either run the @cabal-debian --debianize@, or
-- for more power and flexibility you can put a @Debianize.hs@ script in
-- the package's @debian@ subdirectory.
-- 'Debian.Debianize.Atoms' value and pass it to the
-- 'Debian.Debianize.debianize' function.  The
-- 'Debian.Debianize.callDebianize' function retrieves extra arguments
-- from the @CABALDEBIAN@ environment variable and calls
-- 'Debian.Debianize.debianize' with the build directory set as it
-- would be when the packages is built by @dpkg-buildpackage@.
-- 
-- To see what your debianization would produce, or how it differs
-- from the debianization already present:
-- 
-- > % cabal-debian --debianize -n
-- 
-- This is equivalent to the library call
-- 
-- > % ghc -e 'Debian.Debianize.callDebianize ["-n"]'
-- 
-- To actually create the debianization and then build the debs,
-- 
-- > % ghc -e 'Debian.Debianize.callDebianize []'
-- > % sudo dpkg-buildpackage
-- 
-- At this point you may need to modify Cabal.defaultFlags to achieve
-- specific packaging goals.  Create a module for this in debian/Debianize.hs:
-- 
-- > import Data.Lens.Lazy
-- > import Data.Map as Map (insertWith)
-- > import Data.Set as Set (union, singleton)
-- > import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
-- > import Debian.Debianize (defaultAtoms, depends, debianization, writeDebianization)
-- > main = debianization "." defaultAtoms >>=
-- >        return . modL depends (insertWith union (BinPkgName "cabal-debian") (singleton (Rel (BinPkgName "debian-policy") Nothing Nothing))) >>=
-- >        writeDebianization "."
-- 
-- Then to test it,
-- 
-- > % CABALDEBIAN='["-n"]' runhaskell debian/Debianize.hs
-- 
-- or equivalently
-- 
-- > % ghc -e 'Debian.Debianize.runDebianize ["-n"]'
-- 
-- and to run it for real:
-- 
-- > % runhaskell debian/Debianize.hs
module Debian.Debianize
    ( module Debian.Debianize.Atoms
    , module Debian.Debianize.Bundled
    , module Debian.Debianize.ControlFile
    , module Debian.Debianize.Dependencies
    , module Debian.Debianize.Files
    , module Debian.Debianize.Finalize
    , module Debian.Debianize.Goodies
    , module Debian.Debianize.Input
    , module Debian.Debianize.Interspersed
    , module Debian.Debianize.Lenses
    , module Debian.Debianize.Options
    , module Debian.Debianize.SubstVars
    , module Debian.Debianize.Types
    , module Debian.Debianize.Utility
    , module Debian.DebT
    , module Debian.Policy
    ) where

import Debian.Debianize.Atoms
import Debian.Debianize.Bundled
import Debian.Debianize.Dependencies
import Debian.Debianize.Files
import Debian.Debianize.Finalize
import Debian.Debianize.Goodies
import Debian.Debianize.Input
import Debian.Debianize.Interspersed
import Debian.Debianize.Lenses (Atoms)
import Debian.Debianize.Options
import Debian.Debianize.SubstVars
import Debian.Debianize.Types
import Debian.Debianize.ControlFile hiding (depends, conflicts, maintainer, description, section)
import Debian.Debianize.Utility
import Debian.DebT
import Debian.Policy
