-- | QUICK START: You can either run the cabal-debian executable, or
-- for more power and flexibility you can construct a
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
-- > import Distribution.Debian (Flags(..), defaultFlags)
-- > main = debianize (defaultFlags { extraDevDeps = "haskell-hsx-utils" : extraDevDeps defaultFlags})
-- 
-- Then to test it,
-- 
-- > % CABALDEBIAN='["-n"]' runhaskell debian/Debianize.hs
-- 
-- and to run it
-- 
-- > % runhaskell debian/Debianize.hs
module Debian.Debianize
    ( module Debian.Debianize.Atoms
    , module Debian.Debianize.Bundled
    , module Debian.Debianize.ControlFile
    , module Debian.Debianize.Debianize
    , module Debian.Debianize.Dependencies
    , module Debian.Debianize.Files
    , module Debian.Debianize.Finalize
    , module Debian.Debianize.Generic
    , module Debian.Debianize.Goodies
    , module Debian.Debianize.Input
    , module Debian.Debianize.Interspersed
    , module Debian.Debianize.Options
    , module Debian.Debianize.SubstVars
    , module Debian.Debianize.Tests
    , module Debian.Debianize.Types
    , module Debian.Debianize.Utility
    , module Debian.Policy
    ) where

import Debian.Debianize.Atoms
import Debian.Debianize.Bundled
import Debian.Debianize.Debianize
import Debian.Debianize.Dependencies
import Debian.Debianize.Files
import Debian.Debianize.Finalize
import Debian.Debianize.Generic
import Debian.Debianize.Goodies
import Debian.Debianize.Input
import Debian.Debianize.Interspersed
import Debian.Debianize.Options
import Debian.Debianize.SubstVars
import Debian.Debianize.Tests
import Debian.Debianize.Types
import Debian.Debianize.ControlFile hiding (depends, conflicts, maintainer, description, section)
import Debian.Debianize.Utility
import Debian.Policy
