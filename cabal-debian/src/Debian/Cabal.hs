-- | This library was derived from the source code for the
-- cabal-debian tool.  At this time it has two purposes - to link with
-- the cabal-debian executable, and to allow calls to the two top
-- level functions that cabal-debian calls: 'debianize' and
-- 'substvar'.
-- 
-- QUICK START: You can either call the cabal-debian executable, or
-- for more power and flexibility you can construct a
-- 'Distribution.Debian.Flags' record and pass it to the
-- 'Distribution.Debian.debianize' function file.  The 'Distribution.Debian.Debianize.callDebianize' function
-- retrieves extra arguments from the @CABALDEBIAN@ environment variable and calls 'Distribution.Debian.debianize'
-- with the build directory set as it would be when the packages is built by @dpkg-buildpackage@.
-- 
-- To see what your debianization would produce, or how it differs
-- from the debianization already present:
-- 
-- > % ghc -e 'Distribution.Debian.callDebianize ["-n"]'
-- 
-- To actually create the debianization and then build the debs,
-- 
-- > % ghc -e 'Distribution.Debian.callDebianize []'
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
module Debian.Cabal
    ( {- debianize
    , callDebianize
    , runDebianize
    , withEnvironmentArgs
    , withEnvironmentFlags
    , putEnvironmentArgs
    , substvars
    , Flags(..)
    , DebAction(..)
    , defaultFlags
    , compileArgs
    , DebType
    , DebAtom(..)
    , tightDependencyFixup
    , module Debian.Debianize.Server -}
    ) where

{-
import CabalDebian.Flags (Flags(..), DebAction(..), defaultFlags, compileArgs, withEnvironmentArgs, withEnvironmentFlags, putEnvironmentArgs,
                          debianize, callDebianize, runDebianize)
import Debian.Cabal.PackageInfo (DebType)
import Debian.Cabal.SubstVars (substvars)
import Debian.Debianize.Combinators (tightDependencyFixup)
import Debian.Debianize.Server
import Debian.Debianize.Types (DebAtom(..))
-}
