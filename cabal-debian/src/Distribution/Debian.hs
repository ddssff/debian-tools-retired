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
-- > % CABALDEBIAN='["-n"]' ghc -e 'Distribution.Debian.debianize Distribution.Debian.defaultFlags'
-- 
-- To actually create or update the debianization
-- 
-- > % ghc -e 'Distribution.Debian.debianize Distribution.Debian.defaultFlags'
-- > % sudo dpkg-buildpackage
-- 
-- At this point you may need to modify Cabal.defaultFlags to achieve
-- specific packaging goals.  Create a module for this in debian/Debianize.hs:
-- 
-- > import Distribution.Debian (Flags(..), defaultFlags)
-- > main = debianize (defaultFlags { selfDepend = True -- Add a build dependency on libghc-cabal-debian-dev to the debianization
-- >                                , extraDevDeps = "haskell-hsx-utils" : extraDevDeps defaultFlags})
-- 
-- And then to run it,
-- 
-- > 
module Distribution.Debian
    ( debianize
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
    , module Distribution.Debian.Server
    ) where

import Distribution.Debian.DebHelper (DebAtom(..))
import Distribution.Debian.Debianize (debianize, withEnvironmentArgs, withEnvironmentFlags, putEnvironmentArgs)
import Distribution.Debian.Config (Flags(..), DebAction(..), defaultFlags)
import Distribution.Debian.Options (compileArgs)
import Distribution.Debian.PackageInfo (DebType)
import Distribution.Debian.Server
import Distribution.Debian.SubstVars (substvars)
