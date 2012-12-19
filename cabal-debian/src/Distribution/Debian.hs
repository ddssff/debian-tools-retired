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
module Distribution.Debian
    ( debianize
    , callDebianize
    , runDebianize
    , withEnvironmentArgs
    , withEnvironmentFlags
    , putEnvironmentArgs
    , substvars
    , Flags(..)
    , DebAction(..)
    , Config(..)
    , defaultFlags
    , defaultConfig
    , compileArgs
    , DebType
    , DebAtom(..)
    , tightDependencyFixup
    , module Distribution.Debian.Server
    ) where

import Distribution.Debian.DebHelper (DebAtom(..), tightDependencyFixup)
import Distribution.Debian.Debianize (debianize, callDebianize, runDebianize, withEnvironmentArgs, withEnvironmentFlags, putEnvironmentArgs)
import Distribution.Debian.Config (Flags(..), DebAction(..), defaultFlags, Config(..), defaultConfig)
import Distribution.Debian.Options (compileArgs)
import Distribution.Debian.PackageInfo (DebType)
import Distribution.Debian.Server
import Distribution.Debian.SubstVars (substvars)
