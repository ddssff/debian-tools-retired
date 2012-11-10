-- | This library was derived from the source code for the
-- cabal-debian tool.  At this time it has two purposes - to link with
-- the cabal-debian executable, and to allow calls to the two top
-- level functions that cabal-debian calls: 'debianize' and
-- 'substvar'.
-- 
-- QUICK START: You can either call the cabal-debian executable, which
-- is how the autobuilder debianizes library packages, or you can
-- modify a package's Setup.hs file to call autobuilderDebianize for
-- more sophisticated debianization.  This normally looks like a call
-- to autobuilderDebianize:
-- 
-- > import Distribution.Debian (Flags(..), defaultFlags, autobuilderDebianize,
-- >                             Executable(..), Server(..), Site(..))
-- > import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
-- > 
-- > main = defaultMainWithHooks simpleUserHooks { postConf = \ _ _ _ lbi -> autobuilderDebianize lbi flags }
-- 
-- Now define flags:
-- 
-- ^ flags :: Flags
-- ^ flags = defaultFlags { ... }
-- 
-- To see what your debianization would produce run
-- 
-- > runhaskell Setup configure
-- 
-- To actually create the files, and then build:
-- 
-- > runhaskell Setup configure --builddir=debian
-- > dpkg-buildpackage
module Distribution.Debian
    ( debianize
    , autobuilderDebianize
    , substvars
    , Flags(..)
    , DebAction(..)
    , defaultFlags
    , DebType
    , module Distribution.Debian.Server
    ) where

import Distribution.Debian.Debianize (debianize, autobuilderDebianize)
import Distribution.Debian.Config (Flags(..), DebAction(..), defaultFlags)
import Distribution.Debian.PackageInfo (DebType)
import Distribution.Debian.Server
import Distribution.Debian.SubstVars (substvars)
