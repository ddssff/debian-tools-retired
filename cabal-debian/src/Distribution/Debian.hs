-- | This library was derived from the source code for the
-- cabal-debian tool.  At this time it has two purposes - to link with
-- the cabal-debian executable, and to allow calls to the two top
-- level functions that cabal-debian calls: 'debianize' and
-- 'substvar'.
-- 
-- QUICK START: You can either call the cabal-debian executable, which
-- is how the autobuilder debianizes library packages, or you can
-- add a function named debianize to your package's Setup.hs file.  This
-- function should construct a 'Flags' record and call 'Distribution.Debian.debianize'
-- 
-- > import qualified Distribution.Debian as Cabal
-- > debianize = Cabal.debianize (Cabal.defaultFlags)
-- 
-- To see what your debianization would produce, or how it differs
-- from the debianization already present:
-- 
-- > ghc Setup.hs -e 'Cabal.putEnvironmentArgs ["--dry-run"] >> debianize'
-- 
-- To actually create the files, and then build:
-- 
-- > ghc Setup.hs -e 'debianize'
-- > sudo dpkg-buildpackage
-- 
-- At this point you may need to modify Cabal.defaultFlags to achieve
-- specific packaging goals.
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
