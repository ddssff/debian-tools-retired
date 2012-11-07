-- | This library was derived from the source code for the
-- cabal-debian tool.  At this time it has two purposes - to link with
-- the cabal-debian executable, and to allow calls to the two top
-- level functions that cabal-debian calls: 'debianize' and
-- 'substvar'.
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
