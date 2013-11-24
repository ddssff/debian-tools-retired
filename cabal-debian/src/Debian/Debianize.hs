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
    ( Debian.Debianize.Finalize.debianization

    , Debian.Debianize.Output.doDebianizeAction
    , Debian.Debianize.Output.runDebianizeScript
    , Debian.Debianize.Output.writeDebianization
    , Debian.Debianize.Output.describeDebianization
    , Debian.Debianize.Output.compareDebianization
    , Debian.Debianize.Output.validateDebianization

    , Debian.Debianize.Details.debianDefaultAtoms
    , Debian.Debianize.Details.seereasonDefaultAtoms

    , Debian.Debianize.Goodies.tightDependencyFixup
    , Debian.Debianize.Goodies.doExecutable
    , Debian.Debianize.Goodies.doServer
    , Debian.Debianize.Goodies.doWebsite
    , Debian.Debianize.Goodies.doBackups

    , Debian.Debianize.Input.inputDebianization
    , Debian.Debianize.Input.inputDebianizationFile
    , Debian.Debianize.Input.inputChangeLog
    , Debian.Debianize.Input.inputLicenseFile
    , Debian.Debianize.Input.inputMaintainer

    -- * Deb monad - 'Debian.Debianize.Monad'
    , DebT, runDebT, execDebT, evalDebT, DebM, runDebM, execDebM, evalDebM

    , Debian.Debianize.Facts.Monad.mapCabal
    , Debian.Debianize.Facts.Monad.splitCabal
    , Debian.Debianize.Options.compileArgs
    , Debian.Debianize.SubstVars.substvars

    -- * Utility functions

    , Debian.Debianize.Utility.withCurrentDirectory
    , Debian.Debianize.Utility.buildDebVersionMap
    , Debian.Debianize.Utility.dpkgFileMap
    , Debian.Debianize.Utility.debOfFile

    -- * TBD

    , module Debian.Debianize.Facts.Lenses
    , module Debian.Debianize.Facts.Types
    , module Debian.Policy
{-
    , module Debian.Debianize.Bundled
    , module Debian.Debianize.ControlFile
    , module Debian.Debianize.Files
    , module Debian.Debianize.Finalize
    , module Debian.Debianize.Interspersed
    , module Debian.Debianize.VersionSplits
-}
    ) where

-- import Debian.Debianize.Bundled
import Debian.Debianize.Details
-- import Debian.Debianize.Files
import Debian.Debianize.Finalize
import Debian.Debianize.Goodies
import Debian.Debianize.Input
-- import Debian.Debianize.Interspersed
import Debian.Debianize.Facts.Lenses
import Debian.Debianize.Facts.Monad
import Debian.Debianize.Facts.Types hiding (maintainer, description, depends, conflicts)
import Debian.Debianize.Options
import Debian.Debianize.Output
import Debian.Debianize.SubstVars
import Debian.Debianize.Utility
-- import Debian.Debianize.VersionSplits
import Debian.Policy
