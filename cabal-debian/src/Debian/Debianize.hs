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
    ( Debian.Debianize.Atoms.debianize
    , Debian.Debianize.Atoms.debianization

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
    , Debian.Debianize.Input.inputCabalization
    , Debian.Debianize.Input.inputLicenseFile
    , Debian.Debianize.Input.inputMaintainer

    -- * Deb monad - 'Debian.Debianize.Monad'
    , DebT, runDebT, execDebT, evalDebT, DebM, runDebM, execDebM, evalDebM

    -- * Modes of operation
    , verbosity
    , lookVerbosity
    , dryRun
    , debAction
    , cabalFlagAssignment
    , lookCabalFlagAssignments

    -- * Repository info
    , execMap
    , epochMap
    , missingDependency
    , Debian.Debianize.Monad.mapCabal
    , Debian.Debianize.Monad.splitCabal
    , extraLibMap

    -- * Source Package Info
    , sourcePackageName
    , revision
    , debVersion
    , maintainer
    , copyright
    , sourceArchitecture
    , sourcePriority
    , sourceSection
    , compat
    , sourceFormat
    , changelog
    , comments
    , standards
    , dataDir
    , rulesHead
    , rulesFragment
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageName
    , buildDir
    , watch

    -- * Source Package Build Dependencies
    , buildDeps
    , buildDepsIndep
    , omitLTDeps
    , compilerVersion
    , lookCompilerVersion

    -- * Binary Package Info
    , binaryArchitectures
    , description
    , executable
    , serverInfo
    , website
    , backups
    , apacheSite
    , extraDevDeps
    , postInst
    , postRm
    , preInst
    , preRm
    , binaryPriorities
    , binarySections
    , installInit
    -- * Binary Package Dependencies
    , conflicts
    , provides
    , depends
    , replaces
    -- * Binary Package Files
    , link
    , install
    , installTo
    , installData
    , file
    , installDir
    , logrotateStanza
    , installCabalExec
    , installCabalExecTo

    -- * Unknown, Obsolete, or Internal
    , flags -- obsolete
    , validate -- obsolete
    , packageDescription -- Internal
    , warning -- no-op?
    , compiler
    , intermediateFile
    , packageInfo
    , control

    , Debian.Debianize.Options.compileArgs
    , Debian.Debianize.SubstVars.substvars
{-
    , module Debian.Debianize.Bundled
    , module Debian.Debianize.ControlFile
    , module Debian.Debianize.Files
    , module Debian.Debianize.Finalize
    , module Debian.Debianize.Interspersed
    , module Debian.Debianize.Types
    , module Debian.Debianize.Utility
    , module Debian.Debianize.VersionSplits
    , module Debian.Policy
-}
    ) where

import Debian.Debianize.Atoms
-- import Debian.Debianize.Bundled
-- import Debian.Debianize.ControlFile hiding (depends, conflicts, maintainer, description, section)
import Debian.Debianize.Details
-- import Debian.Debianize.Files
-- import Debian.Debianize.Finalize
import Debian.Debianize.Goodies
import Debian.Debianize.Input
-- import Debian.Debianize.Interspersed
import Debian.Debianize.Monad
import Debian.Debianize.Options
import Debian.Debianize.SubstVars
-- import Debian.Debianize.Types
-- import Debian.Debianize.Utility
-- import Debian.Debianize.VersionSplits
-- import Debian.Policy
