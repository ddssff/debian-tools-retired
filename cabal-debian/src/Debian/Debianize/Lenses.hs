{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Lenses {-# DEPRECATED "Use monadic functions in Debian.Debianize.Monad: execDeb (monadic stuff) atoms" #-}
    ( Atoms
    , Flags(..)
    -- * Modes of operation
    , verbosity
    , dryRun
    , validate
    , debAction
    , flags
    , warning
    -- * Cabal info
    , compilerVersion
    , packageDescription
    , buildDir
    , dataDir
    , compiler
    , extraLibMap
    , execMap
    , cabalFlagAssignments
    -- * Global debian info
    , debianNameMap
    , epochMap
    -- * High level information about the debianization
    , description
    , executable
    , serverInfo
    , website
    , backups
    , apacheSite
    , missingDependencies
    , utilsPackageNames
    , sourcePackageName
    , revision
    , debVersion
    , maintainer
    , packageInfo
    , omitLTDeps
    , noProfilingLibrary
    , noDocumentationLibrary
    , copyright
    , sourceArchitecture
    , binaryArchitectures
    , sourcePriority
    , binaryPriorities
    , sourceSection
    , binarySections
    , buildDeps
    , buildDepsIndep
    , depends
    , conflicts
    , replaces
    , provides
    , extraDevDeps
    -- * Debianization files and file fragments
    , rulesHead
    , rulesFragments
    , postInst
    , postRm
    , preInst
    , preRm
    , compat
    , sourceFormat
    , watch
    , changelog
    , comments
    , control
    , standards
    , logrotateStanza
    , link
    , install
    , installTo
    , installData
    , file
    , installCabalExec
    , installCabalExecTo
    , installDir
    , installInit
    , intermediateFiles
    ) where

import Debian.Debianize.Internal.Lenses
