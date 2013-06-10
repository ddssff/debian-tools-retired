#!/usr/bin/runhaskell

import Control.Monad (when)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks
       {- { postBuild = \ _ _ _ lbi -> when (buildDir lbi /= "dist-ghc/build") (runTestScript lbi)
          , runTests = \ _ _ _ lbi -> runTestScript lbi } -}

runTestScript lbi =
    system (buildDir lbi ++ "/cabal-debian-tests/cabal-debian-tests") >>= \ code ->
    if code == ExitSuccess then return () else error "unit test failure"


