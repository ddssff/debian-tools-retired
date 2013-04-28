#!/usr/bin/runhaskell

import Control.Exception (SomeException)
import Distribution.Simple
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         postBuild = \ _ _ _ _ -> runTestScript
       , runTests = \ _ _ _ _ -> runTestScript
       }

runTestScript =
    -- system "runhaskell Test/Test.hs" >>= \ code ->
    system "dist/build/autobuilder-tests/autobuilder-tests" >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
