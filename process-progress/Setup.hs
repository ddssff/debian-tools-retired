import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Directory
import System.Exit
import System.Process

main = copyFile "debian/changelog" "changelog" >>
       defaultMainWithHooks simpleUserHooks {
         postBuild =
             \ _ _ _ lbi ->
                 case buildDir lbi of
                   "dist-ghc/build" -> return ()
                   _ -> runTestScript lbi
       , runTests = \ _ _ _ lbi -> runTestScript lbi
       }

runTestScript lbi =
    system (buildDir lbi ++ "/tests/tests") >>= \ code ->
    if code == ExitSuccess then return () else error "unit test failure"
