import CabalDebian.Flags (Flags(..), DebAction(..), withFlags, debianize)
import CabalDebian.SubstVars (substvars)
import Debian.Debianize.Types.Atoms (compilerVersion)

-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.
main :: IO ()
main =
  withFlags $ \ fs ->
      case debAction fs of
        SubstVar debType -> substvars (dryRun fs) (verbosity fs) (compilerVersion fs) (cabalFlagAssignments fs) fs debType
        Debianize -> debianize "." fs
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
