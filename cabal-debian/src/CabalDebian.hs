import CabalDebian.Flags (Flags(..), DebAction(..), withFlags, debianize)
import CabalDebian.SubstVars (substvars)
import Data.Set (toList)
import Debian.Debianize.Types.Atoms (compilerVersion, cabalFlagAssignments, defaultAtoms, flags)

-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.
main :: IO ()
main =
  withFlags defaultAtoms $ \ atoms ->
      case debAction (flags atoms) of
        SubstVar debType -> substvars (dryRun (flags atoms)) (verbosity (flags atoms)) (compilerVersion atoms) (toList (cabalFlagAssignments atoms)) atoms debType
        Debianize -> debianize "." atoms
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
