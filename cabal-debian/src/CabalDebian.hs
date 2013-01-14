import CabalDebian.Flags (Flags(..), DebAction(..), withFlags, debianize)
import CabalDebian.SubstVars (substvars)
import Debian.Debianize.Types.Atoms (defaultAtoms, flags)

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
        SubstVar debType -> substvars atoms debType
        Debianize -> debianize "." atoms
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
