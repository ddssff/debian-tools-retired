import Distribution.Debian.Config (Flags(..), DebAction(..))
import Distribution.Debian.Debianize (debianize)
import Distribution.Debian.Options (withFlags)
import Distribution.Debian.SubstVars (substvars)
import Prelude hiding (catch)

-- | This is the main function of the cabal-debian executable.  This is
-- generally run by the autobuilder to debianize packages that don't have
-- any custom debianization code in Setup.hs.
main :: IO ()
main =
  withFlags $ \ flags ->
      case debAction flags of
        SubstVar debType -> substvars flags debType
        Debianize -> debianize "dist-cabal/build" flags
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
