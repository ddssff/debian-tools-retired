import Distribution.Debian.Config (Flags(..), DebAction(..))
import Distribution.Debian.Debianize (debianize)
import Distribution.Debian.Options (withFlags)
import Distribution.Debian.SubstVars (substvars)
import Prelude hiding (catch)

-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.
main :: IO ()
main =
  withFlags $ \ flags ->
      case debAction flags of
        SubstVar debType -> substvars flags debType
        Debianize -> debianize id flags
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
