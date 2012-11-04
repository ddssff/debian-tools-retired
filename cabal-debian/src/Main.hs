import Distribution.Debian.Config (Flags(..), DebAction(..))
import Distribution.Debian.Debianize (debianize)
import Distribution.Debian.Options (withFlags)
import Distribution.Debian.SubstVars (substvars)
import Prelude hiding (catch)

main :: IO ()
main =
  withFlags $ \ flags ->
      case debAction flags of
        SubstVar debType -> substvars flags debType
        Debianize -> debianize flags
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
