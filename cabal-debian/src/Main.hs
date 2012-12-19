import Distribution.Debian.Config (Flags(..), Config(..), DebAction(..))
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
  withFlags $ \ fs ->
      let config = Config {flags = fs, modifyAtoms = id} in
      case debAction fs of
        SubstVar debType -> substvars config debType
        Debianize -> debianize config
        Usage -> error "Unexpected debAction: usage" -- this should have happened in withFlags
