-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.

import Control.Monad.State (get, lift)
import Data.Lens.Lazy (getL)
import Data.List as List (unlines)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Facts.Lenses (debAction)
import Debian.Debianize.Facts.Monad (DebT, evalDebT)
import Debian.Debianize.Facts.Types (Top(Top), DebAction(Debianize, SubstVar, Usage), newAtoms)
import Debian.Debianize.Finalize (debianization)
import Debian.Debianize.Options (compileCommandlineArgs, compileEnvironmentArgs, options)
import Debian.Debianize.Output (doDebianizeAction)
import Debian.Debianize.SubstVars (substvars)
import Prelude hiding (unlines, writeFile, init)
import System.Console.GetOpt (usageInfo)
import System.Environment (getProgName)

top :: Top
top = Top "."

main :: IO ()
main = cabalDebianMain debianDefaultAtoms

-- | The main function for the cabal-debian executable.
cabalDebianMain :: DebT IO () -> IO ()
cabalDebianMain init =
    -- This picks up the options required to decide what action we are
    -- taking.  Yes, it does get repeated in the call to debianize.
    evalDebT (init >> compileEnvironmentArgs >> compileCommandlineArgs >>
              get >>= return . getL debAction >>= finish) newAtoms
    where
      finish (SubstVar debType) = substvars top debType
      finish Debianize = debianization top init (return ()) >> doDebianizeAction top
      finish Usage = do
          progName <- lift getProgName
          let info = unlines [ "Typical usage is to cd to the top directory of the package's unpacked source and run: "
                             , ""
                             , "    " ++ progName ++ " --maintainer 'Maintainer Name <maintainer@email>'."
                             , ""
                             , "This will read the package's cabal file and any existing debian/changelog file and"
                             , "deduce what it can about the debianization, then it will create or modify files in"
                             , "the debian subdirectory.  Note that it will not remove any files in debian, and"
                             , "these could affect the operation of the debianization in unknown ways.  For this"
                             , "reason I recommend either using a pristine unpacked directory each time, or else"
                             , "using a revision control system to revert the package to a known state before running."
                             , "The following additional options are available:" ]
          lift $ putStrLn (usageInfo info options)
