{-# LANGUAGE TemplateHaskell #-}
module Debian.NewDist.Version (myVersion) where

import Data.Version (showVersion)
import Distribution.Simple.Utils (findPackageDesc)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)

-- Compiles in the current version of the autobuilder by looking in the .cabal file.
-- To convert this to type Version, remove ". showVersion" from below.
myVersion :: String
myVersion = $(runIO (findPackageDesc "." >>=
                              readPackageDescription silent >>=
                              return . pkgVersion . package . packageDescription) >>=
                       lift . showVersion)
