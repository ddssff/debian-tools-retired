{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.PackageVersion
    ( PackageVersion(..)
    , PkgVersion(..)
    , prettyPkgVersion
    , readPkgVersion
    , showPkgVersion
    ) where

import Control.Arrow (second)
import Data.Text (Text)
import Debian.Arch (Arch(..))
import qualified Debian.Control.Text as T
import qualified Debian.Relation as B -- ( PkgName, prettyPkgName, Relations, BinPkgName(..), SrcPkgName(..) )
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Release (Section(..))
import Debian.Version (DebianVersion, prettyDebianVersion, parseDebianVersion)
import System.Posix.Types ( FileOffset )
import Text.PrettyPrint.ANSI.Leijen (Doc, text, (<>), Pretty(pretty))

class (Eq a, Ord a) => PackageVersion a where
    pkgName :: a -> BinPkgName
    pkgVersion :: a -> DebianVersion

-- |This is an old type which is still used to interface with the
-- Debian.Relation module.
data PkgVersion = PkgVersion { getName :: BinPkgName
                             , getVersion :: DebianVersion
                             } deriving (Eq, Ord, Show)

instance PackageVersion PkgVersion where
    pkgName = getName
    pkgVersion = getVersion

prettyPkgVersion :: PkgVersion -> Doc
prettyPkgVersion v = pretty (getName v) <> text "=" <> prettyDebianVersion (getVersion v)

showPkgVersion :: PkgVersion -> String
showPkgVersion v = show (prettyPkgVersion v)

readPkgVersion :: String -> PkgVersion
readPkgVersion s = case second (parseDebianVersion . (drop 1)) (span (/= '=') s) of
                     (n, v) -> PkgVersion { getName = BinPkgName n, getVersion = v }
