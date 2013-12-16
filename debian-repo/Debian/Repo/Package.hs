{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package
    ( binaryPackageSourceVersion
    , sourcePackageBinaryNames
    ) where

import Data.List as List (map)
import qualified Data.Text as T (unpack)
import Debian.Control (ControlFunctions(asString), formatParagraph, Paragraph')
import qualified Debian.Control.Text as B (fieldValue, Paragraph)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.PackageID (PackageID(packageName, packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage(packageID, packageInfo), SourcePackage(sourceParagraph))
import Debian.Version (DebianVersion, parseDebianVersion)
import qualified Debian.Version as V (buildDebianVersion, epoch, revision, version)
import Text.Regex (matchRegex, mkRegex, splitRegex)

-- | Return the name and version number of the source package that
-- generated this binary package.
binaryPackageSourceVersion :: BinaryPackage -> Maybe (String, DebianVersion)
binaryPackageSourceVersion package =
    let binaryName = packageName . packageID $ package
        binaryVersion = packageVersion . packageID $ package in
    binarySourceVersion' binaryName binaryVersion (packageInfo package)

binarySourceVersion' :: (ControlFunctions a) => BinPkgName -> DebianVersion -> Paragraph' a -> Maybe (String, DebianVersion)
binarySourceVersion' binaryName binaryVersion paragraph =
    case (B.fieldValue "Source" paragraph) of
      Just source' ->
          case matchRegex re (asString source') of
            Just [name, _, ""] -> Just (name, binaryVersion)
            Just [name, _, version] -> Just (name, copyEpoch binaryVersion (parseDebianVersion version))
            _ -> error "internal error"
      Nothing ->
          Just (asString (unBinPkgName binaryName), binaryVersion)
    where
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"
      -- In the Packages file the version number in the Source: field has
      -- the epoch number stripped off.  I don't know why - I should search
      -- the Debian policy manual for this.  This puts it back on.
      copyEpoch src dst = V.buildDebianVersion (V.epoch src) (V.version dst) (V.revision dst)

sourcePackageBinaryNames :: SourcePackage -> [BinPkgName]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)
    where
      sourceBinaryNames :: B.Paragraph -> [BinPkgName]
      sourceBinaryNames paragraph =
          case B.fieldValue "Binary" paragraph of
            Just names -> List.map BinPkgName (splitRegex (mkRegex "[ ,\t\n]+") (T.unpack names))
            _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ paragraph))
