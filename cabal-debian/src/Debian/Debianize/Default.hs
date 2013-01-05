module Debian.Debianize.Default
    ( newDebianization
    , newSourceDebDescription
    , newBinaryDebDescription
    ) where

import Data.Monoid (mempty)
import Data.Set (empty)
import Data.Text (Text, pack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Types.Debianization (Debianization(..), SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..))
import Debian.Policy (StandardsVersion, PackageArchitectures, parseMaintainer)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Distribution.License (License)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLogEntry -> Either License Text -> Int -> StandardsVersion -> Debianization
newDebianization (WhiteSpace {}) _ _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization entry@(Entry {}) copy level standards =
    Debianization
      { sourceDebDescription = newSourceDebDescription (SrcPkgName (logPackage entry)) (either error id (parseMaintainer (logWho entry))) standards
      , changelog = ChangeLog [entry]
      , rulesHead = pack $ unlines [ "#!/usr/bin/make -f"
                                   , ""
                                   , "DEB_CABAL_PACKAGE = " ++ logPackage entry
                                   , ""
                                   , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                   , "include /usr/share/cdbs/1/class/hlibrary.mk"
                                   , "" ]
      , compat = level
      , copyright = copy
      , atoms = mempty }

newSourceDebDescription :: SrcPkgName -> NameAddr -> StandardsVersion -> SourceDebDescription
newSourceDebDescription src who standards =
    SourceDebDescription
      { source = src -- SrcPkgName (logPackage entry)
      , maintainer = who -- parseMaintainer (logWho entry)
      , changedBy = Nothing
      , uploaders = []
      , dmUploadAllowed = False
      , priority = Nothing
      , section = Nothing
      , buildDepends = []
      , buildConflicts = []
      , buildDependsIndep  = []
      , buildConflictsIndep  = []
      , standardsVersion = standards
      , vcsFields = empty
      , xFields = empty
      , binaryPackages = [] }

newBinaryDebDescription :: BinPkgName -> PackageArchitectures -> BinaryDebDescription
newBinaryDebDescription bin arch =
    BinaryDebDescription
      { package = bin
      , architecture = arch
      , binarySection = Nothing
      , binaryPriority = Nothing
      , essential = False
      , description = mempty
      , relations = newPackageRelations }

newPackageRelations :: PackageRelations
newPackageRelations =
    PackageRelations
      { depends = []
      , recommends = []
      , suggests = []
      , preDepends = []
      , breaks = []
      , conflicts = []
      , provides = []
      , replaces = []
      , builtUsing = [] }
