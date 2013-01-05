{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Orphans where

import Data.Generics (Data, Typeable)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Control (Field'(..))
import Debian.Relation (Relation(..), VersionReq(..), ArchitectureReq(..),
                        BinPkgName(..), SrcPkgName(..))
import Debian.Version (DebianVersion, prettyDebianVersion, parseDebianVersion)
import Distribution.License (License(..))
import Distribution.Version (Version(..), VersionRange, foldVersionRange')
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))

instance Pretty Text where
    pretty = text . unpack

{-
instance Show (Control' String) where
    show _ = "<control file>"

instance Show ChangeLog where
    show _ = "<log entry>"
-}

deriving instance Read ArchitectureReq
deriving instance Read BinPkgName
deriving instance Read ChangeLog
deriving instance Read ChangeLogEntry
-- deriving instance Read NameAddr
deriving instance Read Relation
deriving instance Read SrcPkgName
deriving instance Read VersionReq

deriving instance Show ArchitectureReq
deriving instance Show ChangeLog
deriving instance Show ChangeLogEntry
deriving instance Show Relation
deriving instance Show VersionReq

instance Show DebianVersion where
    show v = "(Debian.Version.parseDebianVersion (" ++ show (show (prettyDebianVersion v)) ++ " :: String))"

instance Read DebianVersion where
    readsPrec _ s =
        case dropPrefix "Debian.Version.parseDebianVersion " s of
          Just s' -> case reads s' :: [(String, String)] of
                       []-> []
                       (v, s'') : _ -> [(parseDebianVersion v, s'')]
          Nothing -> []

dropPrefix :: String -> String -> Maybe String
dropPrefix p s = if isPrefixOf p s then Just (drop (length p) s) else Nothing

deriving instance Data ArchitectureReq
deriving instance Data BinPkgName
deriving instance Data ChangeLog
deriving instance Data ChangeLogEntry
-- deriving instance Data NameAddr
deriving instance Data Relation
deriving instance Data SrcPkgName
deriving instance Data VersionReq

deriving instance Typeable ArchitectureReq
deriving instance Typeable BinPkgName
deriving instance Typeable ChangeLog
deriving instance Typeable ChangeLogEntry
-- deriving instance Typeable NameAddr
deriving instance Typeable Relation
deriving instance Typeable SrcPkgName
deriving instance Typeable VersionReq

deriving instance Ord ChangeLog
deriving instance Ord ChangeLogEntry

{-
instance Pretty SrcPkgName where
    pretty (SrcPkgName x) = pretty x

instance Pretty BinPkgName where
    pretty (BinPkgName x) = pretty x
-}

deriving instance Typeable License
deriving instance Data Version
deriving instance Data License

-- Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.
instance Pretty License where
    pretty (GPL _) = text "GPL"
    pretty (LGPL _) = text "LGPL"
    pretty (Apache _) = text "Apache"
    pretty BSD3 = text "BSD"
    pretty BSD4 = text "BSD-like"
    pretty PublicDomain = text "Public Domain"
    pretty AllRightsReserved = text "Proprietary"
    pretty OtherLicense = text "Non-distributable"
    pretty MIT = text "MIT"
    pretty (UnknownLicense _) = text "Unknown"

deriving instance Data NameAddr
deriving instance Typeable NameAddr
deriving instance Read NameAddr

instance Pretty NameAddr where
    pretty x = text (maybe (nameAddr_addr x) (\ n -> n ++ " <" ++ nameAddr_addr x ++ ">") (nameAddr_name x))

deriving instance Show (Field' String)

instance Pretty VersionRange where
    pretty range =
        foldVersionRange'
          (text "*")
          (\ v -> text "=" <> pretty v)
          (\ v -> text ">" <> pretty v)
          (\ v -> text "<" <> pretty v)
          (\ v -> text ">=" <> pretty v)
          (\ v -> text "<=" <> pretty v)
          (\ x _ -> text "=" <> pretty x <> text ".*") -- not exactly right
          (\ x y -> text "(" <> x <> text " || " <> y <> text ")")
          (\ x y -> text "(" <> x <> text " && " <> y <> text ")")
          (\ x -> text "(" <> x <> text ")")
          range

instance Pretty Version where
    pretty = text . showVersion

instance Pretty DebianVersion where
    pretty = text . show
