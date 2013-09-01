-- | Code pulled out of cabal-debian that straightforwardly implements
-- parts of the Debian policy manual, or other bits of Linux standards.
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Debian.Policy
    ( -- * Paths
      databaseDirectory
    , apacheLogDirectory
    , apacheErrorLog
    , apacheAccessLog
    , serverLogDirectory
    , serverAppLog
    , serverAccessLog
    , errorLogBaseName
    , appLogBaseName
    , accessLogBaseName
    -- * Installed packages
    , debianPackageVersion
    , getDebhelperCompatLevel
    , StandardsVersion(..)
    , getDebianStandardsVersion
    , parseStandardsVersion
    -- * Package fields
    , SourceFormat(..)
    , readSourceFormat
    , PackagePriority(..)
    , readPriority
    , PackageArchitectures(..)
    , parsePackageArchitectures
    , Section(..)
    , readSection
    , Area(..)
    , parseUploaders
    , parseMaintainer
    , getDebianMaintainer
    , haskellMaintainer
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Arrow (second)
import Control.Monad (mplus)
import Data.Char (toLower, isSpace)
import Data.List (groupBy, intercalate)
import Data.Generics (Data, Typeable)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, strip)
import Debian.Debianize.Utility (read')
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion, parseDebianVersion, version)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (readProcess)
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..), address)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text)

databaseDirectory :: BinPkgName -> String
databaseDirectory x = "/srv" </> show (pretty x)

apacheLogDirectory :: BinPkgName -> String
apacheLogDirectory x =  "/var/log/apache2/" ++ show (pretty x)

apacheErrorLog :: BinPkgName -> String
apacheErrorLog x = apacheLogDirectory x </> errorLogBaseName

apacheAccessLog :: BinPkgName -> String
apacheAccessLog x = apacheLogDirectory x </> accessLogBaseName

serverLogDirectory :: BinPkgName -> String
serverLogDirectory x = "/var/log/" ++ show (pretty x)

serverAppLog :: BinPkgName -> String
serverAppLog x = serverLogDirectory x </> appLogBaseName

serverAccessLog :: BinPkgName -> String
serverAccessLog x = serverLogDirectory x </> accessLogBaseName

appLogBaseName :: String
appLogBaseName = "app.log"

errorLogBaseName :: String
errorLogBaseName = "error.log"

accessLogBaseName :: String
accessLogBaseName = "access.log"

debianPackageVersion :: String -> IO (Maybe DebianVersion)
debianPackageVersion name =
    readProcess "dpkg-query" ["--show", "--showformat=${version}", name] "" >>=
    return . parseDebianVersion'
    where
      -- This should maybe be the real parseDebianVersion
      parseDebianVersion' "" = Nothing
      parseDebianVersion' s = Just (parseDebianVersion s)

-- | Assumes debhelper is installed
getDebhelperCompatLevel :: IO (Maybe Int)
getDebhelperCompatLevel =
    debianPackageVersion "debhelper" >>= return . fmap (read . takeWhile (/= '.') . version)

data StandardsVersion = StandardsVersion Int Int Int (Maybe Int) deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty StandardsVersion where
    pretty (StandardsVersion a b c (Just d)) = text $ show a <> "." <> show b <> "." <> show c <> "." <> show d
    pretty (StandardsVersion a b c Nothing) = text $ show a <> "." <> show b <> "." <> show c

-- | Assumes debian-policy is installed
getDebianStandardsVersion :: IO (Maybe StandardsVersion)
getDebianStandardsVersion = debianPackageVersion "debian-policy" >>= return . fmap (parseStandardsVersion . version)

parseStandardsVersion :: String -> StandardsVersion
parseStandardsVersion s =
    case filter (/= ".") (groupBy (\ a b -> (a == '.') == (b == '.')) s) of
      (a : b : c : d : _) -> StandardsVersion (read' (error . ("StandardsVersion" ++) . show) a)
                                              (read' (error . ("StandardsVersion" ++) . show) b)
                                              (read' (error . ("StandardsVersion" ++) . show) c)
                                              (Just (read' (error . ("StandardsVersion" ++) . show) d))
      (a : b : c : _) -> StandardsVersion (read' (error . ("StandardsVersion" ++) . show) a)
                                          (read' (error . ("StandardsVersion" ++) . show) b)
                                          (read' (error . ("StandardsVersion" ++) . show) c) Nothing
      _ -> error $ "Invalid Standards-Version string: " ++ show s

data SourceFormat
    = Native3
    | Quilt3
    deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty SourceFormat where
    pretty Quilt3 = text "3.0 (quilt)\n"
    pretty Native3 = text "3.0 (native)\n"

readSourceFormat :: Text -> Either Text SourceFormat
readSourceFormat s =
    case () of
      _ | strip s == "3.0 (native)" -> Right Native3
      _ | strip s == "3.0 (quilt)" -> Right Quilt3
      _ -> Left $ "Invalid debian/source/format: " <> pack (show (strip s))

data PackagePriority
    = Required
    | Important
    | Standard
    | Optional
    | Extra
    deriving (Eq, Ord, Read, Show, Data, Typeable)

readPriority :: String -> PackagePriority
readPriority s =
    case unpack (strip (pack s)) of
      "required" -> Required
      "important" -> Important
      "standard" -> Standard
      "optional" -> Optional
      "extra" -> Extra
      x -> error $ "Invalid priority string: " ++ show x

instance Pretty PackagePriority where
    pretty = text . map toLower . show

-- | The architectures for which a binary deb can be built.
data PackageArchitectures
    = All            -- ^ The package is architecture independenct
    | Any            -- ^ The package can be built for any architecture
    | Names [String] -- ^ The list of suitable architectures
    deriving (Read, Eq, Ord, Show, Data, Typeable)

instance Pretty PackageArchitectures where
    pretty All = text "all"
    pretty Any = text "any"
    pretty (Names xs) = text $ intercalate " " xs

parsePackageArchitectures :: String -> PackageArchitectures
parsePackageArchitectures "all" = All
parsePackageArchitectures "any" = Any
parsePackageArchitectures s = error $ "FIXME: parsePackageArchitectures " ++ show s

data Section
    = MainSection String -- Equivalent to AreaSection Main s?
    | AreaSection Area String
    deriving (Read, Eq, Ord, Show, Data, Typeable)

readSection :: String -> Section
readSection s =
    case break (== '/') s of
      ("contrib", '/' : b) -> AreaSection Contrib (tail b)
      ("non-free", '/' : b) -> AreaSection NonFree (tail b)
      ("main", '/' : b) -> AreaSection Main (tail b)
      (a, '/' : _) -> error $ "readSection - unknown area: " ++ show a
      (a, _) -> MainSection a

instance Pretty Section where
    pretty (MainSection sec) = text sec
    pretty (AreaSection area sec) = pretty area <> text ("/" <> sec)

-- Is this really all that is allowed here?  Doesn't Ubuntu have different areas?
data Area
    = Main
    | Contrib
    | NonFree
    deriving (Read, Eq, Ord, Show, Data, Typeable)

instance Pretty Area where
    pretty Main = text "main"
    pretty Contrib = text "contrib"
    pretty NonFree = text "non-free"

{-
Create a debian maintainer field from the environment variables:

  DEBFULLNAME (preferred) or NAME
  DEBEMAIL (preferred) or EMAIL

More work could be done to match dch, but this is sufficient for
now. Here is what the man page for dch has to say:

 If the environment variable DEBFULLNAME is set, this will be used for
 the maintainer full name; if not, then NAME will be checked.  If the
 environment variable DEBEMAIL is set, this will be used for the email
 address.  If this variable has the form "name <email>", then the
 maintainer name will also be taken from here if neither DEBFULLNAME
 nor NAME is set.  If this variable is not set, the same test is
 performed on the environment variable EMAIL.  Next, if the full name
 has still not been determined, then use getpwuid(3) to determine the
 name from the pass‐word file.  If this fails, use the previous
 changelog entry.  For the email address, if it has not been set from
 DEBEMAIL or EMAIL, then look in /etc/mailname, then attempt to build
 it from the username and FQDN, otherwise use the email address in the
 previous changelog entry.  In other words, it’s a good idea to set
 DEBEMAIL and DEBFULLNAME when using this script.

-}
getDebianMaintainer :: IO (Maybe NameAddr)
getDebianMaintainer =
    do env <- map (second decodeString) `fmap` getEnvironment
       return $ do fullname <- lookup "DEBFULLNAME" env `mplus` lookup "NAME" env
                   email    <- lookup "DEBEMAIL" env `mplus` lookup "EMAIL" env
                   either (const Nothing) Just (parseMaintainer (fullname ++ " <" ++ email ++ ">"))

haskellMaintainer :: NameAddr
haskellMaintainer =
    NameAddr { nameAddr_name = Just "Debian Haskell Group"
             , nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"}

parseUploaders :: String -> Either String [NameAddr]
parseUploaders x =
    either (Left . show) fixNameAddrs (parse address "" ("Names: " ++ map fixWhite x ++ ";"))
    -- either (\ e -> error ("Failure parsing uploader list: " ++ show x ++ " -> " ++ show e)) id $ 
    where
      fixWhite c = if isSpace c then ' ' else c
      -- We absoletely need a name.
      fixNameAddrs :: [NameAddr] -> Either String [NameAddr]
      fixNameAddrs xs = case mapMaybe fixNameAddr xs of
                          [] -> Left ("No valid debian maintainers in " ++ show x)
                          xs' -> Right xs'
      fixNameAddr :: NameAddr -> Maybe NameAddr
      fixNameAddr y =
          case nameAddr_name y of
            Nothing -> Nothing
            _ -> Just y

parseMaintainer :: String -> Either String NameAddr
parseMaintainer x =
    case parseUploaders x of
      Left s -> Left s
      Right [y] -> Right y
      Right [] -> Left $ "Missing maintainer: " ++ show x
      Right ys -> Left $ "Too many maintainers: " ++ show ys
