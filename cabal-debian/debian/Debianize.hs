{-# LANGUAGE OverloadedStrings #-}
import Debian.Debianize
import Data.Lens.Lazy
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), VersionReq(SLT))
import Debian.Version (parseDebianVersion)
import Data.Map as Map (insertWith)
import Data.Maybe (fromMaybe)
import Data.Set as Set (insert, union, singleton)
import Data.Text as Text (intercalate)

main :: IO ()
main =
    do log <- inputChangeLog "debian"
       old <- inputCabalization "." defaultAtoms
       new <- cabalToDebianization "."
               (modL control (\ y -> y {homepage = Just "http://src.seereason.com/cabal-debian"}) $
                setL changelog (Just log) $
                setL compat (Just 7) $
                setL standards (Just (StandardsVersion 3 9 3 Nothing)) $
                setL sourceFormat (Just Native3) $
                setL utilsPackageName (Just (BinPkgName "cabal-debian")) $
                modL depends (Map.insertWith union (BinPkgName "cabal-debian") (singleton (Rel (BinPkgName "apt-file") Nothing Nothing))) $
                modL conflicts (Map.insertWith union (BinPkgName "cabal-debian")
                                      (singleton (Rel (BinPkgName "haskell-debian-utils") (Just (SLT (parseDebianVersion ("3.59" :: String)))) Nothing))) $
                modL description (Map.insertWith (error "test7") (BinPkgName "cabal-debian")
                                        (Text.intercalate "\n"
                                         [ "Create a debianization for a cabal package"
                                         , " Tool for creating debianizations of Haskell packages based on the .cabal"
                                         , " file.  If apt-file is installed it will use it to discover what is the"
                                         , " debian package name of a C library."
                                         , " ."
                                         , "  Author: David Fox <dsf@seereason.com>"
                                         , "  Upstream-Maintainer: David Fox <dsf@seereason.com>" ])) $
                defaultAtoms)
       inputDebianization "." defaultAtoms >>= \ old -> putStr (compareDebianization old (copyFirstLogEntry old new))
       -- writeDebianization "." new

-- | This copies the first log entry of deb1 into deb2.  Because the
-- debianization process updates that log entry, we need to undo that
-- update in order to get a clean comparison.
copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)
