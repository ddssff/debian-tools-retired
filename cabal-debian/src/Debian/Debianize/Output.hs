-- | Generate a package Debianization from Cabal data and command line
-- options.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( outputDebianization
    , describeDebianization
    , writeDebianization
    ) where

import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL)
import Data.Map as Map (toList, elems)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, unpack, split)
import Debian.Changes (ChangeLog(ChangeLog), ChangeLogEntry(logVersion))
import Debian.Debianize.Atoms (HasAtoms(changelog, control), Atoms, flags)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(source, binaryPackages), BinaryDebDescription(package))
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Types (Flags(validate, dryRun))
import Debian.Debianize.Utility (replaceFile, zipMaps)
import System.Directory (Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.PrettyPrint.ANSI.Leijen (pretty)

outputDebianization :: Maybe Atoms -> Atoms -> IO ()
outputDebianization old new =
       -- It is imperitive that during the time that dpkg-buildpackage
       -- runs the version number in the changelog and the source and
       -- package names in the control file do not change, or the
       -- build will fail.  To ensure this set the validate flag.
       -- This means you probably need to run debianize before
       -- starting dpkg-buildpackage.  However, it is still good to be
       -- able to put the debianize parameters in the Setup file,
       -- rather than storing them apart from the package in the
       -- autobuilder configuration.
       case old of
         Just old' | validate (getL flags new) -> validateDebianization old' new
         _ | validate (getL flags new) -> error "No existing debianization to validate"
         Just old' | dryRun (getL flags new) -> putStr ("Debianization (dry run):\n" ++ describeDebianization old' new)
         _ | dryRun (getL flags new) -> error "No existing debianiztion to compare new one to"
         _ -> writeDebianization new

validateDebianization :: Atoms -> Atoms -> IO ()
validateDebianization old new =
    do let oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog old))))
           newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog new))))
           oldSource = source . getL control $ old
           newSource = source . getL control $ new
           oldPackages = map Debian.package . binaryPackages . getL control $ old
           newPackages = map Debian.package . binaryPackages . getL control $ new
       case () of
         _ | oldVersion /= newVersion -> error ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion))
           | oldSource /= newSource -> error ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource))
           | oldPackages /= newPackages -> error ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages))
           | True -> return ()
    where
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x

-- | Describe a 'Debianization' in relation to one that is written into
describeDebianization :: Atoms -> Atoms -> String
describeDebianization old new =
    concat . Map.elems $ zipMaps doFile oldFiles newFiles
    where
      oldFiles = toFileMap old
      newFiles = toFileMap new
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

writeDebianization :: Atoms -> IO ()
writeDebianization d =
    mapM_ (uncurry doFile) (toList (toFileMap d)) >>
    getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
    where
      doFile path text =
          createDirectoryIfMissing True (takeDirectory path) >>
          replaceFile path (unpack text)

indent :: [Char] -> String -> String
indent prefix text = unlines (map (prefix ++) (lines text))
