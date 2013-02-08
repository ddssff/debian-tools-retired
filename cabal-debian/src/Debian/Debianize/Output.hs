-- | Generate a package Debianization from Cabal data and command line
-- options.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( validateDebianization
    , describeDebianization
    , writeDebianization
    ) where

import Control.Applicative ((<$>))
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL)
import Data.Map as Map (toList, elems)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, unpack, split)
import Debian.Changes (ChangeLog(ChangeLog), ChangeLogEntry(logVersion))
import Debian.Debianize.Atoms (HasAtoms(changelog, control), Atoms)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(source, binaryPackages), BinaryDebDescription(package))
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Goodies (defaultAtoms)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Utility (withCurrentDirectory, replaceFile, zipMaps)
import System.Directory (Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.PrettyPrint.ANSI.Leijen (pretty)

{-
outputDebianization :: FilePath -> Atoms -> IO ()
outputDebianization top new =
    do log <- (Just <$> inputChangeLog "debian/changelog") `catchIOError` (\ _ -> return Nothing)
       old'' <- (Just <$> inputDebianization top defaultAtoms) `catch` (\ (_ :: ErrorCall) -> return Nothing)
       let old = Just (def log old'')

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
         Just old' | getL validate new -> validateDebianization old' new
         _ | getL validate new -> error "No existing debianization to validate"
         Just old' | getL dryRun new -> putStr ("Debianization (dry run):\n" ++ describeDebianization old' new)
         _ | getL dryRun new -> putStr (describeDebianization defaultAtoms new)
         _ -> writeDebianization top new
    where
      def log old = fromMaybe ((setL changelog log) defaultAtoms) old
-}

-- | Don't change anything, just make sure the existing debianization
-- matches the new debianization in several particulars -
-- specifically, version number, and source and binary package names.
validateDebianization :: FilePath -> Atoms -> IO ()
validateDebianization top new =
    do old <- inputDebianization top defaultAtoms
       let oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog old))))
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

describeDebianization :: FilePath -> Atoms -> IO String
describeDebianization top new =
    do oldFiles <- toFileMap <$> inputDebianization top defaultAtoms
       let newFiles = toFileMap new
       return $ concat . Map.elems $ zipMaps doFile oldFiles newFiles
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

writeDebianization :: FilePath -> Atoms -> IO ()
writeDebianization top d =
    withCurrentDirectory top $
      mapM_ (\ (path, text) ->
                 createDirectoryIfMissing True (takeDirectory path) >>
                 replaceFile path (unpack text))
            (toList (toFileMap d)) >>
      getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})

indent :: [Char] -> String -> String
indent prefix text = unlines (map (prefix ++) (lines text))
